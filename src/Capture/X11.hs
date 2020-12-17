{-# LANGUAGE OverloadedStrings #-}
module Capture.X11 where

import Data
import Graphics.X11
import Graphics.X11.Xlib.Extras
import Control.Monad
import Control.Exception (catch, bracket)
import System.IO.Error (catchIOError)
import Control.Applicative
import Data.Either
import Data.Maybe
import Data.String
import Data.Time.Clock
import System.Environment
import System.IO
import qualified Data.MyText as T

import System.Locale.SetLocale
import Graphics.X11.XScreenSaver
import qualified DBus as D
import qualified DBus.Client as D

setupCapture :: IO ()
setupCapture = do
        unless compiledWithXScreenSaver $
                hPutStrLn stderr "arbtt [Warning]: X11 was compiled without support for XScreenSaver"
        loc <- supportsLocale
        unless loc $ hPutStrLn stderr "arbtt [Warning]: locale unsupported"
        dpy <- openDisplay ""
        xSetErrorHandler
        let rwin = defaultRootWindow dpy
        a <- internAtom dpy "_NET_CLIENT_LIST" False
        p <- getWindowProperty32 dpy a rwin
        when (isNothing p) $ do
                hPutStrLn stderr "arbtt: ERROR: No _NET_CLIENT_LIST set for the root window"
        closeDisplay dpy

captureData :: IO CaptureData
captureData = do
        dpy <- openDisplay ""
        xSetErrorHandler
        let rwin = defaultRootWindow dpy

        -- Desktops
        current_desktop <- flip catchIOError (\_ -> return "") $ do
            a <- internAtom dpy "_NET_CURRENT_DESKTOP" False
            p <- getWindowProperty32 dpy a rwin
            let desk_index = do {[d] <- p; return (fromIntegral d)}

            a <- internAtom dpy "_NET_DESKTOP_NAMES" False
            tp <- getTextProperty dpy rwin a
            names <- wcTextPropertyToTextList dpy tp

            return $ case desk_index of
                  Nothing -> ""
                  Just n -> if 0 <= n && n < length names
                            then names !! n
                            else show n
        -- Windows
        a <- internAtom dpy "_NET_CLIENT_LIST" False
        p <- getWindowProperty32 dpy a rwin

        wins <- case p of
                Just wins -> filterM (isInteresting dpy) (map fromIntegral wins)
                Nothing   -> return []

        (fsubwin,_) <- getInputFocus dpy
        fwin <- followTreeUntil dpy (`elem` wins) fsubwin

        winData <- forM wins $ \w -> (,,)
            (w == fwin) <$>
            (T.pack <$> getWindowTitle dpy w) <*>
            (T.pack <$> getProgramName dpy w)

        it <- fromIntegral `fmap` getXIdleTime dpy
        ss <- isScreenSaverActive dpy
        sl <- isSessionLocked

        closeDisplay dpy
        return $ CaptureData winData it (T.pack current_desktop) (ss || sl)

getWindowTitle :: Display -> Window -> IO String
getWindowTitle dpy =  myFetchName dpy

getProgramName :: Display -> Window -> IO String
getProgramName dpy = fmap resName . getClassHint dpy

-- Returns a parent of a window or zero.
getParent :: Display -> Window -> IO Window
getParent dpy w = do
  (_, parent, _) <- queryTree dpy w `catchIOError` (const $ return (0,0,[]))
  return parent

-- | Follows the tree of windows up until the condition is met or the root
-- window is reached.
followTreeUntil :: Display -> (Window -> Bool) -> Window -> IO Window 
followTreeUntil dpy cond = go
  where go w | cond w    = return w
             | otherwise = do p <- getParent dpy w
                              if p == 0 then return w
                                        else go p 

-- | Ignore, for example, Desktop and Docks windows
isInteresting :: Display -> Window -> IO Bool
isInteresting d w = do
    a <- internAtom d "_NET_WM_WINDOW_TYPE" False
    dock <- internAtom d "_NET_WM_WINDOW_TYPE_DOCK" False
    desk <- internAtom d "_NET_WM_WINDOW_TYPE_DESKTOP" False
    mbr <- getWindowProperty32 d a w
    case mbr of
        Just [r] -> return $ fromIntegral r `notElem` [dock, desk]
        _        -> return True

-- | better than fetchName from X11, as it supports _NET_WM_NAME and unicode
--
-- Code taken from XMonad.Managehook.title
myFetchName :: Display -> Window -> IO String
myFetchName d w = do
        let getProp =
                (internAtom d "_NET_WM_NAME" False >>= getTextProperty d w)
                `catchIOError`
                (\_ -> getTextProperty d w wM_NAME)

            extract prop = do l <- wcTextPropertyToTextList d prop
                              return $ if null l then "" else head l

        bracket getProp (xFree . tp_value) extract
            `catchIOError` \_ -> return ""

-- | Check active screen saver using the X11 Screen Saver extension.
--
-- This most likely only works with the simple built-in screen saver
-- configured using @xset s@. Screen savers/lockers such as xscreensaver,
-- xsecurelock, i3lock, etc. work differently.
isScreenSaverActive :: Display -> IO Bool
isScreenSaverActive dpy = do
    info <- xScreenSaverQueryInfo dpy
    return $ case info of
        Just XScreenSaverInfo{xssi_state = ScreenSaverOn} -> True
        _ -> False

-- TODO: https://unix.stackexchange.com/questions/197032/detect-if-screensaver-is-active

-- | Check whether the current systemd-logind session is marked as locked.
--
-- Note that many minimalist screen savers/lockers do not communicate with
-- systemd-logind, so this often doesn't work either.
--
-- TODO: describe this better
-- dbus-send --system --print-reply --dest=org.freedesktop.login1 /org/freedesktop/login1/session/self "org.freedesktop.login1.Session.SetLockedHint" boolean:false
isSessionLocked :: IO Bool
isSessionLocked = do
    xdgSessionId <- lookupEnv "XDG_SESSION_ID"
    -- When running as systemd user unit, …/session/self doesn't work so we
    -- try $XDG_SESSION_ID and fall back to …/session/auto if not set, which
    -- acts like self if run directly from a session, or the user's display
    -- session otherwise.
    let session = fromMaybe "auto" xdgSessionId
    bracket D.connectSystem D.disconnect (getLockedHint session)
        `catch` (return . const False . D.clientErrorMessage)
  where
    dest = "org.freedesktop.login1"
    object session = fromString $ "/org/freedesktop/login1/session/" <> session
    interface = "org.freedesktop.login1.Session"
    property = "LockedHint"
    methodCall obj = (D.methodCall obj interface property){ D.methodCallDestination = Just dest }
    getLockedHint session c = fmap (fromRight False) $ D.getPropertyValue c $ methodCall $ object session
