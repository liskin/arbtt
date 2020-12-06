{-# LANGUAGE OverloadedStrings, RecordWildCards, FlexibleInstances, CPP #-}
module DumpFormat
    ( DumpFormat(..)
    , readDumpFormat
    , dumpActivity
    , dumpSample
    , dumpSamples
    ) where

import Data.MyText (unpack, null, pack, Text)
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Time
#if MIN_VERSION_time(1,5,0)
import Data.Time.Format(defaultTimeLocale)
#else
import System.Locale (defaultTimeLocale)
#endif
import Data.Char
import Data.Foldable (toList)
import Control.Applicative ((<$>), (<|>), (<*>), pure)

import Data
import Text.Printf
import Data.List hiding (null)
import Prelude hiding (null)

data DumpFormat
    = DFShow
    | DFHuman
    | DFJSON
    deriving (Show, Eq)

instance ToJSON Text where
    toJSON = toJSON . unpack
instance FromJSON Text where
    parseJSON x = pack <$> parseJSON x

instance ToJSON (TimeLogEntry CaptureData) where
    toJSON (TimeLogEntry {..}) = object [
        "date" .= tlTime,
        "rate" .= tlRate,
        "inactive" .= cLastActivity tlData,
        "windows" .= map (\(a,t,p) -> object ["active" .= a, "title" .= t, "program" .= p]) (cWindows tlData),
        "desktop" .= cDesktop tlData,
        "screensaver" .= cScreenSaver tlData
        ]

instance FromJSON (TimeLogEntry CaptureData) where
    parseJSON = withObject "TimeLogEntry" $ \v -> do
        tlTime <- v .: "date"
        tlRate <- v .: "rate"
        cLastActivity <- v .: "inactive"
        cWindows  <- (v .: "windows") >>=
            withArray "windows" (mapM (withObject "window" $ \v ->
                (,,) <$> v .: "active" <*> v .: "title" <*> v .: "program"
            ) . toList)
        cDesktop <- v .: "desktop"
        cScreenSaver <- v .: "screensaver"
        let tlData = CaptureData {..}
        let entry = TimeLogEntry {..}
        pure entry

readDumpFormat :: String -> Maybe DumpFormat
readDumpFormat arg =
    case map toLower arg of
        "human"      -> return DFHuman
        "show"       -> return DFShow
        "json"       -> return DFJSON
        _            -> Nothing

dumpActivity :: TimeLog (CaptureData, TimeZone, ActivityData) -> IO ()
dumpActivity = mapM_ go
 where
    go tle = do
        dumpHeader tz (tlTime tle) (cLastActivity cd) (cScreenSaver cd)
        dumpDesktop (cDesktop cd)
        mapM_ dumpWindow (cWindows cd)
        dumpTags ad
      where
        (cd, tz, ad) = tlData tle

dumpTags :: ActivityData -> IO ()
dumpTags = mapM_ go
  where go act = printf "    %s\n" (show act)

dumpHeader :: TimeZone -> UTCTime -> Integer -> Bool -> IO ()
dumpHeader tz time lastActivity screenSaver = do
    printf "%s (%dms inactive%s):\n"
        (formatTime defaultTimeLocale "%F %X" (utcToLocalTime tz time))
        lastActivity
        (if screenSaver then ", screen saver/locker active" else [])

dumpWindow :: (Bool, Text, Text) -> IO ()
dumpWindow (active, title, program) = do
    printf "    %s %-15s %s\n"
        (if active then ("(*)"::String) else "( )")
        (unpack program ++ ":")
        (unpack title)

dumpDesktop :: Text -> IO ()
dumpDesktop d
    | null d    = return ()
    | otherwise = printf "    Current Desktop: %s\n" (unpack d)

dumpSample :: TimeZone -> TimeLogEntry CaptureData -> IO ()
dumpSample tz tle = do
    dumpHeader tz (tlTime tle) (cLastActivity (tlData tle)) (cScreenSaver (tlData tle))
    dumpDesktop (cDesktop (tlData tle))
    mapM_ dumpWindow (cWindows (tlData tle))

dumpSamples :: TimeZone -> DumpFormat -> TimeLog CaptureData -> IO ()
dumpSamples _ DFShow = mapM_ print

dumpSamples tz DFHuman = mapM_ (dumpSample tz)

dumpSamples _ DFJSON = enclose . sequence_ . intersperse (putStrLn ",") . map (LBS.putStr . encode)
  where
    enclose m = putStrLn "[" >> m >> putStrLn "]"
