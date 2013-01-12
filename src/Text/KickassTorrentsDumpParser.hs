module Text.KickassTorrentsDumpParser (InfoHash(..),
                                       URL(..),
                                       ReleaseCategory(..),
                                       ReleaseTitle(..),
                                       Release(..),
                                       parseDump) where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad (mzero)
import qualified Data.ByteString.Lazy as LBS
import Data.Csv
import Data.Char (ord)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Vector as V

newtype InfoHash        = InfoHash { unInfoHash :: Text } deriving (Show, Eq)
-- TODO: use most useful URL type instead of rolling your own
newtype URL             = URL { unUrl :: Text } deriving (Show, Eq)
newtype ReleaseCategory = ReleaseCategory { unReleaseCategory :: Text } deriving (Show, Eq)
newtype ReleaseTitle    = ReleaseTitle { unReleaseTitle :: Text } deriving (Show, Eq)

data Release = Release {
  infoHash   :: InfoHash,
  title      :: ReleaseTitle,
  category   :: ReleaseCategory,
  katUrl     :: URL,
  torrentUrl :: URL
} deriving (Show, Eq)

instance FromRecord Release where
  parseRecord v
    | V.length v == 5 = Release <$> v .! 0
                                <*> v .! 1
                                <*> v .! 2
                                <*> v .! 3
                                <*> v .! 4
    | otherwise = mzero

instance FromField InfoHash where
  parseField s = InfoHash <$> pure (decodeUtf8 s)

instance FromField URL where
  parseField s = URL <$> pure (decodeUtf8 s)

instance FromField ReleaseCategory where
  parseField s = ReleaseCategory <$> pure (decodeUtf8 s)

instance FromField ReleaseTitle where
  parseField s = ReleaseTitle <$> pure (decodeUtf8 s)

parseDump :: LBS.ByteString -> Either String (V.Vector Release)
parseDump = decodeWith options False
  where options = DecodeOptions $ c2w8 '|'
        c2w8 = fromIntegral . ord
