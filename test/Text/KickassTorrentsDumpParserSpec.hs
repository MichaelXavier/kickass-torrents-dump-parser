{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.KickassTorrentsDumpParserSpec (spec) where

import Test.Hspec
import Data.String.QQ (s)
import qualified Data.Vector as V

import Text.KickassTorrentsDumpParser

spec :: Spec
spec = describe "parseDump" $ do
         it "parses a file" $
           parseDump sampleFile `shouldBe` Right sampleDump
         it "doesn't parse an invalid file" $
           parseDump "whatever" `shouldBe` Left "conversion error: mzero"
  where sampleFile = [s|AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA|Cool Software|Applications|http://kat.ph/cool-software.html|http://torcache.net/torrent/AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA.torrent
BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB|Cool Stories|Books|http://kat.ph/cool-books.html|http://torcache.net/torrent/BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB.torrent|]
        sampleDump = V.fromList [release1, release2]
        release1 = Release (InfoHash "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
                           (ReleaseTitle "Cool Software")
                           (ReleaseCategory "Applications")
                           (URL "http://kat.ph/cool-software.html")
                           (URL "http://torcache.net/torrent/AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA.torrent")
        release2 = Release (InfoHash "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB")
                           (ReleaseTitle "Cool Stories")
                           (ReleaseCategory "Books")
                           (URL "http://kat.ph/cool-books.html")
                           (URL "http://torcache.net/torrent/BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB.torrent")

