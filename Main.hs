{-# LANGUAGE OverloadedStrings #-}

import Data.Functor
import Text.HTML.Scalpel
import Text.HTML.TagSoup

-- TODO: ByteString

getURL :: [String] -> URL
getURL [book, chapter, version] = "https://www.biblegateway.com/passage/?" ++
    "search=" ++ book ++ "+" ++ chapter ++ "&version=" ++ version

scraper :: Scraper String [String]
scraper = chroot ("div" @: [hasClass "result-text-style-normal", hasClass "text-html"]) $ do
    chroots ("p" // "span" @: [hasClass "text"]) $ do
        innerHTML anySelector

parseHTML :: Maybe [String] -> [Tag String]
parseHTML = maybe [] (\s -> parseTags $ foldl1 (++) s)

getTags :: URL -> IO [Tag String]
getTags url = scrapeURL url scraper <&> parseHTML
