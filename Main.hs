{-# LANGUAGE OverloadedStrings #-}

import Data.List
import Data.Maybe
import System.Environment
import Text.HTML.Scalpel
import Text.Regex

stripRegexes :: [Regex]
stripRegexes = [
    mkRegex "<span\\s+class=\"chapternum\">[0-9]+\160*<\\/span>",
    mkRegex "<sup\\s+class=\"versenum\">[0-9]+\160*<\\/sup>",
    mkRegex "<sup\\s+class=\"crossreference\".+><\\/sup>",
    mkRegex "<sup.+class=\"footnote\".+>.+<\\/sup>"
]

selector :: Selector
selector = "div" @:
    [hasClass "result-text-style-normal", hasClass "text-html"]

scraper :: Scraper String [String]
scraper = chroot selector $ do
    chroots ("p" // "span" @: [hasClass "text"]) $ do
        innerHTML anySelector >>= return

getURL :: [String] -> URL
getURL [book, chapter, version] =
    "https://www.biblegateway.com/passage/?" ++
    "search=" ++ book ++ "+" ++ chapter ++
    "&version=" ++ version

stripHTML :: [Regex] -> [String] -> [String]
stripHTML []              strippedHTML = strippedHTML
stripHTML (regex:regexes) rawHTML      = stripHTML regexes strippedHTML
    where strippedHTML :: [String]
          strippedHTML = map (\line -> subRegex regex line "") rawHTML

main :: IO ()
main = do
    citation <- getArgs
    results <- scrapeURL (getURL citation) scraper
    let rawHTML :: [String]
        rawHTML = fromMaybe [] results
    if null rawHTML then return ()
    else mapM_ (appendFile "Romans1NKJVStripped.txt") $ intersperse "\n" $ stripHTML stripRegexes rawHTML
