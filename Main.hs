{-# LANGUAGE OverloadedStrings #-}

import Data.List
import Data.Maybe
import System.Environment
import Text.HTML.Scalpel

getURL :: [String] -> URL
getURL [book, chapter, version] =
    "https://www.biblegateway.com/passage/?" ++
    "search=" ++ book ++ "+" ++ chapter ++
    "&version=" ++ version

chapterSelector :: Selector
chapterSelector = "div" @:
    [hasClass "result-text-style-normal", hasClass "text-html"]

scraper :: Scraper String [String]
scraper = chroot chapterSelector $ do
    chroots ("p" // "span" @: [hasClass "text"]) $ do
        innerHTML anySelector >>= return

writeToFile :: FilePath -> [String] -> IO ()
writeToFile fileName contents = do
    putStrLn $ "Writing contents to file " ++ fileName ++ " ..."
    mapM_ (appendFile fileName) $ intersperse "\n" contents

chapterScraper :: [String] -> IO ()
chapterScraper args = do
    results <- scrapeURL (getURL args) scraper
    let contents :: [String]
        contents = fromMaybe [] results
    if null contents then putStrLn "Could not retrieve contents."
    else do
        let fileName :: FilePath
            fileName = foldl1 (++) args ++ "HTML.txt"
        writeToFile fileName contents

main :: IO ()
main = do
    getArgs >>= chapterScraper
