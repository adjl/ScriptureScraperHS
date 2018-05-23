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

chapterScraper :: FilePath -> URL -> IO Bool
chapterScraper fileName url = do
    results <- scrapeURL url scraper
    let contents :: [String]
        contents = fromMaybe [] results
    if null contents then return False
    else do
        writeToFile fileName contents
        return True

bibleScraper :: IO ()
bibleScraper = do
    args <- getArgs
    let fileName :: FilePath
        fileName = foldl1 (++) args ++ "HTML.txt"
    success <- chapterScraper fileName $ getURL args
    return ()

main :: IO ()
main = bibleScraper
