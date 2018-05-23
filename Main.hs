{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe
import System.Environment
import Text.HTML.Scalpel

getURL :: [String] -> URL
getURL [book, chapter, version] = "https://www.biblegateway.com/passage/?search=" ++ book ++ "+" ++ chapter ++ "&version=" ++ version

scraper :: Scraper String [[String]]
scraper = chroot ("div" @: [hasClass "result-text-style-normal", hasClass "text-html"]) $ do
    chroots "p" $ do
        chroots ("span" @: [hasClass "text"]) $ do
            verses <- text anySelector
            return verses

bibleScraper :: [String] -> IO ()
bibleScraper args = do
    results <- scrapeURL (getURL args) scraper
    let contents :: [String]
        contents = concat $ fromMaybe [] $ results
    if null contents then
        putStrLn "Could not retrieve contents."
    else do
        let fileName :: FilePath
            fileName = foldl1 (++) args ++ ".txt"
        putStrLn $ "Writing contents to file " ++ fileName ++ " ..."
        mapM_ (appendFile fileName) contents

main :: IO ()
main = getArgs >>= bibleScraper
