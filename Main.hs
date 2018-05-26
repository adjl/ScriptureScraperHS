{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.Scalpel
import Text.Regex.PCRE ((=~))

data Replacement
    = Replace     { body :: String }
    | WrapReplace { head :: String, end :: String }
    deriving Show

type Substitution = (Regex, Replacement)

substitutions :: [Substition]
substitutions = [
    ("<span\\s+class=\"chapternum\">[0-9]+?\160*?<\\/span>", Replace ""),
    ("<sup\\s+class=\"versenum\">[0-9]+?\160*?<\\/sup>",     Replace ""),
    ("<sup\\s+class=\"crossreference\".+?><\\/sup>",         Replace ""),
    ("<sup.+?class=\"footnote\".+?>.+?<\\/sup>",             Replace ""),
    ("—",                                                    Replace "---"),
    ("<span\\s+?class=\"oblique\">“?(.+?)”?<\\/span>",       WrapReplace "``" "''"),
    ("<i>(.+?)<\\/i>",                                       WrapReplace "\\emph{" "}")]

subRegex :: String -> Substitution -> String
subRegex input sub
    | input == output = output
    | otherwise       = subRegex sub output
    where
        output :: String
        output = subRegex' sub input

subRegex' :: String -> Substitution -> String
subRegex' input (regex, replace)
    | null match  = input
    | null groups = before ++ body replace ++ after
    | otherwise   = before ++ head replace ++ head groups ++ end replace ++ after
    where
        matchResult :: (String, String, String, [String])
        (before, match, after, groups) = input =~ regex

subHTML :: [String] -> [String]
subHTML = map subHTML'
    where
        subHTML' :: String -> String
        subHTML' rawHTML = foldl1 (subRegex rawHTML) substitutions

scraper :: Scraper String [String]
scraper = chroot ("div" @: [hasClass "result-text-style-normal", hasClass "text-html"]) $ do
    chroots ("p" // "span" @: [hasClass "text"]) $ do
        innerHTML anySelector >>= return

getURL :: [String] -> URL
getURL [book, chapter, version] = "https://www.biblegateway.com/passage/?"
    ++ "search=" ++ book ++ "+" ++ chapter
    ++ "&version=" ++ version
