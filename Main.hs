{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import Text.HTML.Scalpel
import Text.HTML.TagSoup
import Text.Regex

-- TODO: ByteString

getURL :: [String] -> URL
getURL [book, chapter, version] = "https://www.biblegateway.com/passage/?" ++
    "search=" ++ book ++ "+" ++ chapter ++ "&version=" ++ version

scraper :: Scraper String [String]
scraper = chroot ("div" @: [hasClass "result-text-style-normal", hasClass "text-html"]) $ do
    chroots ("p" // "span" @: [hasClass "text"]) $ do
        innerHTML anySelector

getTags :: URL -> IO [Tag String]
getTags url = parseHTML <$> scrapeURL url scraper

parseHTML :: Maybe [String] -> [Tag String]
parseHTML = maybe [] (\html -> parseTags $ foldl1 (++) html)

filterTags :: [Tag String] -> [Tag String]
filterTags = filterTags_ (False, "")

filterTags_ :: (Bool, String) -> [Tag String] -> [Tag String]
filterTags_ _ [] = []
filterTags_ (False, _) (tag@(TagOpen name _):tags)
    | name == "i"      = openEmphTag : filteredTags (False, "i")
    | isDivineName tag = openNameTag : filteredTags (False, "d")
    | isOblique tag    =               filteredTags (False, "o")
    | stripElem name   =               filteredTags (True,  name)
    where
        isDivineName :: Tag String -> Bool
        isDivineName tag = fromAttrib "class" tag == "small-caps divine-name"

        isOblique :: Tag String -> Bool
        isOblique tag = fromAttrib "class" tag == "oblique"

        stripElem :: String -> Bool
        stripElem name = elem name ["span", "sup"]

        filteredTags :: (Bool, String) -> [Tag String]
        filteredTags state = filterTags_ state tags

        openEmphTag :: Tag String
        openEmphTag = TagText "\\emph{"

        openNameTag :: Tag String
        openNameTag = TagText "\\textsc{"

filterTags_ (False, word) (TagClose _ : tags)
    | word == "i" = closingTag : filteredTags
    | word == "d" = closingTag : filteredTags
    | word == "o" =              filteredTags
    where
        filteredTags :: [Tag String]
        filteredTags = filterTags_ (False, "") tags

        closingTag :: Tag String
        closingTag = TagText "}"

filterTags_ (True, word) (TagClose name : tags)
    | word == name = filterTags_ (False, "") tags

filterTags_ state@(stripMode, _) (tag:tags)
    | stripMode =       filteredTags
    | otherwise = tag : filteredTags
    where
        filteredTags :: [Tag String]
        filteredTags = filterTags_ state tags

concatText :: [Tag String] -> String
concatText tags = foldl1 (++) $ map fromTagText tags

cleanText :: String -> String
cleanText = cleanText_ regexes
    where
        regexes :: [(Regex, String)]
        regexes = [
            (mkRegex "\\\8212",     "---"),
            (mkRegex "\\\8220",     "``"),
            (mkRegex "\\\8221",     "''"),
            (mkRegex "(\\w)(``)",   "\\1 \\2"),
            (mkRegex spacing,       "\\1 \\2"),
            (mkRegex "\\s*---\\s*", "---"),
            (mkRegex "``\\s*",      "``"),
            (mkRegex "\\s*''",      "''"),
            (mkRegex "\\s+",        " ")]
        spacing :: String
        spacing = "(''|[!,.:;?])([\\w`])"

cleanText_ :: [(Regex, String)] -> String -> String
cleanText_ [] text = text
cleanText_ ((pattern, replacement):regexes) text = cleanText_ regexes replacedText
    where
        replacedText :: String
        replacedText = subRegex pattern text replacement

processTagText :: [Tag String] -> String
processTagText = cleanText . concatText . filterTags

extractChapter :: [String] -> IO String
extractChapter chapter = do
    (getTags $ getURL chapter) >>= return . processTagText

extractBook :: [String] -> [IO String]
extractBook [book, version, chapters] = [extractChapter $ getCitation chapter | chapter <- [1..numChapters]]
    where
        numChapters :: Int
        numChapters = read chapters :: Int

        getCitation :: Int -> [String]
        getCitation chapter = [book, show chapter, version]

bibleScraper :: IO ()
bibleScraper = do
    citation@[book, version, _] <- getArgs
    let
        bookFilename :: String
        bookFilename = book ++ version ++ ".txt"

        writeChapter :: IO String -> IO ()
        writeChapter chapter = chapter >>= appendFile bookFilename

    writeFile bookFilename ""
    mapM_ writeChapter $ extractBook citation

main :: IO ()
main = bibleScraper
