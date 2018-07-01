{-# LANGUAGE OverloadedStrings #-}

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

getTags :: URL -> IO [Tag String]
getTags url = parseHTML <$> scrapeURL url scraper

parseHTML :: Maybe [String] -> [Tag String]
parseHTML = maybe [] (\html -> parseTags $ foldl1 (++) html)

filterTags :: [Tag String] -> [Tag String]
filterTags = filterTags_ (False, "")

filterTags_ :: (Bool, String) -> [Tag String] -> [Tag String]
filterTags_ _ [] = []
filterTags_ (False, _) (tag@(TagOpen name _):tags)
    | name == "i"    = openEmphTag : filteredTags (False, "i")
    | isOblique tag  =               filteredTags (False, "o")
    | stripElem name =               filteredTags (True,  name)
    where
        isOblique :: Tag String -> Bool
        isOblique tag = fromAttrib "class" tag == "oblique"

        stripElem :: String -> Bool
        stripElem name = elem name ["span", "sup"]

        filteredTags :: (Bool, String) -> [Tag String]
        filteredTags state = filterTags_ state tags

        openEmphTag :: Tag String
        openEmphTag = TagText "\\emph{"

filterTags_ (False, word) (TagClose _ : tags)
    | word == "i" = closeEmphTag : filteredTags
    | word == "o" =                filteredTags
    where
        filteredTags :: [Tag String]
        filteredTags = filterTags_ (False, "") tags

        closeEmphTag :: Tag String
        closeEmphTag = TagText "}"

filterTags_ (True, word) (TagClose name : tags)
    | word == name = filterTags_ (False, "") tags

filterTags_ state@(stripMode, _) (tag:tags)
    | stripMode =       filteredTags
    | otherwise = tag : filteredTags
    where
        filteredTags :: [Tag String]
        filteredTags = filterTags_ state tags
