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

tagsToStrip :: [String]
tagsToStrip = ["span", "sup"]

oblique :: String
oblique = "oblique"

isOblique :: Tag String -> Bool
isOblique tag = fromAttrib "class" tag == oblique

getTags :: URL -> IO [Tag String]
getTags url = parseHTML <$> scrapeURL url scraper

parseHTML :: Maybe [String] -> [Tag String]
parseHTML = maybe [] (\html -> parseTags $ foldl1 (++) html)

filterTags :: [Tag String] -> [Tag String]
filterTags = filterTags_ (False, "")

filterTags_ :: (Bool, String) -> [Tag String] -> [Tag String]
filterTags_ _ [] = []
filterTags_ (False, _) (tag@(TagOpen name _) : tags)
    | isOblique tag         =                     filterTags_ (False, oblique) tags
    | name == "i"           = TagText "\\emph{" : filterTags_ (False, "i")     tags
    | elem name tagsToStrip =                     filterTags_ (True,  name)    tags
filterTags_ (False, word) (TagClose _ : tags)
    | word == oblique =               filterTags_ (False, "") tags
    | word == "i"     = TagText "}" : filterTags_ (False, "") tags
filterTags_ (True, word) (TagClose name : tags)
    | word == name = filterTags_ (False, "") tags
filterTags_ state@(stripMode, _) (tag:tags)
    | stripMode =       filterTags_ state tags
    | otherwise = tag : filterTags_ state tags
