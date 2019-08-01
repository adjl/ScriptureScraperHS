module Sanitizer where

import Text.Regex

sanitize :: String -> String
sanitize text = subRegex (mkRegex "\128") text "``"

-- regexes :: [(Regex, String)]
-- regexes = [
--     (mkRegex "\\\8212",                     "---"),
--     (mkRegex "\\\8220",                     "``"),
--     (mkRegex "\\\8221",                     "''"),
--     (mkRegex "([[:graph:]])(``)",           "\\1 \\2"),
--     (mkRegex "(''|[!,.:;?])([[:alnum:]])",  "\\1 \\2"),
--     (mkRegex "([[:graph:]])$",              "\\1 "),
--     (mkRegex "[[:space:]]*---[[:space:]]*", "---"),
--     (mkRegex "``[[:space:]]*",              "``"),
--     (mkRegex "[[:space:]]*''",              "''"),
--     (mkRegex "[[:space:]]{2,}",             " ")]

-- cleanText :: String -> String
-- cleanText = cleanText_ regexes

-- cleanText_ :: [(Regex, String)] -> String -> String
-- cleanText_ [] text = text
-- cleanText_ ((pattern, replacement):regexes) text = cleanText_ regexes replacedText
--     where
--         replacedText :: String
--         replacedText = subRegex pattern text replacement
