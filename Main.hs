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

subRegex :: Substitution -> String -> String
subRegex sub input
    | input == output = output
    | otherwise       = subRegex sub output
    where
        output :: String
        output = subRegex' sub input

subRegex' :: Substitution -> String -> String
subRegex' (regex, replace) input
    | null match  = input
    | null groups = before ++ body replace ++ after
    | otherwise   = before ++ head replace ++ head groups ++ end replace ++ after
    where
        matchResult :: (String, String, String, [String])
        (before, match, after, groups) = input =~ regex
