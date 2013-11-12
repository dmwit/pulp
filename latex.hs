{-# LANGUAGE PatternGuards #-}

import Control.Applicative
import Control.Arrow
import Data.Char
import Data.List
import Text.Regex.Posix

groupUntil = groupWhen . (not .)
groupWhen p xs = case span p xs of
	(b, m:e) -> (b ++ [m]) : groupWhen p e
	([], []) -> []
	(b,  []) -> [b]

coalesce = map concat
         . groupWhen (\l -> length l == 79 && not (".tex" `isSuffixOf` l))
         . lines

data InterestLevel = Boring
	deriving (Eq, Ord, Show, Read, Enum, Bounded)
data MessageLevel = Info | Warning | Error
	deriving (Eq, Ord, Show, Read, Enum, Bounded)
data Line
	= Known InterestLevel String
	| File String [Line]
	| Message MessageLevel String [String]
	-- TODO: add LineMarker Integer and emit an extra Line for each line that matches "line [[:digit:]]+" or something like that
	| ExtraCloseFile
	| Unknown String
	deriving (Eq, Ord, Show, Read)

compile :: String -> Regex
compile = makeRegex

prefixes =
	["This is pdfTeX, Version 3."
	,"Style option: `fancyvrb' v"
	,"[Loading MPS to PDF converter (version "
	]
equalities =
	["entering extended mode"
	," restricted \\write18 enabled."
	," %&-line parsing enabled."
	,"**paper"
	,""
	,"For additional information on amsmath, use the `?' option."
	,"ABD: EveryShipout initializing macros"
	]
regexen = map compile $
	["^LaTeX2e <[[:digit:]]{4}/[[:digit:]]{2}/[[:digit:]]{2}>$"
	,"^Babel <.*> and hyphenation patterns for [[:digit:]]* languages loaded\\.$"
	,"^Document Class: .* Standard LaTeX document class$"
	,"^File: " ++ filenameRegex ++ " [[:digit:]]{4}/[[:digit:]]{2}/[[:digit:]]{2}"
	,"^Package: " ++ "[^ ]*"    ++ " [[:digit:]]{4}/[[:digit:]]{2}/[[:digit:]]{2}"
	,"^\\\\[^ =]+=\\\\(count|dimen|toks|mathgroup|skip|box|muskip|write|read)[[:digit:]]+$"
	,"^\\\\openout[[:digit:]]+ = [^']*'\\.$"
	,"^Chapter [[:digit:]]+\\.$"
	]
filenameRegex = "[-_./a-zA-Z0-9]*\\.[a-z]{2,}"

matchBeginning pat_ = let pat = compile pat_ in \s ->
	case match pat s of
		MR { mrBefore = "", mrMatch = b, mrAfter = e } | not (null b) -> Just (b, e)
		_ -> Nothing

bracketNumber ss = (lines <$>) <$> bracketNumber' (unlines ss)
bracketNumber' = matchBeginning "[[:space:]]*\\[[[:digit:]]+[[:space:]]*][[:space:]]*"
openFile  = matchBeginning ("[[:space:]]*\\(" ++ filenameRegex)
closeFile = matchBeginning "[[:space:]]*\\)"
beginMessage = matchBeginning "(LaTeX|Package) ([^ ]* )?(Info|Warning|Error): "

parseMessage b e ss = first (thisM:) (categorize' ss') where
	(package, level) = case words b of
		[_, package, level] -> (package, level)
		[_, level] -> ("LaTeX", level)
	(es, ss') = span (("(" ++ package ++ ")") `isPrefixOf`) ss
	ms        = map (dropWhile isSpace) (e:map (drop (length package + 2)) es)
	thisM     = Message (read (init level)) package ms

categorize' [] = ([], [])
categorize' (s:ss)
	| any (`isPrefixOf` s) prefixes    = label (Known Boring)
	| any (s==)            equalities  = label (Known Boring)
	| any (`match` s)      regexen     = label (Known Boring)
	| Just (f, s' ) <- openFile s      = let (b, e) = categorize' (s':ss)
	                                     in first (file f b:) (categorize' e)
	| Just (_, s' ) <- closeFile s     = ([], s':ss)
	| Just (b, e  ) <- beginMessage s  = parseMessage b e ss
	| Just (b, ss') <- bracketNumber (s:ss) = first (Known Boring b:) (categorize' ss')
	| otherwise = label Unknown
	where
	label f = first (f s:) (categorize' ss)
	file = File . drop 1 . dropWhile isSpace

categorize ss = let (b, e) = categorize' ss in b ++ case e of
	[] -> []
	moreLines -> ExtraCloseFile : categorize moreLines

treeTake n = fst . treeTake' n where
	treeTake' n _ | n <= 0 = ([], 0)
	treeTake' n []         = ([], n)
	treeTake' n (File f ls:rest) = case treeTake' n ls of
		(ls', n') -> first (File f ls':) (treeTake' n' rest)
	treeTake' n (other:rest) = first (other:) (treeTake' (n-1) rest)

parse = categorize . coalesce
interesting = concatMap locallyInteresting
locallyInteresting (Known Boring s) = []
locallyInteresting (Message Info _ _) = []
locallyInteresting (File f ls) = case interesting ls of
	[] -> []
	ls -> [File f ls]
locallyInteresting other = [other]

main = do
	s <- readFile "dissertation/paper.log"
	mapM_ print . treeTake 5 . interesting . parse $ s
