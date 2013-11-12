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

data MessageLevel = Info | Warning | Error
	deriving (Eq, Ord, Show, Read, Enum, Bounded)
data Line
	= Boring String
	| HBox String String
	| File String [Line]
	| Message MessageLevel String [String]
	-- TODO: add LineMarker Integer and emit an extra Line for each line that
	-- matches "line [[:digit:]]+" or "lines [[:digit:]]+--[[:digit:]]+" or
	-- something like that
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
	,"For additional information on amsmath, use the `?' option."
	,"ABD: EveryShipout initializing macros"
	,"Here is how much of TeX's memory you used:"
	,"PDF statistics:"
	]
regexen = map compile $
	["^[[:space:]]*$"
	,"^LaTeX2e <[[:digit:]]{4}/[[:digit:]]{2}/[[:digit:]]{2}>$"
	,"^Babel <.*> and hyphenation patterns for [[:digit:]]* languages loaded\\.$"
	,"^Document Class: .* Standard LaTeX document class$"
	,"^File: " ++ filenameRegex ++ " [[:digit:]]{4}/[[:digit:]]{2}/[[:digit:]]{2}"
	,"^Package: " ++ "[^ ]*"    ++ " [[:digit:]]{4}/[[:digit:]]{2}/[[:digit:]]{2}"
	,"^\\\\[^ =]+=\\\\(count|dimen|toks|mathgroup|skip|box|muskip|write|read)[[:digit:]]+$"
	,"^\\\\openout[[:digit:]]+ = [^']*'\\.$"
	,"^Chapter [[:digit:]]+\\.$"
	,"^[[:space:]]*<" ++ filenameRegex ++ ", id=[[:digit:]]+, " ++ ptRegex ++ " x " ++ ptRegex ++ ">$"
	,"^File: " ++ filenameRegex ++ " Graphic file \\(type [a-z]+\\)$"
	,"^<use " ++ filenameRegex ++ ">$"
	,"^ [[:digit:]]+ (" ++ intercalate "|" statistics ++ ") out of "
	,"^ [^ ]* stack positions out of"
	,"^ [[:digit:]]+ compressed objects within [[:digit:]]+ object streams$"
	,"^([<>]|" ++ filenameRegex ++ ")+$"
	,"^Output written on " ++ filenameRegex ++ " \\([[:digit:]]+ pages, [[:digit:]]+ bytes\\)\\.$"
	] where
	statistics =
		["strings"
		,"string characters"
		,"words of memory"
		,"multiletter control sequences"
		,"words of font info for [[:digit:]]+ fonts,"
		,"hyphenation exceptions"
		,"PDF objects"
		,"named destinations"
		,"words of extra memory for PDF output"
		]
filenameRegex = "[-_./a-zA-Z0-9]*\\.[a-z]{2,}"
ptRegex = "[[:digit:]]+(\\.[[:digit:]]+)?pt"

matchBeginning pat_ = let pat = compile pat_ in \s ->
	case match pat s of
		MR { mrBefore = "", mrMatch = b, mrAfter = e } | not (null b) -> Just (b, e)
		_ -> Nothing

bracketNumber ss = (lines <$>) <$> bracketNumber' (unlines ss)
bracketNumber' = matchBeginning ("[[:space:]]*\\[[[:digit:]]+([[:space:]]|[<>{}]|" ++ filenameRegex ++ ")*\\]")
openFile  = matchBeginning ("[[:space:]]*\\(" ++ filenameRegex)
closeFile = matchBeginning "[[:space:]]*\\)"
beginMessage = matchBeginning "(LaTeX|Package) ([^ ]* )?(Info|Warning|Error): "
beginHBox = matchBeginning ("(Over|Under)full \\\\hbox \\(((badness [[:digit:]]+)|(" ++ ptRegex ++ " too wide))\\) ")

parseMessage b e ss = first (thisM:) (categorize' ss') where
	(package, level) = case words b of
		[_, package, level] -> (package, level)
		[_, level] -> ("LaTeX", level)
	(es, ss') = span (("(" ++ package ++ ")") `isPrefixOf`) ss
	ms        = map (dropWhile isSpace) (e:map (drop (length package + 2)) es)
	thisM     = Message (read (init level)) package ms

-- TODO: I'm sure " []" isn't the only thing that can follow an
-- overfull/underfull hbox message; but what else can?
parseHBox s (e:" []":ss) = first (HBox s e:) (categorize' ss)
parseHBox s (e:ss      ) = first (HBox s e:) (categorize' ss)
parseHBox s ([]        ) = ([HBox s hboxErrorTooShort], []) where
	hboxErrorTooShort = "Huh. I was expecting another line to happen after this hbox error, but none did! Maybe there's a bug in the parser."

categorize' [] = ([], [])
categorize' (s:ss)
	| any (`isPrefixOf` s) prefixes         = label Boring
	| any (s==)            equalities       = label Boring
	| any (`match` s)      regexen          = label Boring
	| Just (f, s' ) <- openFile s           = let (b, e) = categorize' (s':ss)
	                                          in first (file f b:) (categorize' e)
	| Just (_, s' ) <- closeFile s          = ([], s':ss)
	| Just (b, e  ) <- beginMessage s       = parseMessage b e ss
	| Just (b, ss') <- bracketNumber (s:ss) = first (Boring b:) (categorize' ss')
	| Just _        <- beginHBox s          = parseHBox s ss
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
locallyInteresting (Boring s) = []
locallyInteresting (Message Info _ _) = []
locallyInteresting (HBox {}) = []
locallyInteresting (File f ls) = case interesting ls of
	[] -> []
	ls -> [File f ls]
locallyInteresting other = [other]

main = do
	s <- readFile "dissertation/paper.log"
	mapM_ print . treeTake 5 . interesting . parse $ s
