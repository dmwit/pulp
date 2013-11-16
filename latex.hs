{-# LANGUAGE DataKinds, FlexibleInstances, GADTs, PatternGuards, StandaloneDeriving, TypeFamilies #-}

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Char
import Data.List
import Text.Regex.Posix

-- for auto-generated code
import GHC.Show

-- TODO: import the real Void when Hackage isn't down
data Void
deriving instance Eq  Void
deriving instance Ord  Void
deriving instance Show Void
--deriving instance Read Void -- throws an exception during compilation with 7.4.2

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
data LineIndicators = Markers | Annotations | Flat
	deriving (Eq, Ord, Show, Read, Enum, Bounded)

type Annotation = (Maybe Integer, Maybe Integer)
type family   File (a :: LineIndicators)
type instance File Markers     = [Line Markers]
type instance File Annotations = [(Annotation, Line Annotations)]
type instance File Flat        = Void

data Line a where
	Boring         :: String -> Line a
	HBox           :: String -> String -> Line a
	File           :: String -> File a -> Line a
	Message        :: MessageLevel -> String -> [String] -> Line a
	LineMarker     :: Integer -> Line Markers
	ExtraCloseFile :: Line a
	Unknown        :: String -> Line a
deriving instance Eq   (Line Markers)
deriving instance Ord  (Line Markers)
deriving instance Show (Line Markers)
deriving instance Read (Line Markers)
-- auto-generated code {{{
instance Show (Line Annotations) where
  showsPrec a_a14L (Main.Boring b1_a14M)
    = showParen
        ((a_a14L >= 11))
        ((.)
           (showString "Boring ") (showsPrec 11 b1_a14M))
  showsPrec a_a14N (Main.HBox b1_a14O b2_a14P)
    = showParen
        ((a_a14N >= 11))
        ((.)
           (showString "HBox ")
           ((.)
              (showsPrec 11 b1_a14O)
              ((.) showSpace (showsPrec 11 b2_a14P))))
  showsPrec a_a14Q (Main.File b1_a14R b2_a14S)
    = showParen
        ((a_a14Q >= 11))
        ((.)
           (showString "File ")
           ((.)
              (showsPrec 11 b1_a14R)
              ((.) showSpace (showsPrec 11 b2_a14S))))
  showsPrec a_a14T (Main.Message b1_a14U b2_a14V b3_a14W)
    = showParen
        ((a_a14T >= 11))
        ((.)
           (showString "Message ")
           ((.)
              (showsPrec 11 b1_a14U)
              ((.)
                 showSpace
                 ((.)
                    (showsPrec 11 b2_a14V)
                    ((.)
                       showSpace (showsPrec 11 b3_a14W))))))
  showsPrec _ Main.ExtraCloseFile
    = showString "ExtraCloseFile"
  showsPrec a_a14Z (Main.Unknown b1_a150)
    = showParen
        ((a_a14Z >= 11))
        ((.)
           (showString "Unknown ") (showsPrec 11 b1_a150))
  showList = showList__ (showsPrec 0)
-- }}}

retag :: Line a -> [Line b]
retag (Boring s) = [Boring s]
retag (HBox s e) = [HBox s e]
retag (File s f) = []
retag (Message l s ss) = [Message l s ss]
retag (LineMarker n)   = []
retag (ExtraCloseFile) = [ExtraCloseFile]
retag (Unknown s)      = [Unknown s]

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
lineNumber = let pat = compile "lines? ([[:digit:]]+)(--([[:digit:]]+))?" in \s ->
	case match pat s of
		MR { mrSubList = [b, _, ""] } -> range b b
		MR { mrSubList = [b, _, e ] } -> range b e
		_ -> Nothing
	where
	convert = LineMarker . read
	range s1 s2 = Just (convert s1, convert s2)

-- TODO: don't silently drop l on the ground, and look for line markers within es
parseMessage l b e ss = first (thisM:) (categorize' Nothing ss') where
	(package, level) = case words b of
		[_, package, level] -> (package, level)
		[_, level] -> ("LaTeX", level)
	(es, ss') = span (("(" ++ package ++ ")") `isPrefixOf`) ss
	ms        = map (dropWhile isSpace) (e:map (drop (length package + 2)) es)
	thisM     = Message (read (init level)) package ms

-- TODO: I'm sure " []" isn't the only thing that can follow an
-- overfull/underfull hbox message; but what else can?
parseHBox l s ss = first (HBox s e:) (putLineHere l ss') where
	(e, ss') = case break null ss of
		(es, ss')
			| null  es  -> (hboxErrorTooShort, ss')
			| short es && last es == " []" -> (unlines (init es), ss')
			| short es  -> (unlines es, ss')
			| otherwise -> (hboxErrorTooLong, ss)

	-- heuristic: the Overfull/Underfull hbox message is probably terminated by
	-- a blank line the way we expect if the blank line comes within three or
	-- four lines of the original complaint (otherwise guess that this compiler
	-- uses a different format for all messages, or at least that this message
	-- is in a different format)
	short xs = zipWith const (replicate 5 ()) xs /= replicate 5 ()
	hboxErrorTooShort = "Huh. I was expecting another line to happen after this hbox error, but none did! Maybe there's a bug in the parser."
	hboxErrorTooLong  = "Huh. I was expecting this hbox error to end with a blank line pretty quickly, but it took a long time! Maybe there's a bug in the parser."

maybeCons = maybe id (:)
putLineHere l ss = first (maybeCons l) (categorize' Nothing ss)

categorize' l [] = (maybeCons l [], [])
categorize' l (s:ss)
	| any (`isPrefixOf` s) prefixes         = label Boring
	| any (s==)            equalities       = label Boring
	| any (`match` s)      regexen          = label Boring
	| Just (f, s' ) <- openFile s           = let (b, e) = categorize' Nothing (s':ss)
	                                          in first (file f b:) (putLineHere l e)
	| Just (_, s' ) <- closeFile s          = (maybeCons l [], s':ss)
	| Just (b, ss') <- bracketNumber (s:ss) = first (Boring b:) (putLineHere l ss')
	| (Nothing, Just (b, e)) <- (l, lineNumber s)
	                                        = first (b:) (categorize' (Just e) (s:ss))
	| Just (b, e  ) <- beginMessage s       = parseMessage l b e ss
	| Just _        <- beginHBox s          = parseHBox l s ss
	| otherwise = label Unknown
	where
	label f = first (f s:) (putLineHere l ss)
	file = File . drop 1 . dropWhile isSpace

categorize ss = let (b, e) = categorize' Nothing ss in b ++ case e of
	[] -> []
	moreLines -> ExtraCloseFile : categorize moreLines

parse = categorize . coalesce

treeTake n = fst . treeTake' n where
	treeTake' n _ | n <= 0 = ([], 0)
	treeTake' n []         = ([], n)
	treeTake' n (File f ls:rest) = case treeTake' n ls of
		(ls', n') -> first (File f ls':) (treeTake' n' rest)
	treeTake' n (other:rest) = first (other:) (treeTake' (n-1) rest)

annotate :: File Markers -> File Annotations
annotate = concatMap retagAnnot . liftA3 zip3 (scanl (flip combine) Nothing) (scanr combine Nothing) id where
	combine (LineMarker n) l = Just n
	combine _ l = l

	retagAnnot (b, e, l) = (,) (b, e) <$> case l of
		File s f       -> [File s (annotate f)]
		_              -> retag l

interesting :: File Annotations -> File Annotations
interesting = concatMap go where
	go (l, File f ls) = case interesting ls of
		[] -> []
		ls -> [(l, File f ls)]
	go (l, m) = (,) l <$> (retag >=> locallyInteresting >=> retag) m

locallyInteresting (Boring s) = []
locallyInteresting (Message Info _ _) = []
locallyInteresting (HBox s _) | "Underfull \\hbox (badness " `isPrefixOf` s = []
locallyInteresting other = [other]

prettyPrint = show . interesting . annotate

used :: File Markers -> [String]
used = concatMap go where
	go (File f ls) = f : used ls
	go _ = []

prettyPrintUsedFiles = unlines . nub . filter ("." `isPrefixOf`) . used

main = do
	s <- readFile "dissertation/paper.log"
	putStrLn . prettyPrint . parse $ s
