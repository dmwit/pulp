{-# LANGUAGE DataKinds, FlexibleInstances, GADTs, PatternGuards, StandaloneDeriving, TypeFamilies #-}

module Text.Pulp
	( parse, prettyPrint, uglyPrint
	, retag
	, Line(..)
	, File(..)
	, MessageLevel(..)
	, LineIndicators(..)
	) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Void
import Text.Regex.Posix

-- for auto-generated code
import GHC.Show

-- TODO: why is parsing slow.log SO slow (like, more than a minute slow)?
-- TODO: some of the logs in the test suite produce an ExtraCloseFile; why?
-- TODO: check that we handle "fatal error" message correctly, e.g. see tests/fatal-error.log

groupUntil = groupWhen . (not .)
groupWhen p xs = case span p xs of
	(b, m:e) -> (b ++ [m]) : groupWhen p e
	([], []) -> []
	(b,  []) -> [b]

coalesce = map concat
         . groupWhen (\l -> length l == 79 && not (".tex" `isSuffixOf` l || "..." `isSuffixOf` l))
         . lines

data MessageLevel = Info | Message | Warning
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
	LaTeXMessage   :: String -> MessageLevel -> [String] -> Line a
	LineMarker     :: Integer -> Line Markers
	ExtraCloseFile :: Line a
	-- the arguments to Error are:
	-- 1. the package that complained (or "LaTeX" or "TeX")
	-- 2. the error message
	-- 3. the command sequence that was being processed when things died, if different from the next argument
	-- 4. the command sequence in the source file that was being expanded when things died
	-- 5. detailed message
	Error          :: String -> String -> Maybe (String, String) -> (String, String) -> String -> Line a
	Unknown        :: String -> Line a
deriving instance Eq   (Line Markers)
deriving instance Ord  (Line Markers)
deriving instance Show (Line Markers)
deriving instance Read (Line Markers)
-- auto-generated code {{{
instance Show (Line Annotations) where
    showsPrec a_a1qO (Text.Pulp.Boring b1_a1qP)
      = showParen
          ((a_a1qO >= 11))
          ((.)
             (showString "Boring ") (showsPrec 11 b1_a1qP))
    showsPrec a_a1qQ (Text.Pulp.HBox b1_a1qR b2_a1qS)
      = showParen
          ((a_a1qQ >= 11))
          ((.)
             (showString "HBox ")
             ((.)
                (showsPrec 11 b1_a1qR)
                ((.) showSpace (showsPrec 11 b2_a1qS))))
    showsPrec a_a1qT (Text.Pulp.File b1_a1qU b2_a1qV)
      = showParen
          ((a_a1qT >= 11))
          ((.)
             (showString "File ")
             ((.)
                (showsPrec 11 b1_a1qU)
                ((.) showSpace (showsPrec 11 b2_a1qV))))
    showsPrec
      a_a1qW
      (Text.Pulp.LaTeXMessage b1_a1qX b2_a1qY b3_a1qZ)
      = showParen
          ((a_a1qW >= 11))
          ((.)
             (showString "LaTeXMessage ")
             ((.)
                (showsPrec 11 b1_a1qX)
                ((.)
                   showSpace
                   ((.)
                      (showsPrec 11 b2_a1qY)
                      ((.)
                         showSpace (showsPrec 11 b3_a1qZ))))))
    showsPrec _ Text.Pulp.ExtraCloseFile
      = showString "ExtraCloseFile"
    showsPrec
      a_a1r2
      (Text.Pulp.Error b1_a1r3 b2_a1r4 b3_a1r5 b4_a1r6 b5_a1r7)
      = showParen
          ((a_a1r2 >= 11))
          ((.)
             (showString "Error ")
             ((.)
                (showsPrec 11 b1_a1r3)
                ((.)
                   showSpace
                   ((.)
                      (showsPrec 11 b2_a1r4)
                      ((.)
                         showSpace
                         ((.)
                            (showsPrec 11 b3_a1r5)
                            ((.)
                               showSpace
                               ((.)
                                  (showsPrec 11 b4_a1r6)
                                  ((.)
                                     showSpace (showsPrec 11 b5_a1r7))))))))))
    showsPrec a_a1r8 (Text.Pulp.Unknown b1_a1r9)
      = showParen
          ((a_a1r8 >= 11))
          ((.)
             (showString "Unknown ") (showsPrec 11 b1_a1r9))
    showList = showList__ (showsPrec 0)
-- }}}
instance Eq (Line Annotations) where
	Boring  s   == Boring  s'    = s == s'
	HBox    s e == HBox    s' e' = s == s' && e == e'
	File    s f == File    s' f' = s == s' && f == f'
	Unknown s   == Unknown s'    = s == s'
	LaTeXMessage s m ss == LaTeXMessage s' m' ss' = s == s' && m == m' && ss == ss'
	ExtraCloseFile == ExtraCloseFile = True
	_ == _ = False

instance Read (Line Annotations) where
	readsPrec n s = do
		(v, s') <- readsPrec n s
		v' <- (retag :: Line Markers -> [Line Annotations]) v
		return (v', s')

retag :: Line a -> [Line b]
retag (Boring s) = [Boring s]
retag (HBox s e) = [HBox s e]
retag (File s f) = []
retag (LaTeXMessage p l ss) = [LaTeXMessage p l ss]
retag (LineMarker n)        = []
retag (ExtraCloseFile)      = [ExtraCloseFile]
retag (Error p m s s' d)    = [Error p m s s' d]
retag (Unknown s)           = [Unknown s]

compile :: String -> Regex
compile = makeRegex

trim = dropWhile isSpace
prefixes =
	["This is pdfTeX, Version 3."
	,"Style option: `fancyvrb' v"
	,"[Loading MPS to PDF converter (version "
	,"*geometry* driver: "
	,"*geometry* detected driver: "
	]
equalities =
	["entering extended mode"
	,"restricted \\write18 enabled."
	,"%&-line parsing enabled."
	,"For additional information on amsmath, use the `?' option."
	,"ABD: EveryShipout initializing macros"
	,"Here is how much of TeX's memory you used:"
	,"PDF statistics:"
	,"Forcing optional page break"
	]
regexen = map compile $
	["^[[:space:]]*$"
	,"^LaTeX2e <" ++ dateRegex ++ ">$"
	,"^Babel <.*> and hyphenation patterns for [[:digit:]]* languages loaded\\.$"
	,"^Document Class: (beamer|report|article)"
	,"^File: " ++ filenameRegex ++ " " ++ dateRegex
	,"^File: " ++ filenameRegex ++ " Graphic file \\(type [a-z]+\\)$"
	,"^File: " ++ filenameRegex ++ " $"
	,"^Package: [^ ]* " ++ dateRegex
	,"^\\\\[^ =]+=\\\\(count|dimen|toks|mathgroup|skip|box|muskip|write|read)[[:digit:]]+$"
	,"^\\\\openout[[:digit:]]+ = [^']*'\\.$"
	,"^Chapter [[:digit:]]+\\.$"
	,"^[[:space:]]*<" ++ filenameRegex ++ ", id=[[:digit:]]+, " ++ ptRegex ++ " x " ++ ptRegex ++ ">$"
	,"^[[:space:]]*<use " ++ filenameRegex ++ ">$"
	,"^ [[:digit:]]+ (" ++ intercalate "|" statistics ++ ") out of "
	,"^ [[:digit:]]+ compressed objects? within [[:digit:]]+ object streams?$"
	,"^ [^ ]* stack positions? out of"
	,"^([<>]|" ++ filenameRegex ++ ")+$"
	,"^Output written on " ++ filenameRegex ++ " \\([[:digit:]]+ pages?, [[:digit:]]+ bytes?\\)\\.$"
	,"^\\*\\*[-_.a-zA-Z0-9]*$"
	,"^Dictionary: [-a-z]*, Language: [[:alpha:]]* $"
	] where
	statistics =
		["strings?"
		,"string characters?"
		,"words? of memory"
		,"multiletter control sequences?"
		,"words? of font info for [[:digit:]]+ fonts?,"
		,"hyphenation exceptions?"
		,"PDF objects?"
		,"named destinations?"
		,"words? of extra memory for PDF output"
		]
dateRegex = "[[:digit:]]{4}/[[:digit:]]{2}/[[:digit:]]{2}"
filenameRegex = "[-_./a-zA-Z0-9]*\\.[a-z]{2,}"
ptRegex = "[[:digit:]]+(\\.[[:digit:]]+)?pt"

matchBeginning pat_ = let pat = compile pat_ in \s ->
	case match pat s of
		MR { mrBefore = "", mrMatch = b, mrAfter = e } | not (null b) -> Just (b, e)
		_ -> Nothing

bracketNumber ss = (lines <$>) <$> bracketNumber' (unlines ss)
bracketNumber' = matchBeginning ("[[:space:]]*\\[[[:digit:]]+([[:space:]]|[<>{}]|" ++ filenameRegex ++ ")*\\]")
openFile       = matchBeginning ("[[:space:]]*\\(" ++ filenameRegex)
closeFile      = matchBeginning "[[:space:]]*\\)"
beginMessage   = matchBeginning "(LaTeX|Package) ([^ ]* )?(Info|Message|Warning): "
beginHBox      = matchBeginning ("(Over|Under)full \\\\[hv]box \\(((badness [[:digit:]]+)|(" ++ ptRegex ++ " too (wide|high)))\\) ")
lineNumber = let pat = compile "lines? ([[:digit:]]+)(--([[:digit:]]+))?" in \s ->
	case match pat s of
		MR { mrSubList = [b, _, ""] } -> range b b
		MR { mrSubList = [b, _, e ] } -> range b e
		_ -> Nothing
	where
	convert = LineMarker . read
	range s1 s2 = Just (convert s1, convert s2)

-- TODO: May have to parse LaTeX warnings(/errors/infos/messages?) separately,
-- since they don't put "(LaTeX)" at the beginning of each continuation line.
-- (Maybe they terminate with a blank line.) See e.g.
-- tests/multiline-latex-warning.log.

parseMessage l b e ss = first (thisM:) (putLineHere (maximum' lms) ss') where
	(package, level) = case words b of
		[_, package, level] -> (package, level)
		[_, level] -> ("LaTeX", level)
	(es, ss') = span (("(" ++ package ++ ")") `isPrefixOf`) ss
	ms        = map (dropWhile isSpace) (e:map (drop (length package + 2)) es)
	thisM     = LaTeXMessage package (read (init level)) ms
	lms       = [lm | Just lm <- [l]] ++ [lm | Just (b, e) <- map lineNumber ms, lm <- [b, e]]

	maximum' [] = Nothing
	maximum' xs = Just (maximum xs)

-- heuristic: the Overfull/Underfull hbox message is probably terminated by
-- a blank line the way we expect if the blank line comes within three or
-- four lines of the original complaint (otherwise guess that this compiler
-- uses a different format for all messages, or at least that this message
-- is in a different format)
parseHBox l s ss = first (HBox s e:) (putLineHere l ss') where
	(expected, unsure) = splitAt 1 ss
	messageEnd         = findBlankWithin 4 unsure
	(message_, ss'_)   = let Just (a, b) = messageEnd in (expected ++ a, b)
	-- TODO: I'm sure " []" isn't the only thing that can follow an
	-- overfull/underfull hbox message; but what else can?
	message            = if last message_ == " []" then init message_ else message_

	(e, ss') = case (expected, messageEnd) of
		(_ , Nothing) -> (hboxErrorTooLong , ss  )
		([], Just {}) -> (hboxErrorTooShort, ss'_)
		_             -> (unlines message  , ss'_)

	hboxErrorTooShort = "Huh. I was expecting another line to happen after this hbox error, but none did! Maybe there's a bug in the parser."
	hboxErrorTooLong  = "Huh. I was expecting this hbox error to end with a blank line pretty quickly, but it took a long time! Maybe there's a bug in the parser."

geometryVerboseMode = "*geometry* verbose mode - [ preamble ] result:"
parseGeometryVerboseMode l ss = first (map Boring results ++) (putLineHere l rest) where
	(results, rest) = span ("* " `isPrefixOf`) ss

-- TeX errors usually look like this:
-- 1. A header. One of three things:
--        ! LaTeX Error: <message>
--        <blank line>
--        <instructions to look in the documentation>
--
--        OR
--
--        ! Package <package name> Error: <message>
--        <blank line>
--        <instructions to look in the documentation>
--
--        OR
--
--        ! message
--    When it's passed to us, the "! " has already been stripped.
--
-- 2. An optional broken command sequence telling what's currently being
--    processed.
--        \foo\bar
--                \baz
--
-- 3. A line-annotated broken command sequence telling where we are in
--    user-written source.
--        l.<number> \quux\bedazzle
--                                 \ricket\flam
--
-- 4. Detailed help. This is arbitrary text terminated by a blank line.
parseTeXError l s ss = fromMaybe giveUp (parseLaTeXError <|> parsePackageError <|> parseTeXError) where
	parseLaTeXError = do
		messageBegin <- stripPrefix "LaTeX Error: " s
		parseLaTeXOrPackageRemainder "LaTeX" messageBegin
	parsePackageError = do
		s' <- stripPrefix "Package " s
		(package, messageBegin) <- stripInfix " Error: " s'
		parseLaTeXOrPackageRemainder package messageBegin
	parseTeXError = do
		(messageEnd, seqCurr, l', seqSource, ss')
		                <- findCommandSequences ss
		(details, ss'') <- findBlankWithin 20 ss'
		return $ done (Error
		               	"TeX"
		               	(unwords . map trim $ s:messageEnd)
		               	seqCurr
		               	seqSource
		               	(unwords details)
		              ) (Just l') ss''
	giveUp = done (Unknown ("! " ++ s)) Nothing ss

	done line marker ss = first (maybeCons marker . (line:) . maybeCons (marker <|> l)) (categorize' Nothing ss)
	parseLaTeXOrPackageRemainder package messageBegin = do
		(messageEnd, ss') <- findBlankWithin 7 ss
		(docInstr, seqCurr, l', seqSource, ss'')
		                  <- findCommandSequences ss'
		(details, ss''')  <- findBlankWithin 20 ss''
		return $ done (Error
		               	package
		               	(unwords . map trim $ messageBegin:messageEnd)
		               	seqCurr
		               	seqSource
		               	(unwords details)
		              ) (Just l') ss'''

	isLineHerald ('l':'.':rest) = not . null . (reads :: ReadS Integer) $ rest
	isLineHerald _ = False

	parseLineHerald h = do
		lineMark <- stripPrefix "l." h
		case reads lineMark of
			(n, s):_ -> return (n, s)
			_ -> Nothing
	
	trimBoth s s' = (trim s, trim s')

	findCommandSequences ss = do
		n <- findIndex isLineHerald (take 10 ss)
		if n >= 2
			then do
				(b, seqCurrBegin:seqCurrEnd:lineHerald:seqSourceEnd:e) <- return (splitAt (n-2) ss)
				(l, seqSourceBegin) <- parseLineHerald lineHerald
				return (b, Just (trimBoth seqCurrBegin seqCurrEnd), LineMarker l, trimBoth seqSourceBegin seqSourceEnd, e)
			else do
				(b, lineHerald:seqSourceEnd:e) <- return (splitAt n ss)
				(l, seqSourceBegin) <- parseLineHerald lineHerald
				return (b, Nothing, LineMarker l, trimBoth seqSourceBegin seqSourceEnd, e)

findBlankWithin n ss = guard (short bs && not (null es)) >> return (bs, drop 1 es) where
	shortList = replicate (n+1) ()
	short     = (shortList /=) . zipWith const shortList
	(bs, es)  = break null ss

stripInfix _ [] = Nothing
stripInfix needle haystack@(h:aystack)
	=   ((,) [] <$> stripPrefix needle haystack)
	<|> (first (h:) <$> stripInfix needle aystack)

maybeCons = maybe id (:)
putLineHere l ss = first (maybeCons l) (categorize' Nothing ss)

categorize' l [] = (maybeCons l [], [])
categorize' l (s:ss)
	| any (`isPrefixOf` s) prefixes         = label Boring
	| any (trim s==)       equalities       = label Boring
	| any (`match` s)      regexen          = label Boring
	| s == geometryVerboseMode              = first (Boring s:) (parseGeometryVerboseMode l ss)
	| Just (f, s' ) <- openFile s           = let (b, e) = categorize' Nothing (s':ss)
	                                          in first (file f b:) (putLineHere l e)
	| Just (_, s' ) <- closeFile s          = (maybeCons l [], s':ss)
	| Just (b, ss') <- bracketNumber (s:ss) = first (Boring b:) (categorize' l ss')
	| (Nothing, Just (b, e)) <- (l, lineNumber s)
	                                        = first (b:) (categorize' (Just e) (s:ss))
	| Just (b, e  ) <- beginMessage s       = parseMessage  l b e ss
	| Just _        <- beginHBox s          = parseHBox     l s   ss
	| Just s'       <- stripPrefix "! " s   = parseTeXError l s'  ss
	| otherwise = label Unknown
	where
	label f = first (f s:) (putLineHere l ss)
	file = File . drop 1 . dropWhile isSpace

categorize ss = let (b, e) = categorize' Nothing ss in b ++ case e of
	[] -> []
	moreLines -> ExtraCloseFile : categorize moreLines

parse = annotate . categorize . coalesce

annotate :: File Markers -> File Annotations
annotate = concatMap retagAnnot . liftA3 zip3 (scanl (flip combine) Nothing) (scanr combine Nothing) id where
	combine (LineMarker n) l = Just n
	combine _ l = l

	retagAnnot (b, e, l) = (,) (b, e) <$> case l of
		File s f -> [File s (annotate f)]
		_        -> retag l

prettyPrint :: File Annotations -> String
prettyPrint = concatMap (go []) where
	go fs         (l, File f ls) = concatMap (go ((l, f):fs)) ls
	go ((_, f):_) (l, m)         = f ++ ":" ++ pprintLoc l ++ pprintMess m ++ "\n"
	go []         (l, m)         = pprintLoc l ++ pprintMess m ++ "\n"

	pprintLoc (l1, l2)  = pprintLine l1 ++ "-" ++ pprintLine l2 ++ ": "
	pprintLine Nothing  = "?"
	pprintLine (Just l) = show l

	pprintMess (Boring s) = s
	pprintMess (HBox s e) = s
	pprintMess (LaTeXMessage p l ss) = p ++ " " ++ map toLower (show l) ++ ":\n\t" ++ intercalate "\n\t" ss
	pprintMess (ExtraCloseFile) = "For some reason, the log-file parser noticed an extra 'close file' marker here.\n\tIt's possible that the filenames and line numbers reported near this are wrong.\n\tThis is likely a bug -- you should report it and include your log file!"
	pprintMess (Error p s (Just (beg, end)) (beg', end') _) = p ++ " error: " ++ s ++ "\n\t" ++ beg' ++ "\n\t" ++ end' ++ "\n\t" ++ beg ++ "\n\t" ++ end
	pprintMess (Error p s Nothing           (beg', end') _) = p ++ " error: " ++ s ++ "\n\t" ++ beg' ++ "\n\t" ++ end'
	pprintMess (Unknown s) = s

uglyPrint :: File Annotations -> String
uglyPrint = unlines . map show . concatMap (go []) where
	go fs (l, File f ls) = concatMap (go ((l, f):fs)) ls
	go fs m = [(fs, m)]
