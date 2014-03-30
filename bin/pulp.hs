{-# LANGUAGE DataKinds #-}

import Control.Applicative
import Control.Arrow
import Control.Exception
import Control.Monad
import Data.Char
import Data.List
import GHC.IO.Encoding
import System.Environment
import Text.Pulp
import Text.Regex.Posix

type Operator = Bool -> Bool -> Bool
data Sentence a
	= Lit Bool
	| Atom a
	| Not (Sentence a)
	| Bin (Sentence a) Operator (Sentence a)

evalSentence :: (a -> Bool) -> (Sentence a -> Bool)
evalSentence f = go where
	go (Lit  b) = b
	go (Atom a) = f a
	go (Not  s) = not (go s)
	go (Bin s (*) s') = go s * go s'

data Fullness  = Under      | Over     | UnknownFullness  deriving (Eq, Ord, Show, Read, Bounded, Enum)
data Direction = Horizontal | Vertical | UnknownDirection deriving (Eq, Ord, Show, Read, Bounded, Enum)

fullness s
	| "Underfull" `isPrefixOf` s = Under
	| "Overfull"  `isPrefixOf` s = Over
	| otherwise = UnknownFullness

direction s = case words s of
	_:"\\hbox":_ -> Horizontal
	_:"\\vbox":_ -> Vertical
	_ -> UnknownDirection

type UncompiledRegex = (Bool, String)
data Atom
	= ABoring
	| AUnknown
	| AExtraCloseFile
	| ABoxFullness Fullness
	| ABoxDirection Direction
	| ABoxThreshold Rational
	| AMessageLevel (Maybe MessageLevel)
	| AMessage UncompiledRegex
	| APackage UncompiledRegex
	| ADetails UncompiledRegex
	deriving (Eq, Ord, Show, Read)

evalAtom :: Atom -> Line a -> Bool
evalAtom ABoring                  (Boring         {})   = True
evalAtom AUnknown                 (Unknown        {})   = True
evalAtom AExtraCloseFile          (ExtraCloseFile {})   = True
evalAtom (ABoxFullness f)         (HBox s _)            = fullness  s == f
evalAtom (ABoxDirection d)        (HBox s _)            = direction s == d
evalAtom (ABoxThreshold t)        (HBox s _)            = extractBoxThreshold s > t
evalAtom (AMessageLevel Nothing)  (Error          {})   = True
evalAtom (AMessageLevel (Just l)) (LaTeXMessage _ l' _) = l == l'
evalAtom (AMessage r)             (Boring s)            =         s `matchesRegex` r
evalAtom (AMessage r)             (LaTeXMessage _ _ s)  = unlines s `matchesRegex` r
evalAtom (AMessage r)             (Error s _ _ _ _)     =         s `matchesRegex` r
evalAtom (AMessage r)             (Unknown s)           =         s `matchesRegex` r
evalAtom (APackage r)             (LaTeXMessage p _ _)  =         p `matchesRegex` r
evalAtom (APackage r)             (Error p _ _ _ _)     =         p `matchesRegex` r
evalAtom (ADetails r)             (Error _ _ _ _ d)     =         d `matchesRegex` r
evalAtom _ _ = False

readsRational s = do
	(b, "") <- reads b_
	(e, "") <- reads e_ <|> [(0, "") | null e_]
	return (fromInteger b + fromInteger e / 10 ^ eLen)
	where
	(b_, e__) = break (=='.') s
	e_        = drop 1 e__
	eLen      = genericLength e_

extractBoxThreshold s = case nums of
	[]  -> -1
	n:_ -> n
	where nums = do
		numS <- filter isNumPart <$> words s
		guard (any isDigit numS)
		readsRational numS

matchesRegex s (True , r) = (s =~ r) == (0 :: MatchOffset, length s :: MatchLength)
matchesRegex s (False, r) =  s =~ r

-- proposed EBNF:
-- atom ::=
--     | 'boring'
--     | 'unknown'
--     | 'close'
--     | 'overfull'
--     | 'underfull'
--     | 'hbox'
--     | 'vbox'
--     | 'threshold' n
--     | 'info'
--     | 'message'
--     | 'warning'
--     | 'error'
--     | regex
--     | 'package' regex
--     | 'details' regex
--
-- n ::= digit+ | digit+ '.' digit+
--
-- regex ::= '\'' stringS '\'' | '"' stringD '"'
--
-- stringS ::= charS*
-- stringD ::= charD*
--
-- charS ::= char | escape | '"'
-- charD ::= char | escape | '\''
--
-- char ::= anything but '"' or '\''
-- escape ::= '\\n' | '\\t' | '\\"' | '\\\''
--
-- chunk ::=
--     | '(' sentence ')'
--     | atom
--     | nullop
--     | unop chunk
--
-- sentence ::= chunk | chunk binop sentence
--
-- nullop ::= 'true' | 'false' | '0' | '1'
-- unop   ::= 'not' | '!' | '~'
-- binop  ::= '&&' | '||' | '=>' | '->' | '<=>' | '<->' | '==' | '!=' | '/=' | '^' | '*' | '+' | '/\' | '\/'
--          | 'and' | 'or' | 'xor' | 'nor' | 'nand'

data Token = Token String | Open | Close | Regex UncompiledRegex | UnclosedString
	deriving (Eq, Ord, Show, Read)

tokenize :: String -> [Token]
tokenize s = case dropWhile isSpace s of
	[] -> []
	'\'':s -> tokenizeQuote '\'' (Regex . (,) True ) "" s
	'\"':s -> tokenizeQuote '\"' (Regex . (,) False) "" s
	'(':s -> Open  : tokenize s
	')':s -> Close : tokenize s
	s -> let (b, e) = break (\c -> categorize c /= categorize (head s)) s in Token b : tokenize e

tokenizeQuote d f = go where
	go acc []             = [UnclosedString]
	go acc (c:s) | c == d = f (reverse acc) : tokenize s
	go acc ('\\':'n':s)   = go ('\n':acc) s
	go acc ('\\':'t':s)   = go ('\t':acc) s
	go acc ('\\':c:s)     = go (c:acc) s
	go acc (c:s)          = go (c:acc) s

data Category = Number | Letter | White | Active | Symbol
	deriving (Eq, Ord, Show, Read, Bounded, Enum)

categorize c | isNumPart c = Number
             | isActive  c = Active
             | isLetter  c = Letter
             | isSpace   c = White
             | otherwise   = Symbol

isNumPart c = isDigit c || c == '.'
isActive  c = c `elem` "'\"()"

type Parser a = [Token] -> Either [String] ([Token], a)

parseSentence :: Parser (Sentence Atom)
parseSentence = parseSentence_ >>== \s -> returnn (precedence s) where
	-- TODO
	precedence (s, []) = s
	precedence (s, (b,c):rest) = Bin s b (precedence (c, rest))

parseSentence_ :: Parser (Sentence Atom, [(Operator, Sentence Atom)])
parseSentence_ = parseChunk >>== \c ->
	(parseBinop >>== \b -> parseSentence_ >>== \(s, rest) -> returnn (c, (b, s):rest)) <|>>
	returnn (c, [])

parseChunk :: Parser (Sentence Atom)
parseChunk ts = case ts of
	Token {}:_ -> parseKeywordChunk ts
	Regex {}:_ -> parseRegexChunk   ts
	Open  {}:_ -> parseParens       ts
	_          -> expecting "a bare token, a regex, or an open paren" ts

parseKeywordChunk :: Parser (Sentence Atom)
parseKeywordChunk = parseKeyword chunkKeywords >>== \k -> case k of
	Right sentence   -> returnn sentence
	Left Nothing     -> parseChunk >>== \c -> returnn (Not c)
	Left (Just atom) -> case atom of
		"boring"    -> at ABoring
		"unknown"   -> at AUnknown
		"close"     -> at AExtraCloseFile
		"overfull"  -> at (ABoxFullness Over)
		"underfull" -> at (ABoxFullness Under)
		"hbox"      -> at (ABoxDirection Horizontal)
		"vbox"      -> at (ABoxDirection Vertical)
		"threshold" -> number >>== \n -> at (ABoxThreshold n)
		"info"      -> ml Info
		"message"   -> ml Message
		"warning"   -> ml Warning
		"error"     -> at (AMessageLevel Nothing)
		"package"   -> regex >>== \r -> at (APackage r)
		"details"   -> regex >>== \r -> at (ADetails r)
	where
	at = returnn . Atom
	ml = at . AMessageLevel . Just

parseRegexChunk = regex >>== \r -> returnn (Atom (AMessage r))

parseParens :: Parser (Sentence Atom)
parseParens = consume Open >>== \_ -> parseSentence >>== \s -> consume Close >>== \_ -> returnn s

parseBinop :: Parser Operator
parseBinop = parseKeyword binopKeywords

parseKeyword :: [(String, a)] -> Parser a
parseKeyword cs (Token s:rest) = case expandKeyword s cs of
	Match v      -> Right (rest, v)
	Ambiguous cs -> Left ["Ambiguous keyword " ++ s, "Continuations include " ++ intercalate ", " cs]
	NoMatch      -> Left ["Unknown keyword " ++ s, "Expecting one of " ++ intercalate ", " (map fst cs)]
parseKeyword cs _ = Left ["Unexpected EOF", "Expecting one of " ++ intercalate ", " (map fst cs)]

data KeywordMatch a
	= Match a
	| Ambiguous [String]
	| NoMatch

expandKeyword :: String -> [(String, a)] -> KeywordMatch a
expandKeyword k choices = case filter ((k `isPrefixOf`) . fst) choices of
	[]            -> NoMatch
	[(_, choice)] -> Match choice
	cs            -> case filter ((k==) . fst) cs of
		[]            -> Ambiguous (map fst cs)
		[(_, choice)] -> Match choice
		cs            -> Ambiguous (map fst cs) -- weird case! should never happen

atomKeywords     = ["boring", "unknown", "close", "overfull", "underfull", "hbox", "vbox", "threshold", "info", "message", "warning", "error", "package", "details"]
nullopKeywords   = [("true", True), ("false", False), ("0", False), ("1", True)]
unopKeywords     = ["not", "!", "~"]
binopKeywords    =
	[ ("&&", (&&)), ("*", (&&)), ("/\\", (&&)), ("and", (&&))
	, ("||", (||)), ("+", (||)), ("\\/", (||)), ("or",  (||))
	, ("=>", (==>)), ("->", (==>))
	, ("<=>", (==)), ("<->", (==)), ("==", (==))
	, ("!=", (/=)), ("/=", (/=)), ("^", (/=)), ("xor", (/=))
	, ("nor", nor)
	, ("nand", nand)
	]
chunkKeywords
	=  [(s, Left (Just s)) | s <- atomKeywords]
	++ [(s, Right (Lit b)) | (s, b) <- nullopKeywords]
	++ [(s, Left Nothing)  | s <- unopKeywords]

True  ==> y = y
False ==> y = True

nand x y = not (x && y)
nor  x y = not (x || y)

eof :: Parser ()
eof []    = Right ([], ())
eof (t:_) = Left ["Expected EOF", "Saw token instead: " ++ show t]

regex :: Parser UncompiledRegex
regex (Regex r:rest) = Right (rest, r)
regex ts = expecting "a regex" ts

number :: Parser Rational
number ts@(Token s:rest) = case readsRational s of
	r:_ -> returnn r rest
	_ -> expecting "a number" ts
number ts = expecting "a number" ts

consume :: Token -> Parser Token
consume t (t':rest) | t == t' = Right (rest, t)
consume t ts = expecting (show t) ts

expecting :: String -> Parser a
expecting s ts = Left ["Unexpected " ++ case ts of t:_ -> show t; [] -> "EOF", "Expecting " ++ s]

(p1 >>== f) ts = case p1 ts of
	Left e -> Left e
	Right (ts, a) -> f a ts

returnn v ts = Right (ts, v)

(<|>>) :: Parser a -> Parser a -> Parser a
(p1 <|>> p2) ts = case p1 ts of
	Left _ -> p2 ts
	right  -> right

interesting :: Sentence Atom -> File Annotations -> File Annotations
interesting formula = concatMap go where
	go (l, File f ls) = case concatMap go ls of
		[] -> []
		ls -> [(l, File f ls)]
	go (l, m) = (,) l <$> (retag >=> locallyInteresting formula >=> retag) m

locallyInteresting formula l = [l | evalSentence (`evalAtom` l) formula]

Right ([], defaultFormula) = parseSentence . tokenize $
	"not (boring | info | message | under | over)"

main = do
	args <- getArgs
	setLocaleEncoding latin1
	s <- case args of
		[]     -> getContents
		[file] -> readFile file
		_ -> error "I haven't made a proper command-line parser yet, so this is what\nyou get instead. I hope you know what went wrong now. Idiot."
	-- TODO: we want a more complex chain, I think, so pull this out, refactor
	-- it, think about the chain exactly, etc.
	f <- case args of
		[]     -> return defaultFormula
		[file] -> do
			-- TODO: should probably look at like .file.pulp rather than
			-- file.pulp or something, but be careful about file names with
			-- directory bits in
			s <- try (parseSentence . tokenize <$> readFile (file ++ ".pulp"))
			case s of
				Left e -> let e' :: IOException; e' = e in return defaultFormula
				Right (Right ([], f)) -> return f
				Right (Right (j, _)) -> error ("trailing junk: " ++ show j)
				Right (Left e) -> error (intercalate "\n\t" ("parse error:":e))
		_ -> error "this can't happen -- we should already have thrown an exception!!"
	putStr . prettyPrint . interesting f . parse $ s
