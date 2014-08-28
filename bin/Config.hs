module Config (Config, parse, eval) where

import Control.Applicative
import Control.Monad.State
import Data.Char
import Data.List
import Data.Universe.Instances.Eq   ()
import Data.Universe.Instances.Ord  ()
import Data.Universe.Instances.Read ()
import Data.Universe.Instances.Show ()
import Text.Pulp hiding (parse)
import Text.Regex.Posix

type Operator = Bool -> Bool -> Bool
data Sentence a
	= Lit Bool
	| Atom a
	| Not (Sentence a)
	| Bin (Sentence a) Operator (Sentence a)
	deriving (Eq, Ord, Show, Read)

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
	| ACloseFile
	| ABoxFullness Fullness
	| ABoxDirection Direction
	| ABoxThreshold Rational
	| AMessageLevel (Maybe MessageLevel)
	| AMessage UncompiledRegex
	| APackage UncompiledRegex
	| ADetails UncompiledRegex
	deriving (Eq, Ord, Show, Read)

type Backtrace = (Annotation, [(String, Annotation)])
evalAtom :: Backtrace -> Line a -> Atom -> Bool
evalAtom _ (Boring         {})   ABoring                  = True
evalAtom _ (Unknown        {})   AUnknown                 = True
evalAtom _ (ExtraCloseFile {})   ACloseFile               = True
evalAtom _ (   NoCloseFile {})   ACloseFile               = True
evalAtom _ (HBox s _)            (ABoxFullness f)         = fullness  s == f
evalAtom _ (HBox s _)            (ABoxDirection d)        = direction s == d
evalAtom _ (HBox s _)            (ABoxThreshold t)        = extractBoxThreshold s > t
evalAtom _ (Error          {})   (AMessageLevel Nothing)  = True
evalAtom _ (LaTeXMessage _ l' _) (AMessageLevel (Just l)) = l == l'
evalAtom _ (Boring s)            (AMessage r)             =          s `matchesRegex` r
evalAtom _ (LaTeXMessage _ _ s)  (AMessage r)             = unlines' s `matchesRegex` r
evalAtom _ (Error s _ _ _ _)     (AMessage r)             =          s `matchesRegex` r
evalAtom _ (Unknown s)           (AMessage r)             =          s `matchesRegex` r
evalAtom _ (LaTeXMessage p _ _)  (APackage r)             =          p `matchesRegex` r
evalAtom _ (Error p _ _ _ _)     (APackage r)             =          p `matchesRegex` r
evalAtom _ (Error _ _ _ _ d)     (ADetails r)             =          d `matchesRegex` r
evalAtom _ _ _ = False

type Config = Sentence Atom
eval :: Backtrace -> Line a -> Config -> Bool
eval = evalSentence .: evalAtom where (.:) = (.).(.)

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

-- that final newline is so pesky
unlines' = intercalate "\n"

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

data Token = Token String | Open | Close | Regex UncompiledRegex | UnclosedRegex
	deriving (Eq, Ord, Show, Read)

token fToken fOpen fClose fRegex fUnclosed t = case t of
	Token s -> fToken s
	Open    -> fOpen
	Close   -> fClose
	Regex u -> fRegex u
	UnclosedRegex -> fUnclosed

isToken, isOpen, isClose, isRegex, isUnclosedRegex :: Token -> Bool
isToken = token (const True) False False (const False) False
isOpen  = token (const False) True False (const False) False
isClose = token (const False) False True (const False) False
isRegex = token (const False) False False (const True) False
isUnclosedRegex = token (const False) False False (const False) True

tokenize :: String -> [Token]
tokenize s = case dropWhile isSpace s of
	[] -> []
	'\'':s -> tokenizeQuote '\'' (Regex . (,) True ) "" s
	'\"':s -> tokenizeQuote '\"' (Regex . (,) False) "" s
	'(':s -> Open  : tokenize s
	')':s -> Close : tokenize s
	s -> let (b, e) = break (\c -> categorize c /= categorize (head s)) s in Token b : tokenize e

tokenizeQuote d f = go where
	go acc []             = [UnclosedRegex]
	go acc (c:s) | c == d = f (reverse acc) : tokenize s
	go acc ('\\':'n':s)   = go ('\n':acc) s
	go acc ('\\':'t':s)   = go ('\t':acc) s
	go acc ('\\':'\'':s)  = go ('\'':acc) s
	go acc ('\\':'"':s)   = go ('"':acc) s
	go acc ('\\':c:s)     = go (c:'\\':acc) s
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

type Parser = StateT [Token] (Either [String])

parse :: String -> Either [String] (Sentence Atom)
parse = evalStateT (parseSentence <* eof) . tokenize

parseSentence :: Parser (Sentence Atom)
parseSentence = precedence <$> parseSentence_ where
	precedence raw = case foldr gather raw precedenceTable of
		(s, []) -> s
		_ -> error "Amazing! We managed to parse an operator that's not in the list of binary operators. This is definitely a bug."

	gather op (s, []) = (s, [])
	gather op (s, (b, c):rest) = case gather op (c, rest) of
		(c, rest) | op == b   -> (Bin s b c, rest)
		          | otherwise -> (s, (b, c):rest)

	precedenceTable = reverse . nub . map snd $ binopKeywords

-- why not (Data.Functor.Alt.<!>)? too many dependencies
-- why not (Control.Applicative.<|>)? the ErrorList class is stupid and also sucks
(<!>) :: Parser a -> Parser a -> Parser a
p1 <!> p2 = StateT $ \s -> case runStateT p1 s of
	Left es    -> runStateT p2 s
	v@Right {} -> v

parseSentence_ :: Parser (Sentence Atom, [(Operator, Sentence Atom)])
parseSentence_ = parseChunk >>= \c -> parseMoreChunks c <!> return (c, []) where
	parseMoreChunks c = do
		b <- parseBinop
		(s, rest) <- parseSentence_
		return (c, (b, s):rest)

parseChunk :: Parser (Sentence Atom)
parseChunk = get >>= \ts -> case ts of
	Token {}:_ -> parseKeywordChunk
	Regex {}:_ -> regex AMessage
	Open  {}:_ -> parseParens
	_          -> expecting "a bare token, a regex, or an open paren" ts

parseKeywordChunk :: Parser (Sentence Atom)
parseKeywordChunk = parseKeyword chunkKeywords >>= \k -> case k of
	Right sentence   -> return sentence
	Left Nothing     -> Not <$> parseChunk
	Left (Just atom) -> case atom of
		"boring"    -> at ABoring
		"unknown"   -> at AUnknown
		"close"     -> at ACloseFile
		"overfull"  -> at (ABoxFullness Over)
		"underfull" -> at (ABoxFullness Under)
		"hbox"      -> at (ABoxDirection Horizontal)
		"vbox"      -> at (ABoxDirection Vertical)
		"threshold" -> Atom . ABoxThreshold <$> number
		"info"      -> ml Info
		"message"   -> ml Message
		"warning"   -> ml Warning
		"error"     -> at (AMessageLevel Nothing)
		"package"   -> regex APackage
		"details"   -> regex ADetails
	where
	at = return . Atom
	ml = at . AMessageLevel . Just

parseParens :: Parser (Sentence Atom)
parseParens = satisfy "open parenthesis" isOpen *> parseSentence <* satisfy "close parenthesis" isClose

parseBinop :: Parser Operator
parseBinop = parseKeyword binopKeywords

parseKeyword :: [(String, a)] -> Parser a
parseKeyword cs = do
	Token s <- satisfy ("one of " ++ intercalate ", " (map fst cs)) isToken
	case expandKeyword s cs of
		Match v      -> return v
		Ambiguous cs -> lift $ Left ["Ambiguous keyword " ++ s, "Continuations include " ++ intercalate ", " cs]
		NoMatch      -> lift $ Left ["Unknown keyword " ++ s, "Expecting one of " ++ intercalate ", " (map fst cs)]

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
binopKeywords    = -- note to self: this list doubles as a precedence order
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
eof = do
	ts <- get
	case ts of
		[] -> return ()
		_  -> expecting "EOF" ts

regex :: (UncompiledRegex -> Atom) -> Parser (Sentence Atom)
regex f = do
	Regex r <- satisfy "a regex" isRegex
	return . Atom . f $ r

number :: Parser Rational
number = do
	Token s <- satisfy "a number" isToken
	case readsRational s of
		r:_ -> return r
		_   -> expecting "a number" [Token s]

satisfy :: String -> (Token -> Bool) -> Parser Token
satisfy s p = do
	t <- state (splitAt 1)
	if any p t
	then return (head t)
	else expecting s t

expecting :: String -> [Token] -> Parser a
expecting s ts = lift . Left $ ["Unexpected " ++ head (map show ts ++ ["EOF"]), "Expecting " ++ s]
