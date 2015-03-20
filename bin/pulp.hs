{-# LANGUAGE DataKinds #-}

import Config as Config
import Control.Applicative
import Control.Exception
import Data.List
import GHC.IO.Encoding
import System.Environment
import System.Exit
import Text.Pulp as Pulp

version = "0.0"

interesting :: Config -> File Annotations -> File Annotations
interesting formula = concatMap (go []) where
	go bt (l, File f ls) = case concatMap (go $ (f, l):bt) ls of
		[] -> []
		ls -> [(l, File f ls)]
	go bt (l, m) = [(l, m) | Config.eval (l, bt) m formula]

defaultFormulaString = "not (boring | info | message | under | over)"
Right defaultFormula = Config.parse defaultFormulaString

data InputSource = STDIN | F String deriving (Eq,Ord,Show)
data State = State { formula :: Config, inputSource :: InputSource } deriving (Eq,Ord,Show)

defaultState = State defaultFormula STDIN

usage = do
	name <- getProgName
	putStrLn $ "pulp v" ++ version
	putStrLn $ "Usage: " ++ name ++ " [-f formula] [filename]\n"
	putStrLn "[formula] specifies which messages to show; it is a boolean formula over message predicates:"
	putStrLn $ intercalate "\n\t"
		[""
		,"overfull, underfull, hbox, vbox -- messages about over/underfull h/vboxes"
		,"info, message, warning, error   -- output from \\PackageError and friends"
		,"boring          -- matches a big regex for known, boring stuff"
		,"unknown         -- unparsed bits of the log file"
		,"close           -- TeX reported opening a file but not closing it, or closing more files than it opened"
		,"threshold <n>   -- *full *box messages whose badness/points are greater than n"
		,"package <regex> -- \\PackageError and friends was called by a package whose name matches the regex"
		,"details <regex> -- long output from \\PackageError or \\ClassError matches the regex"
		,"<regex>         -- a boring, unknown, info, message, warning, or error whose contents matches the regex"
		,"true            -- always"
		,"false           -- never"
		]
	putStrLn $ "\nThe default formula is '" ++ defaultFormulaString ++ "'."
	putStrLn "See https://github.com/dmwit/pulp for detailed info on formulas."
	putStrLn $ "When processing a file 'file.log', if 'file.log.pulp' exists and contains\n" ++ "a formula, it overrides the '-f' option."
	exitFailure

showVersion = do
	putStrLn $ "pulp v" ++ version
	exitFailure

parseFormula expr s =
	case Config.parse expr of
		Left e -> do
			putStrLn $ "Error while parsing formula '" ++ expr ++ "'\n\t" ++ intercalate "\n\t" e
			exitFailure
		Right f -> return $ s { formula = f }

parseArgs [] s = return s
parseArgs ("-h":_) _ = usage
parseArgs ("--help":_) _ = usage
parseArgs ("-v":_) _ = showVersion
parseArgs ("--version":_) _ = showVersion
parseArgs ("-f":expr:args) s = parseFormula expr s >>= parseArgs args
parseArgs ("-":args) s = parseArgs args $ s { inputSource = STDIN }
parseArgs (file:args) s = parseArgs args $ s { inputSource = F file }

checkForPulpFile s =
	if inputSource s /= STDIN && formula s == defaultFormula
	then do
		-- TODO: should probably look at like .file.pulp rather than
		-- file.pulp or something, but be careful about file names with
		-- directory bits in
		let F file = inputSource s
		let pf = file ++ ".pulp"
		f <- try (Config.parse <$> readFile pf)
		case f of
			Left e -> let e' :: IOException; e' = e in return s
			Right (Right f) -> return $ s { formula = f }
			Right (Left e) -> do
				putStrLn $ "ERROR: bad pulp file 'pf':\n\t" ++ intercalate "\n\t" e
				exitFailure
	else return s

readInputSource STDIN = getContents
readInputSource (F file) = readFile file

main = do
	args <- getArgs
	setLocaleEncoding latin1
	-- TODO: we want a more complex chain, I think, so pull this out, refactor
	-- it, think about the chain exactly, etc.
	s <- parseArgs args defaultState >>= checkForPulpFile
	log <- readInputSource $ inputSource s
	putStr . prettyPrint . interesting (formula s) . Pulp.parse $ log
