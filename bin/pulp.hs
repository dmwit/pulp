{-# LANGUAGE DataKinds #-}

import Config as Config
import Control.Applicative
import Control.Exception
import Control.Monad
import Data.List
import GHC.IO.Encoding
import System.Environment
import Text.Pulp as Pulp

interesting :: Config -> File Annotations -> File Annotations
interesting formula = concatMap go where
	locallyInteresting formula l = [l | Config.eval formula l]
	go (l, File f ls) = case concatMap go ls of
		[] -> []
		ls -> [(l, File f ls)]
	go (l, m) = (,) l <$> (retag >=> locallyInteresting formula >=> retag) m

Right defaultFormula = Config.parse "not (boring | info | message | under | over)"

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
			s <- try (Config.parse <$> readFile (file ++ ".pulp"))
			case s of
				Left e -> let e' :: IOException; e' = e in return defaultFormula
				Right (Right f) -> return f
				Right (Left e) -> error (intercalate "\n\t" ("parse error:":e))
		_ -> error "this can't happen -- we should already have thrown an exception!!"
	putStr . prettyPrint . interesting f . Pulp.parse $ s
