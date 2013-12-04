{-# LANGUAGE DataKinds #-}

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.List
import GHC.IO.Encoding
import System.Environment
import Text.Pulp

treeTake n = fst . treeTake' n where
	treeTake' n _ | n <= 0 = ([], 0)
	treeTake' n []         = ([], n)
	treeTake' n (File f ls:rest) = case treeTake' n ls of
		(ls', n') -> first (File f ls':) (treeTake' n' rest)
	treeTake' n (other:rest) = first (other:) (treeTake' (n-1) rest)

interesting :: File Annotations -> File Annotations
interesting = concatMap go where
	go (l, File f ls) = case interesting ls of
		[] -> []
		ls -> [(l, File f ls)]
	go (l, m) = (,) l <$> (retag >=> locallyInteresting >=> retag) m

locallyInteresting (Boring _) = []
locallyInteresting (LaTeXMessage _ Info    _) = []
locallyInteresting (LaTeXMessage _ Message _) = []
locallyInteresting (HBox _ _) = []
locallyInteresting other = [other]

used :: File Markers -> [String]
used = concatMap go where
	go (File f ls) = f : used ls
	go _ = []

prettyPrintUsedFiles = unlines . nub . filter ("." `isPrefixOf`) . used

main = do
	args <- getArgs
	setLocaleEncoding latin1
	s <- case args of
		[]     -> getContents
		[file] -> readFile file
		_ -> error "I haven't made a proper command-line parser yet, so this is what\nyou get instead. I hope you know what went wrong now. Idiot."
	putStr . prettyPrint . interesting . parse $ s
