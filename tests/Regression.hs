module Regression where

import Control.Exception
import Distribution.TestSuite
import System.Directory
import System.FilePath
import System.IO.Strict as Strict
import Text.Pulp (parse)

tests = do
	fs <- getLogs "tests"
	return [Group
		{ groupName = "regression tests"
		, concurrently = True
		, groupTests = map (Test . testLog) fs
		}]

getLogs dir = do
	fs <- getDirectoryContents dir
	return
		[ dir </> prefix
		| f <- fs
		, let (prefix, ext) = splitExtension f
		, ext == ".log"
		]

testLog f = TestInstance
	{ run = go
	, name = f
	, tags = []
	, options = []
	, setOption = \_ _ -> Left "this test does not support any options"
	} where
	inFile  = f `addExtension` ".log"
	outFile = f `addExtension` ".out"
	fileError ty name k = fileCatch (\_ -> return . Finished . Error . unwords $ ["could not open", ty, "file", name]) $ Strict.readFile name >>= k
	go = fileError  "input"  inFile $ \ inText ->
	     fileError "output" outFile $ \outText ->
	     if show (parse inText) == outText
	     then return (Finished Pass)
	     else return . Finished . Fail $ ""

	-- In the future, we could actually inspect the IOError to make sure it
	-- comes from opening a file. But that's probably an edge case we don't
	-- need to worry about for quite some time.
	fileCatch :: (IOError -> IO a) -> IO a -> IO a
	fileCatch = flip Control.Exception.catch
