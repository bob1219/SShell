-- Copyright 2018 Daiki Yoshida
--
-- This file is part of SShell.
--
-- SShell is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- SShell is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with SShell. If not, see <http://www.gnu.org/licenses/>.

module SShell.Command (commandProcess, tokenizeCommand) where

import System.Directory	(doesFileExist, removeFile, copyFileWithMetadata, renameFile, createDirectory, removeDirectory, doesDirectoryExist, listDirectory, renameDirectory, setCurrentDirectory, getCurrentDirectory, exeExtension)
import System.IO.Error	(catchIOError, isAlreadyExistsError, isDoesNotExistError, isAlreadyInUseError, isFullError, isEOFError, isIllegalOperation, isPermissionError, ioError)
import SShell.Constant	(unexceptedException, version, commandLineError)
import Text.Read	(readMaybe)
import System.Exit	(exitSuccess)
import System.Process	(createProcess, waitForProcess, proc)
import System.FilePath	(isAbsolute)

commandProcess :: [String] -> IO ()
commandProcess []		=	error "got empty list"
commandProcess (token:tokens)	=	(case token of	"mkfile"	-> run tokens 1 (command_mkfile $ tokens !! 0)
							"rmfile"	-> run tokens 1 (command_rmfile $ tokens !! 0)
							"cpfile"	-> run tokens 2 (command_cpfile (tokens !! 0) (tokens !! 1))
							"renfile"	-> run tokens 2 (command_renfile (tokens !! 0) (tokens !! 1))
							"mkdir"		-> run tokens 1 (command_mkdir $ tokens !! 0)
							"rmdir"		-> run tokens 1 (command_rmdir $ tokens !! 0)
							"cpdir"		-> run tokens 2 (command_cpdir (tokens !! 0) (tokens !! 1))
							"rendir"	-> run tokens 2 (command_rendir (tokens !! 0) (tokens !! 1))
							"view"		-> run tokens 1 (command_view $ tokens !! 0)
							"chcwd"		-> run tokens 1 (command_chcwd $ tokens !! 0)
							"pcwd"		-> command_pcwd
							"path"		-> run tokens 2 $ command_path tokens
							"list"		-> run tokens 1 (command_list $ tokens !! 0)
							"version"	-> command_version
							"exit"		-> command_exit
							_		-> exec (token:tokens))
						`catchIOError` (\e -> commandLineError $ case e of _	| isAlreadyExistsError e	-> "it already exists"
													| isDoesNotExistError e		-> "it does not exist"
													| isAlreadyInUseError e		-> "it is already in use"
													| isFullError e			-> "your device is full"
													| isEOFError e			-> "eof error"
													| isIllegalOperation e		-> error "your platform is not supported in SShell"
													| isPermissionError e		-> "you do not have the permission"
													| otherwise			-> unexceptedException e)


run :: [String] -> Int -> IO () -> IO ()
run tokens n f = if (length tokens) < n then commandLineError "few args" else f

command_mkfile :: FilePath -> IO ()
command_mkfile file = do	exists <- doesFileExist file
				if exists
					then commandLineError "that file already exists"
					else writeFile file ""

checkAndDo :: String -> IO () -> IO ()
checkAndDo message f = do	putStrLn $ message ++ " (y/n)"
				loop
	where
		loop = do	putChar '>'
				answer <- getChar
				case answer of	'y'	-> f
						'n'	-> return ()
						_	-> loop

command_rmfile :: FilePath -> IO ()
command_rmfile file = checkAndDo ("Are you realy remove file \"" ++ file ++ "\"?") $ removeFile file

command_cpfile :: FilePath -> FilePath -> IO ()
command_cpfile = copyFileWithMetadata

command_renfile :: FilePath -> FilePath -> IO ()
command_renfile = renameFile

command_mkdir :: FilePath -> IO ()
command_mkdir = createDirectory

command_rmdir :: FilePath -> IO ()
command_rmdir dir = checkAndDo ("Do you really want to remove directory \"" ++ dir ++ "\"?") $ removeDirectory dir

command_cpdir :: FilePath -> FilePath -> IO ()
command_cpdir src dst = do	srcExists <- doesDirectoryExist src
				if not srcExists
					then commandLineError "it does not exist"
					else do	dstExists <- doesDirectoryExist dst
						if not dstExists
							then createDirectory dst
							else return ()
						listDirectory src >>= loop src dst
	where
		loop _ _ []			= return ()
		loop src' dst' (file:files)	= do	isFile <- doesFileExist src'
							if isFile
								then copyFileWithMetadata newsrc newdst
								else command_cpdir newsrc dst'
							loop src dst files
			where
				newsrc = src' ++ "/" ++ file
				newdst = dst' ++ "/" ++ file

command_rendir :: FilePath -> FilePath -> IO ()
command_rendir = renameDirectory

command_view :: FilePath -> IO ()
command_view file = (lines <$> readFile file) >>= loop (1 :: Integer)
	where
		loop _ []	= return ()
		loop n (x:xs)	= do	putStrLn $ (show n) ++ ":\t" ++ x
					loop (n + 1) xs

command_chcwd :: FilePath -> IO ()
command_chcwd = setCurrentDirectory

command_pcwd :: IO ()
command_pcwd = getCurrentDirectory >>= putStrLn

command_path :: [String] -> IO ()
command_path []		= error "got empty list"
command_path (arg:args)	= case arg of	"list"	-> command_path_list
					"add"	-> run args 1 (command_path_add $ args !! 0)
					"clear"	-> command_path_clear
					"del"	-> run args 1 $ case readMaybe (args !! 0) of	Just n	-> command_path_del n
												Nothing	-> commandLineError "invalid number"
					_	-> commandLineError "that command not found"

pathFileName :: FilePath
pathFileName = "./../data/PATH"

getPaths :: IO [FilePath]
getPaths =	(lines <$> readFile pathFileName)
			`catchIOError` (\e ->	if isDoesNotExistError e
							then return []
							else ioError e)

command_path_list :: IO ()
command_path_list = command_view pathFileName

command_path_add :: FilePath -> IO ()
command_path_add dir = do	isAlreadyFound <- elem dir <$> getPaths
				if isAlreadyFound
					then commandLineError "it is already found in the paths"
					else appendFile pathFileName (dir ++ "\n")

command_path_clear :: IO ()
command_path_clear = writeFile pathFileName ""

command_path_del :: Int -> IO ()
command_path_del n = do	paths <- getPaths
			if n < 1 || n > (length paths)
				then commandLineError "invalid number"
				else (writeFile pathFileName) . unlines . (f n) $ paths
	where
		f x list = (take (x - 1) list) ++ (drop x list)

command_list :: FilePath -> IO ()
command_list dir = listDirectory dir >>= loop
	where
		loop []			= return ()
		loop (file:files)	= do	isFile <- doesFileExist (dir ++ "/" ++ file)
						putStrLn $ (if isFile then "file" else "dir") ++ ":\t" ++ file
						loop files

command_version :: IO ()
command_version = putStrLn version

command_exit :: IO a
command_exit = exitSuccess

exec :: [String] -> IO ()
exec []			= error "got empty list"
exec (software:args)	= do	software' <- pathProcess software
				case software' of	Just software''	-> do	(_, _, _, handle) <- createProcess (proc software'' args)
										_ <- waitForProcess handle
										return ()
							Nothing		-> commandLineError "that command or software not found"

pathProcess :: FilePath -> IO (Maybe FilePath)
pathProcess software = do	if isAbsolute software
					then	do	exists <- doesFileExist software
							if exists
								then	return $ Just software
								else	let software' = software ++ exeExtension
									in do	exists' <- doesFileExist software'
										if exists'
											then return $ Just software'
											else return Nothing
					else	let software' = "./" ++ software
						in do	exists <- doesFileExist software'
							if exists
								then	return $ Just software'
								else	let software'' = software' ++ exeExtension
									in do	exists' <- doesFileExist software''
										if exists'
											then return $ Just software''
											else do	software''' <- (getPaths >>= loop software)
												case software''' of	Just _	-> return software'''
															Nothing	-> return Nothing
	where
		loop _ []			= return Nothing
		loop software' (path:paths)	= do	let software'' = path ++ "/" ++ software'
							exists <- doesFileExist software''
							if exists
								then return $ Just software''
								else do	let software''' = software'' ++ exeExtension
									exists' <- doesFileExist software'''
									if exists'
										then return $ Just software'''
										else loop software' paths

tokenizeCommand :: String -> Maybe [String]
tokenizeCommand command = loop command False False "" []
	where
		loop [] _ _ temp result				= Just (if temp == "" then result else (result ++ [temp]))
		loop (c:cs) isQuoted isEscaped temp result	= case c of	'\''	->	if isEscaped
													then loop cs isQuoted False (temp ++ ['\'']) result
													else loop cs (not isQuoted) False "" (result ++ [temp])
										'\\'	->	if isEscaped
													then	loop cs True False (temp ++ ['\\']) result
													else	if isQuoted
															then loop cs True True temp result
															else loop cs False False (temp ++ ['\\']) result
										' '	->	if isEscaped
													then	Nothing
													else	if isQuoted
															then loop cs True False (temp ++ [' ']) result
															else loop cs False False "" (if temp == "" then result else (result ++ [temp]))
										_	->	if isEscaped
													then Nothing
													else loop cs isQuoted False (temp ++ [c]) result
