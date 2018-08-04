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

import System.IO	(hPutStrLn, stderr)
import System.Directory	(doesFileExist, removeFile, copyFileWithMetadata)
import System.IO.Error	(IOError, isAlreadyExistsError, isDoesNotExistError, isAlreadyInUseError, isFullError, isEOFError, isIllegalOperation, isPermissionError)
import SShell.Constant	(unexceptedException)

commandProcess :: [String] -> IO ()
commandProcess []		= error "got empty list"
commandProcess (token:tokens)	= case token of	"mkfile"	-> run tokens 1 (command_mkfile $ tokens !! 0)
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
						"list"		-> run tokens 2 (command_list (tokens !! 0) (tokens !! 1))
						"version"	-> command_version
						"exit"		-> command_exit
						_		-> run tokens 2 $ exec (token:tokens)

run :: [String] -> Int -> IO () -> IO ()
run tokens n f = if (length tokens) < n then commandLineError "few args" else f

commandLineError :: String -> IO ()
commandLineError message = hPutStrLn stderr $ "Error: " ++ message

exceptionHandling :: IOError -> IO ()
exceptionHandling e = commandLineError $ case e of _	| isAlreadyExistsError e	-> "it already exists"
							| isDoesNotExistError e		-> "it does not exist"
							| isAlreadyInUseError e		-> "it already in use"
							| isFullError e			-> "your device is full"
							| isEOFError e			-> error "eof error"
							| isIllegalOperation e		-> error "your platform not supported in SShell"
							| isPermissionError e		-> "you do not have the permission"
							| otherwise			-> unexceptedException e

command_mkfile :: FilePath -> IO ()
command_mkfile file = do	exists <- doesFileExist file
				if exists
					then commandLineError "that file already exists"
					else (writeFile file "") `catchIOError` exceptionHandling

command_rmfile :: FilePath -> IO ()
command_rmfile file = (removeFile file) `catchIOError` exceptionHandling

command_cpfile :: FilePath -> FilePath -> IO ()
command_cpfile src dst = (copyFileWithMetadata src dst) `catchIOError` exceptionHandling
