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

module SShell.Constant where

import System.IO.Error		(IOError, isAlreadyExistsError, isDoesNotExistError, isAlreadyInUseError, isFullError, isEOFError, isIllegalOperation, isPermissionError)
import Control.Exception	(displayException)

version :: String
version = "1.0.0"

generateErrorMessage :: IOError -> String
generateErrorMessage e = case e of _	| isAlreadyExistsError e	-> "it already exists"
					| isDoesNotExistError e		-> "it does not exist"
					| isAlreadyInUseError e		-> "it is already in use"
					| isFullError e			-> "your device is full"
					| isEOFError e			-> "eof error"
					| isIllegalOperation e		-> error "your platform is not supported in SShell"
					| isPermissionError e		-> "you do not have the permission"
					| otherwise			-> unexceptedException e

unexceptedException :: IOError -> a
unexceptedException e = error $ "an unexcepted exception occured: " ++ (displayException e)
