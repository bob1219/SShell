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

import System.IO.Error		(IOError)
import Control.Exception	(displayException)
import System.IO		(hPutStrLn, stderr)

version :: String
version = "1.0.3"

unexceptedException :: IOError -> a
unexceptedException e = error $ "an unexcepted exception occured: " ++ (displayException e)

commandLineError :: String -> IO ()
commandLineError message = hPutStrLn stderr $ "Error: " ++ message
