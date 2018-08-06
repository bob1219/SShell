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

module Main (main) where

import System.IO	(hSetBuffering, stdout, stdin)
import SShell.Constant	(version, unexceptedException)
import SShell.Command	(tokenizeCommand, commandProcess)
import System.IO.Error	(catchIOError, isEOFError)

main :: IO ()
main = do	hSetBuffering stdout NoBuffering
		hSetBuffering stdin LineBuffering
		putStrLn $ "SShell Version " ++ version
		putStrLn "Copyright 2018 Daiki Yoshida"
		putChar '\n'

		putStrLn "This program comes with ABSOLUTELY NO WARRANTY."
		putStrLn "This is free software, and you are welcome to redistribute it under certain conditions."
		putChar '\n'

		loop

loop :: IO ()
loop = do	putChar '>'
		tokens <- tokenizeCommand <$> (getLine `catchIOError` (\e ->	if isEOFError e
											then loop
											else unexceptedException e))

		if null tokens
			then loop
			else commandProcess tokens

		putChar '\n'
		loop
