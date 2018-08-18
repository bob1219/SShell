SShell
======
SShell is a shell project.

Development Environment
-----------------------
* Programming Language: Haskell
* Operating System: Microsoft Windows 10
* Compiler: Glasgow Haskell Compiler (GHC) Version 8.4.3

Install
-------
If you using Microsoft Windows, you can use default executable file, it's bin/sshell.exe.  
If else, you can use compile.pl (`perl compile.pl`), but you should install perl and GHC.  
If you do not have perl, and you do not want to install perl, you should manually compile. source code is in "src" directory.
After install, if you want to set paths first, you can use setup.pl (`perl setup.pl`), but you should install perl.

Commands
--------
* mkfile
	* Description: Make a file
	* Usage: `mkfile [file]`

* rmfile
	* Description: Remove a file
	* Usage: `rmfile [file]`

* cpfile
	* Description: Copy a file
	* Usage: `cpfile [src] [dst]`

* renfile
	* Description: Rename a file
	* Usage: `renfile [src] [dst]`

* mkdir
	* Description: Make a directory
	* Usage: `mkdir [dir]`

* rmdir
	* Description: Remove a directory
	* Usage: `rmdir [dir]`

* cpdir
	* Description: Copy a directory
	* Usage: `cpdir [src] [dst]`

* rendir
	* Description: Rename a directory
	* Usage: `rendir [src] [dst]`

* view
	* Description: View a file
	* Usage: `view [file]`

* chcwd
	* Description: Change current working directory
	* Usage: `chcwd [dir]`

* pcwd
	* Description: Print current working directory
	* Usage: `pcwd`

* path list
	* Description: List paths
	* Usage: `path list`

* path clear
	* Description: Clear paths
	* Usage: `path clear`

* path add
	* Descritption: Addition a path to paths
	* Usage: `path add [dir]`

* path del
	* Description: Delete a path from paths
	* Usage: `path del [n]`

* list
	* Description: Print files and directories in a directory
	* Usage: `list [dir]`

* version
	* Description: Print current version
	* Usage: `version`

* exit
	* Description: Exit SShell
	* Usage: `exit`

Files
-----
* .git/: Repository
* data/: datas
* src/: source codes
* LICENSE: GNU General Public License
* README.md: Readme
* compile.pl: compile script
* setup.pl: setup script

History
-------
* 1.0.0 (August 9th, 2018)
	* First Version

* 1.0.1 (August 9th, 2018)
	* Bugfix path feature

* 1.0.2 (August 16th, 2018)
	* Modify an error message

* 1.0.3 (August 17th, 2018)
	* Bugfix invalid-character-alignment-bug
	* Addition alert to denger feature
	* Modify view command and list command path-list command
	* Modify cpfile command and renfile command and cpdir command and rendir command

* 1.0.4 (August 17th, 2018)
	* Delete invalid-character-alignment error

* 1.0.5 (August 18th, 2018)
	* Modify rmfile command and rmdir command
