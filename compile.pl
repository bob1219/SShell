use strict;
use warnings;

chdir("src") or die "failed";
system("ghc Main.hs -Wall -fno-warn-tabs") and die "failed";
chdir("..") or die "failed";
if(-f "bin/sshell.exe") {
	unlink("bin/sshell.exe") or die "failed";
}
rename("src/Main.exe", "bin/sshell.exe") or die "failed";
unlink("src/Main.o", "src/Main.hi", "src/SShell/Constant.o", "src/SShell/Constant.hi", "src/SShell/Command.o", "src/SShell/Command.hi") or die "failed";
