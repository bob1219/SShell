use strict;
use warnings;
use File::Copy;

print "(1:Take over from other SShell, 2:Setup new SShell)\n";
while(1) {
	print ">";
	chomp(my $select = <STDIN>);
	if($select == "1") {
		&take_over;
		last;
	} elsif($select == "2") {
		&setup;
		last;
	}
}

sub take_over {
	print "old SShell's directory: ";
	chomp(my $directory = <STDIN>);
	copy("$directory/data/PATH", "./data/PATH");
}

sub setup {
	print "Paths:\n";
	my @paths = <STDIN>;
	open(PATHS, "> ./data/PATH") or die "Error: failed open path-file\n";
	print PATHS $_ for @paths;
}
