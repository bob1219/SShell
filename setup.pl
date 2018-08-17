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
	copy("$directory/data/PATH", "./data/PATH") or &error("failed copy path-file");
}

sub setup {
	open(my $fh, "<", "./data/PATH") or &error("failed open path-file");
	print "Paths:\n";
	print $fh $_ while <STDIN>;
}

sub error {
	my ($message) = @_;
	die "Error: $message\n";
}
