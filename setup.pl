use strict;
use warnings;

print "Paths:\n";
my @paths = <STDIN>;

open(PATH, "> ./data/PATH") or die "Error: could not open path-file\n";
print PATH $_ for @paths;
