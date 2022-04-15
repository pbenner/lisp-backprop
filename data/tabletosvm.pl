#! /usr/bin/perl -w

use strict;

while(<STDIN>) {
    chop;
    my @line = split(/[\t ]/, $_);
    my $n    = $#line;
    print("$line[$n]");
    for(my $i = 1; $i < $n; $i++) {
	print(" $i:$line[$i]");
    }
    print("\n");
}
