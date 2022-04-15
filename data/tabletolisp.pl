#! /usr/bin/perl -w

use strict;

print("(\n");

while(<STDIN>) {
    chop;
    my @line = split(/[\t ]/, $_);
    my $n    = $#line;
    print("(#(");
    for(my $i = 1; $i < $n; $i++) {
	print(" $line[$i]");
    }
#    print(") #(1 -1))\n") if $line[$n] == 1;
#    print(") #(-1 1))\n") if $line[$n] == 0;

    print(") #(1 -1 -1 -1 -1 -1 -1 -1 -1 -1))\n") if $line[$n] == 0;
    print(") #(-1 1 -1 -1 -1 -1 -1 -1 -1 -1))\n") if $line[$n] == 1;
    print(") #(-1 -1 1 -1 -1 -1 -1 -1 -1 -1))\n") if $line[$n] == 2;
    print(") #(-1 -1 -1 1 -1 -1 -1 -1 -1 -1))\n") if $line[$n] == 3;
    print(") #(-1 -1 -1 -1 1 -1 -1 -1 -1 -1))\n") if $line[$n] == 4;
    print(") #(-1 -1 -1 -1 -1 1 -1 -1 -1 -1))\n") if $line[$n] == 5;
    print(") #(-1 -1 -1 -1 -1 -1 1 -1 -1 -1))\n") if $line[$n] == 6;
    print(") #(-1 -1 -1 -1 -1 -1 -1 1 -1 -1))\n") if $line[$n] == 7;
    print(") #(-1 -1 -1 -1 -1 -1 -1 -1 1 -1))\n") if $line[$n] == 8;
    print(") #(-1 -1 -1 -1 -1 -1 -1 -1 -1 1))\n") if $line[$n] == 9;
}

print(")\n");
