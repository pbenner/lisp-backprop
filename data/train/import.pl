#! /usr/bin/perl -w

use strict;

my $n    = 100;
my $path = "..";

opendir(DIR,$path) or die "Couldn't open directory: $!";

while (defined(my $file = readdir(DIR))) {
    if ($file =~ /.*.png/) {
	for (my $i = 1; $i <= $n; $i++) {
	    my $out = sprintf("%.3d-$file",$i);
	    # scale & rotate
	    if (rand(1) >= 0.5) {
		my $s = [1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0]->[int(rand(10))];
		my $r = int(rand(360));
		print("$i: $file: Rotate ($r) & Scale ($s)\n");
		system("convert ".$path."/".$file." -type Grayscale -depth 8 -gravity center -background white -rotate $r -crop 64x64+0+0\! -flatten ".$out."\n");
		system("convert ".          $out ." -type Grayscale -depth 8 -gravity center -affine $s,0,0,$s -transform -background white -crop 64x64+0+0\! -flatten ".$out."\n");
	    }
	    # translate & rotate
	    else {
		my $x = int(rand(30)-15);
		my $y = int(rand(30)-15);
		my $r = int(rand(360));
		print("$i: $file: Rotate ($r) & Translate ($x,$y)\n");
		system("convert ".$path."/".$file." -type Grayscale -depth 8 -gravity center -background white -rotate $r -crop 64x64+0+0\! -flatten ".$out."\n");
		system("convert ".          $out ." -type Grayscale -depth 8 -gravity center -affine 1,0,0,1,$x,$y -transform -background white -crop 64x64+0+0\! -flatten ".$out."\n");
	    }
	}
    }
}
closedir(DIR);
