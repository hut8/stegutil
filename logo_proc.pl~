#!/usr/bin/perl
use strict;
use warnings;
my ($data, $n, $i);
open(my $fh, '<', 'logo_final') or die $!;
$i=0;
while (($n = read($fh, $data, 1)) != 0) {
    printf("ACHAR(%03d),",ord($data));
    if (++$i % 7 == 0) { print "\$ \n"; }
}
print "\n\n[$i]\n";
