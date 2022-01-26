#!/usr/bin/perl
use strict;
use warnings;
my ($data, $n, $i);
$i=0;
print("CHARACTER(LEN=*),PARAMETER :: $ARGV[0] = \&\n");
while (($n = read(STDIN, $data, 1)) != 0) {
    printf("ACHAR(%03d)",ord($data));
    if (!eof(STDIN))   { print("//"); }
    if (++$i % 7 == 0) { print("\& \n"); }
}
print ("\n\n[$i]\n");
