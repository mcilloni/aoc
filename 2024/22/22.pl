#!/usr/bin/env perl

# AoC 2024 Day 22
# This algorithm is a bit slow (10s on my computer), but correct. The problem is extremely straightforward: 
# we advance all entries in the input file 2000 times while collecting [delta, value] pairs.
# The last entry is part 1. For part 2, we weight the quadruplets of deltas (i.e. we assign to each quadruplet the value
# of the last entry in the quadruplet), and discard quadruplets that are unreachable because they have shadowed by an
# earlier one. Finally, iterate over all quadruplets and find the best one. 

use strict;
use utf8;
use warnings;
use v5.40;

use Data::Dumper;
use Getopt::Long;
use Pod::Usage;

use constant {
    MODULO => 16777216,
    ITERATIONS => 2000,
};

sub askey(@args) {
    join ',', @args
}

sub step($input) {
    my $v1 = $input << 6;
    my $v2 = $v1 ^ $input;
    my $v3 = $v2 % MODULO;

    my $v4 = $v3 >> 5;
    my $v5 = $v4 ^ $v3;
    my $v6 = $v5 % MODULO;

    my $v7 = $v6 << 11;
    my $v8 = $v7 ^ $v6;
    my $v9 = $v8 % MODULO;

    return $v9;
}

sub advance($input, $iters = ITERATIONS) {
    my @deltas;
    my $prev;

    for (1..$iters) {
        my $value = $input % 10;

        if (defined $prev) {
            push @deltas, [$value - $prev, $value];
        }

        $prev = $value;
        
        $input = step($input);
    }

    my $last = $input % 10;
    push @deltas, [$last - $prev, $last];

    return $input, \@deltas;
}

sub find_best($quadruplets) {
    my $best = 0;
    my $best_key;
    my %done;

    foreach my $set (@$quadruplets) {
        foreach my $qplet (keys %$set) {
            next if exists $done{$qplet};

            my $sum = sum_all($quadruplets, $qplet);

            if ($sum > $best) {
                $best = $sum;
                $best_key = $qplet;
            }

            $done{$qplet} = 1;
        }            
    }

    return ($best_key, $best);
}

sub parse_input($fname) {
    open my $fh, '<', $fname or die "error: $!\n";
    
    my @lines = map { chomp; int $_ } <$fh> or die "error: $!\n";

    close $fh;

    return \@lines;
}

sub sum_all($quadruplets, $qplet) {
    my $sum = 0;

    $sum += $_->{$qplet} // 0 foreach @$quadruplets;

    return $sum;
} 

sub weight_quadruplets($deltas) {
    my %weights;

    my $len = scalar @$deltas;
    for (my $i = 0; $i < $len - 3; ++$i) {
        my @slice = $deltas->@[$i..$i+3];

        my $key = askey map { $_->[0] } @slice;
        my $value = $slice[-1]->[1];

        # if there's already a given pattern at an earlier index, then it's unreachable because the earlier index will 
        # always get picked first
        $weights{$key} = $value unless exists $weights{$key};
    }

    return \%weights;
}

sub main {
    my $help = 0;

    GetOptions (
        'h|help' => \$help
    ) or pod2usage(2);

    pod2usage(0) if $help;

    if (scalar @ARGV != 1) {
        say STDERR "error: wrong number of arguments";
        pod2usage(2);
    }

    my $sum = 0;
    my @quadruplets;

    foreach my $input(@{ parse_input($ARGV[0]) }) {
        my ($result, $deltas) = advance($input);

        $sum += $result;
        push @quadruplets, weight_quadruplets($deltas);
    }

    say "part 1: $sum";

    my ($best_key, $best) = find_best(\@quadruplets);

    say "part 2: ($best_key), $best";
}

main unless caller;

__END__

=head1 NAME

22 - AoC 2024 Day 22

=head1 SYNOPSIS

22 [options] FILE

Options:

    -h|help           show this help message
    FILE              input file
=cut
