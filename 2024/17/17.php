#!/usr/bin/env php
<?php declare(strict_types=1);

# AoC 2024 Day 17

// My input program
// 2,4,1,1,7,5,1,5,0,3,4,4,5,5,3,0
// can be converted to the following SSA code:
/*
do {
    $v1 = $a;
    $v2 = $v1 & 0b111;
    $v3 = $v2 ^ 1;
    $v4 = ($v1 >> $v3) & 0b111;
    $v5 = $v2 ^ 4;
    $v6 = $v5 ^ $v4;
    array_push($out, $v6); 
    $v7 = $v1 >> 3;
    
    $a = $v7;
} while ($a !== 0);
*/
// I've optimised it by 
// - removing unnecessary cruft
// - simplifying a few XORs
// - propagating the and over the xor (A)
// note that:
// - a is read 3 bits at a time
// - b and c are useless and can be converted to local variables
// - the result v6 is the xor of v5 (depends on last 3 bits of a) and v4 (depends on all of a)
// in general these simplifications are not necessary, but help a lot understanding how to optimise
// this search

// Idea: instead of bruteforcing 2^64 values (that's a lot, even for a speedy PC boi), we can
// reach a simple observation: for the last value, $a will have:
// - at least one of the last 3 bits non-zero
// - all the other bits are zeroes
// so in order to find $a at the last step, it's as simple as trying all sequences from 0 to 7
// until we find a sequence for which the program output the expected last value
// then, we can just shift right by 3 and apply the algorithm again, but by doing $a << 3 | $i instead.
// This ends up being a nice recursive algorithm where every step gets a sliced input array and the tentative $a value
// The for loops allow for backtracking for those cases when some combination doesn't work (the & 0b111 cause the solutions
// to be potentially many)
// by iterating from 0 to 0b111, we ensure that the solution will be as small as possible

const INPUT = [2,4,1,1,7,5,1,5,0,3,4,4,5,5,3,0];

function do_round(int $a): int {
    $v1 = $a;
    $v2 = $v1 & 0b111;
    $v3 = $v2 ^ 1;
    $v4 = ($v1 >> $v3) & 0b111;
    $v5 = $v2 ^ 4;
    return $v5 ^ $v4;
}

function probe_three(int $a, int $expected): ?int {
    $a <<= 3;

    for ($i = 0; $i <= 0b111; ++$i) {
        $tentative = $a | $i;
        if (do_round($tentative) === $expected) {
            return $tentative;
        }
    }

    return null;
}

function dectrec($a, $input): ?int {
    if (empty($input)) {
        return $a;
    }

    $a <<= 3;

    $expected = array_pop($input);

    for ($i = 0; $i <= 0b111; ++$i) {
        $tentative = $a | $i;
        if (do_round($tentative) === $expected) {
            $res = dectrec($tentative, $input);
            if ($res !== null) {
                return $res;
            }
        }
    }

    return null;
}    

function dect(): ?int {
    return dectrec(0, INPUT);
}

function main() {
    $a = dect();

    if ($a === null) {
        echo "No solution found\n";
    } else {
        echo "part 2: $a" . PHP_EOL;
    }
}

main();

// This solves the problem in 7 ms instead of 500+ million years, which is a nice speedup if you ask me
// If someone had a lot of time, they could probably find a way to compile any input into a similar program
// because I suspect all inputs will out + jump 0 at the end in a similar way, but that's probably not worth the effort
// and utterly out of scope