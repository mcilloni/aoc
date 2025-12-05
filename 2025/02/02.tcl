#!/usr/bin/env tclsh

# AoC 2025 day 02

namespace eval aoc02 {
    proc find-bounds {ranges} {
        if {! [llength $ranges]} {
            return 0
        }

        set upper 0
        set lower [expr { 1 << 64 }]

        foreach range $ranges {
            lassign $range min max

            set upper [expr { max($upper, $max) }]
            set lower [expr { min($lower, $min) }]
        }

        return "$lower $upper"
    }

    proc in-range {n range} {
        lassign $range min max

        return [expr { $n <= $max && $n >= $min }]
    }

    proc load-input {path} {
        set f [open $path r]

        set ranges [lmap range [split [read $f] ","] { split $range "-" }]

        close $f

        return $ranges
    }

    # idea: we generate all numbers that are made of repeated patterns
    # e.g., 11, 1212, 123123, 121212, 123123123, ...
    # we can reduce the solution space significantly by limiting to the tokens that have lengths between
    # the shortest lower range bound and longest upper range bound, i.e. we know that 121212 will never be needed
    # if the ranges are all between 10-87872, for instance
    # we can also reduce the solution space by skipping patterns that are half the length of the largest upper bound,
    # because anything larger would immediately go out of range when repeated the first time
    # this makes the problem vastly faster, even in Tcl
    proc make-seq {range} {
        set name "coro[clock micro]"

        set lambda {{range} {
            proc next-pattern {current maxlen} {
                set nxt [expr {$current + 1}]
                
                if {[string length $nxt] > $maxlen} {
                    return 0
                }

                if {[skip-pattern $nxt]} {
                    tailcall next-pattern $nxt $maxlen
                }

                return $nxt
            }

            # it's useless to generate patterns like 11, 111, 2222, because they're already covered by the single
            # digit patterns already (i.e. 1 generates 11, 111, 1111, etc) so we can skip them and speed up generation
            # a lot
            proc skip-pattern {pattern} {
                if { [string length $pattern] < 2 } {
                    return 0
                }

                set first [string index $pattern 0]

                foreach char [split [string range $pattern 1 end] ""] {
                    if { $char ne $first } {
                        return 0
                    }
                }

                return 1
            }

            yield

            lassign $range min max

            set pattern 1
            set token 11

            set minlen [string length $min]
            set maxlen [string length $max]
            set maxplen [expr { $maxlen / 2 }]

            while {1} {
                if {[string length $token] > $maxlen} {
                    set pattern [next-pattern $pattern $maxplen]

                    if {! $pattern } {
                        return 0
                    }

                    set token "$pattern$pattern"
                }

                if {[string length $token] >= $minlen} {
                    yield $token
                }

                set token "$token$pattern"
            }
        }}
            
        uplevel 1 "coroutine $name apply {$lambda} {$range}"

        return $name
    }

    proc test-id {id} {
        set len [string length $id]
        if {$len % 2} {
            # odd length strings are always valid
            return 1
        }

        set mid [expr {$len / 2}]
        set left [string range $id 0 [expr {$mid - 1}]]
        set right [string range $id $mid end]

        return [expr {$left ne $right}]
    }

    proc test-range {start end} {
        proc test-range-tail {tally cur end} {
            if {$cur > $end} {
                return $tally
            }

            if {![test-id $cur]} {
                set tally [expr {$tally + $cur}]
            }

            tailcall test-range-tail $tally [expr {$cur + 1}] $end
        }

        tailcall test-range-tail 0 $start $end
    }

    proc part1 {ranges} {
        proc test-ranges-tail {sum ranges} {
            if {[llength $ranges] == 0} {
                return $sum
            }

            set ranges [lassign $ranges range]
            lassign $range start end

            tailcall test-ranges-tail [expr {$sum + [test-range $start $end]}] $ranges
        }

        puts "part1: [test-ranges-tail 0 $ranges]"
    }

    proc part2 {ranges} {
        set bounds [find-bounds $ranges]
        
        set ids {}

        set seq [make-seq $bounds]

        while {1} {
            set n [$seq]

            if {!$n} {
                break
            }

            foreach range $ranges {
                if {[in-range $n $range]} {
                    dict set ids $n ""
                }
            }
        }

        set total 0

        foreach id [dict keys $ids] {
            set total [expr { $total + $id }]
        }

        puts "part2: $total"
    }

    proc main {pname argv} {
        if {[llength $argv] != 1} {
            puts stderr "error: wrong number of arguments\nusage: $pname <filename>"
            exit 1
        }

        if {[catch {load-input $argv} ranges]} {
            puts stderr "error opening file: $argv"
            exit 1
        }

        part1 $ranges
        part2 $ranges
    }
}

if {[info script] eq $argv0} {
    aoc02::main $argv0 $argv
}