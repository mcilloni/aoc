#!/usr/bin/env tclsh

namespace eval aoc06 {

    proc dump-grid {grid {current {-1 N}} {visited {}}} {
        proc caret-dir {dir} {
            switch $dir {
                N {return "^"}
                E {return ">"}
                S {return "v"}
                W {return "<"}
            }
        }

        lassign $current cpos dir

        puts "current position: $cpos, facing $dir"

        set i 0

        foreach row $grid {
            set j 0

            foreach cell $row {
                set p [list $i $j]

                if {$cpos eq $p} {
                    puts -nonewline [caret-dir $dir]
                } elseif {[dict exists $visited $p]} {
                    puts -nonewline "X"
                } else {
                    puts -nonewline $cell
                }

                incr j
            }

            puts ""

            incr i
        }
    }

    proc extract-initial {input} {
        upvar $input iref

        set pos [matrix search $iref ^]

        if {$pos eq -1} {
            return -1
        }

        # remove the initial position from the matrix
        matrix set iref {*}$pos .

        return $pos
    }

    proc find-in {input value {current {0 0}} {dim ""}} {
        if {$dim eq ""} {
            set dim [matrix dim $input]
        }

        set it [matrix index $input {*}$current]
        if {$it eq $value} {
            return $current
        }

        set next [point next $current $dim]
        if {$next eq -1} {
            return -1
        }

        tailcall find-in $input $value [point next $current $dim] $dim
    }

    proc load-input {path} {
        set f [open $path r]

        set input [lmap line [split [read $f] \n] { split $line "" }]

        close $f

        return $input
    }

    proc matrix {subcmd m args} {
        switch $subcmd {
            contains {
                if {[llength $args] != 2} {
                    error {wrong number of args: should be "matrix contains m i j"}
                }

                return [expr {[matrix index $m {*}$args] ne ""}]
            }
            dim {
                if {[llength $args] != 0} {
                    error {wrong number of args: should be "matrix dim m"}
                }

                # no validation for now
                return [list [llength $m] [llength [lindex $m 0]]]
            }
            index {
                if {[llength $args] != 2} {
                    error {wrong number of args: should be "matrix get m row col"}
                }

                return [lindex $m [lindex $args 0] [lindex $args 1]]
            }
            search {
                if {[llength $args] != 1} {
                    error {wrong number of args: should be "matrix search m value"}
                }

                return [find-in $m $args]
            }
            set {
                if {[llength $args] != 3} {
                    error {wrong number of args: should be "matrix set m row col value"}
                }

                upvar $m mref

                return [lset mref [lindex $args 0] [lindex $args 1] [lindex $args 2]]
            }
            default {
                error "unknown subcommand: $subcmd"
            }
        }
    }

    proc navigate {input start dir} {
        proc move-around {input current dir visited} {
            set result [next-move $input $current $dir]

            if {$result eq -1} {
                return $visited
            }

            lassign $result ndir nx

            # puts "at $nx, visited [llength [dict keys $visited]] cells"

            # dump-grid $input [list $nx $ndir] $visited

            # puts ""

            tailcall move-around $input $nx $ndir [dict set visited $nx 1]
        }

        return [move-around $input $start $dir [dict create $start 1]]
    } 

    proc next-dirs {dir} {
        for {set i 0} {$i < 4} {incr i} {
            lappend dirs $dir
            set dir [turn $dir]
        }

        return $dirs
    }

    proc next-move {input current dir} {
        proc attempt-move {input current dirs} {
            if {[llength $dirs] == 0} {
                # we got stuck. This is probably an error in the input
                return -1 
            }

            set dir [lindex $dirs 0]
            set dim [matrix dim $input]
            set nx [point move $current $dir $dim]

            if {$nx eq -1} {
                # we ended outside of the grid
                return -1
            }

            switch [matrix index $input {*}$nx] {
                . {
                    return [list $dir $nx]
                }
                \# {
                    # we hit a wall, try the next direction
                    tailcall attempt-move $input $current [lrange $dirs 1 end]
                }
                default {
                    error {malformed input: unexpected character at $nx}
                }
            }
        }
        
        # try to move forward in the current direction, then in the other directions
        return [attempt-move $input $current [next-dirs $dir]]
    }

    proc part1 {input} {
        set cur [extract-initial input]
        set visited [navigate $input $cur N]

        dump-grid $input [list -1 N] $visited

        puts "part1: [dict size $visited]"
    }

    proc point {subcmd p args} {
        switch $subcmd {
            add {
                if {[llength $args] != 2} {
                    error {wrong number of args: should be "point add p q dim"}
                }

                lassign $p i j
                lassign [lindex $args 0] i' j'
                lassign [lindex $args 1] m n

                set x [expr {$i + ${i'}}]
                set y [expr {$j + ${j'}}]

                if {$x < 0 || $x >= $m || $y < 0 || $y >= $n} {
                    return -1
                }

                return [list $x $y]
            }
            move {
                if {[llength $args] != 2} {
                    error {wrong number of args: should be "point move p dir"}
                }

                set dir [lindex $args 0]
                set dim [lindex $args 1]

                return [point add $p [versor $dir] $dim]
            }
            next {
                if {[llength $args] != 1} {
                    error {wrong number of args: should be "point next p dim"}
                }

                lassign $p i j
                lassign {*}$args m n
                
                incr j

                if {$j >= $n} {
                    incr i
                    set j 0
                }

                if {$i >= $m} {
                    return -1
                }

                return [list $i $j]
            }
            x {
                return [lindex $p 0]
            }
            y {
                return [lindex $p 1]
            }
            default {
                error "unknown subcommand: $subcmd"
            }
        }
    }

    proc turn {dir} {
        switch $dir {
            N {return E}
            E {return S}
            S {return W}
            W {return N}
        }
    }

    proc versor {dir} {
        switch $dir {
            N {return {-1 0}}
            E {return {0 1}}
            S {return {1 0}}
            W {return {0 -1}}
            default {error "unknown direction: $dir"}
        }
    }

    proc main {pname argv} {
        if {[llength $argv] != 1} {
            puts stderr "error: wrong number of arguments\nusage: $pname <filename>"
            exit 1
        }

        if {[catch {load-input $argv} input]} {
            puts stderr "error opening file: $argv"
            exit 1
        }

        part1 $input
    }
}

if {[info script] eq $argv0} {
    aoc06::main $argv0 $argv
}