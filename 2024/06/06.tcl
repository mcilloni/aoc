#!/usr/bin/env tclsh

# Solution to AoC 2024 day 6 in TCL
# Must admit, TCL has been a pleasant surprise so far

namespace eval aoc06 {

    proc dump-grid {grid {path {}} {dots {}} {current {-1 N}}} {
        proc dash-dir {dir} {
            switch $dir {
                N {return "|"}
                E {return "-"}
                S {return "|"}
                W {return "-"}
            }
        }

        proc mark-path {input path} {
            upvar $input iref

            if {[llength $path] == 0 || [matrix dim $iref] eq {0 0}} {
                return
            }

            set w [windows $path]
            set cur {-1 N}
            while {[set points [$w]] != -1} {
                lassign $points cur next
                lassign $cur cpos cdir
                lassign $next npos ndir

                if {$cdir eq $ndir} {
                    set chr [dash-dir $cdir]
                } else {
                    set chr "+"
                }

                matrix set iref {*}$cpos $chr
                set cur $next
            }

            # handle last item
            lassign $cur cpos cdir
            matrix set iref {*}$cpos [dash-dir $cdir]

            #mark the first item with ^
            matrix set iref {*}[lindex [lindex $path 0] 0] ^
        }

        foreach dot $dots {
            matrix set grid {*}$dot O
        }

        mark-path grid $path

        lassign $current cpos dir

        if {$cpos != -1} {
            puts "current position: $cpos, facing $dir"
        }

        foreach row $grid {
            puts [join $row ""]
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

        set start [extract-initial input]
        if {$start eq -1} {
            error "could not find initial position"
        }

        return [list $input $start]
    }

    proc loop-detect {input current dir} {
        set traversed [dict create]

        while {1} {
            set result [next-move $input $current $dir]

            if {$result eq -1} {
                return 0
            }

            if {[dict exists $traversed $result]} {
                return 1
            }

            dict set traversed $result 1

            lassign $result dir current
        }
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

            tailcall move-around $input $nx $ndir [lappend visited [list $nx $ndir]]
        }

        return [move-around $input $start $dir [list [list $start $dir]]]
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

            set chr [matrix index $input {*}$nx]
            
            switch $chr {
                . {
                    return [list $dir $nx]
                }
                \# {
                    # we hit a wall, try the next direction
                    tailcall attempt-move $input $current [lrange $dirs 1 end]
                }
                default {
                    error "malformed input: unexpected character \"$chr\" at $nx"
                }
            }
        }
        
        # try to move forward in the current direction, then in the other directions
        return [attempt-move $input $current [next-dirs $dir]]
    }

    proc part2 {input start} {
        set visited [navigate $input $start N]

        dump-grid $input $visited

        set matches [dict create]

        set w [windows $visited]
        set cpos ""
        while {[set points [$w]] != -1} {
            lassign $points cur next
            set npos [lindex $next 0]

            if {[try-block-at $input $npos $start]} {
                dict set matches $npos 1
            }

            set cpos $npos
        }

        # finally, try the last position
        if {[try-block-at $input $cpos $start]} {
            dict set matches $cpos 1
        }

        puts "[dict size $matches] matches: [dict keys $matches]"

        dump-grid $input {} [dict keys $matches]
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

    proc try-block-at {input pos start} {
        # if there's a block already, don't do anything
        if {[matrix index $input {*}$pos] eq "#"} {
            return 0
        }

        # set a block at the target position
        matrix set input {*}$pos "#"

        # probe if the guard now loops
        # the guard always starts pointing at north btw
        return [loop-detect $input $start N]
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

    proc windows {lst} {
        set name "coro[clock micro]"

        set lambda {{lst} {
            yield

            set len [llength $lst]

            for {set i 0} {$i < $len} {incr i} {
                set j [expr {$i + 1}]

                if {$j >= $len} {
                    break
                }

                yield [lrange $lst $i $j]
            }

            return -1
        }}
        
        uplevel 1 "coroutine $name apply {$lambda} {$lst}"

        return $name
    }

    proc main {pname argv} {
        if {[llength $argv] != 1} {
            puts stderr "error: wrong number of arguments\nusage: $pname <filename>"
            exit 1
        }

        if {[catch {load-input $argv} result]} {
            puts stderr "error opening file: $argv"
            exit 1
        }

        part2 {*}$result
    }
}

if {[info script] eq $argv0} {
    aoc06::main $argv0 $argv
}