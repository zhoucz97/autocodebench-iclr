#!/usr/bin/env tclsh

proc quicksort {list} {
    if {[llength $list] <= 1} {
        return $list
    }
    
    set pivot [lindex $list 0]
    set rest [lrange $list 1 end]
    set less {}
    set greater {}
    
    foreach x $rest {
        if {$x < $pivot} {
            lappend less $x
        } else {
            lappend greater $x
        }
    }
    
    return [concat [quicksort $less] [list $pivot] [quicksort $greater]]
}

proc checkQuicksort {} {
    if {[quicksort {3 1 4 1 5 9 2 6 5 3 5}] ne {1 1 2 3 3 4 5 5 5 6 9}} {
        error "Test case 1 Failed"
    }
    if {[quicksort {5 4 3 2 1}] ne {1 2 3 4 5}} {
        error "Test case 2 Failed"
    }
    if {[quicksort {}] ne {}} {
        error "Test case 3 Failed"
    }
    if {[quicksort {1}] ne {1}} {
        error "Test case 4 Failed"
    }
}

checkQuicksort
