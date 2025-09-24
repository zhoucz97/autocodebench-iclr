#!/usr/bin/env tclsh

package require tcltest 2.0
namespace import ::tcltest::*

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

# 将原来的 checkQuicksort 转换为 tcltest 格式
test checkQuicksort-1 {Test case 1} {
    quicksort {3 1 4 1 5 9 2 6 5 3 5}
} {1 1 2 3 3 4 5 5 5 6 9}

test checkQuicksort-2 {Test case 2} {
    quicksort {5 4 3 2 1}
} {1 2 3 4 5}

test checkQuicksort-3 {Test case 3} {
    quicksort {}
} {}

test checkQuicksort-4 {Test case 4} {
    quicksort {1}
} {1}

# 运行测试
runAllTests
