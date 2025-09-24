proc time_to_buy_tickets {tickets k} {
    set queue [list]
    set n [llength $tickets]
    for {set i 0} {$i < $n} {incr i} {
        lappend queue [list [lindex $tickets $i] $i]
    }
    
    set time 0
    set current_pos 0
    
    while {[llength $queue] > 0} {
        set person [lindex $queue 0]
        set remaining [lindex $person 0]
        set original_pos [lindex $person 1]
        
        # Process the first person
        set remaining [expr {$remaining - 1}]
        incr time
        
        if {$original_pos == $k && $remaining == 0} {
            return $time
        }
        
        # Remove the person from the front
        set queue [lrange $queue 1 end]
        
        if {$remaining > 0} {
            lappend queue [list $remaining $original_pos]
        }
    }
    
    return $time
}


proc check {time_to_buy_tickets} {
if {[time_to_buy_tickets {5 1 1 1} 0] == 8} {
puts "Test case 1 Passed"
} else {
error "Test case 1 Failed"
}

if {[time_to_buy_tickets {2 3 2} 2] == 6} {
puts "Test case 2 Passed"
} else {
error "Test case 2 Failed"
}

if {[time_to_buy_tickets {1 2 3 4 5} 3] == 13} {
puts "Test case 3 Passed"
} else {
error "Test case 3 Failed"
}

if {[time_to_buy_tickets {2 2 2 2 2} 4] == 10} {
puts "Test case 4 Passed"
} else {
error "Test case 4 Failed"
}

if {[time_to_buy_tickets {1 1 1 1 1} 2] == 3} {
puts "Test case 5 Passed"
} else {
error "Test case 5 Failed"
}

if {[time_to_buy_tickets {3 3 3 3 3} 0] == 11} {
puts "Test case 6 Passed"
} else {
error "Test case 6 Failed"
}

if {[time_to_buy_tickets {1 1 1 1 1} 0] == 1} {
puts "Test case 7 Passed"
} else {
error "Test case 7 Failed"
}
}

# Call the check procedure
check time_to_buy_tickets