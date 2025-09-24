proc char_to_ascii {char} {
    # Check if the input is exactly one character and not a space
    if {[string length $char] != 1 || [string is space $char]} {
        error "Input must be a single non-space character"
    }
    
    # Convert the character to its ASCII code
    return [scan $char %c]
}


proc check_char_to_ascii {} {
    proc assert_equal {actual expected message} {
        if {$actual != $expected} {
            error "$message. Expected $expected but got $actual"
        } else {
            puts "$message - Passed"
        }
    }

    # Test cases
    assert_equal [char_to_ascii "A"] 65 "Test case 1: 'A' to ASCII"
    assert_equal [char_to_ascii "a"] 97 "Test case 2: 'a' to ASCII"
    assert_equal [char_to_ascii "0"] 48 "Test case 3: '0' to ASCII"
    assert_equal [char_to_ascii "@"] 64 "Test case 4: '@' to ASCII"
}

check_char_to_ascii