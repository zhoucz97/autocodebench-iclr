proc ascii_to_char {ascii_code} {
    # Ensure the ASCII code is within the range of visible characters (0 < ascii_code < 128)
    if {$ascii_code <= 0 || $ascii_code >= 128} {
        error "ASCII code must be between 1 and 127 (inclusive)."
    }
    
    # Convert the ASCII code to a character using the format command
    return [format %c $ascii_code]
}


proc check_ascii_to_char {} {
    proc assert_equal {actual expected message} {
        if {$actual ne $expected} {
            error "$message. Expected $expected but got $actual"
        } else {
            puts "$message - Passed"
        }
    }

    # Test cases
    assert_equal [ascii_to_char 65] "A" "Test case 1: ASCII to char for 65"
    assert_equal [ascii_to_char 97] "a" "Test case 2: ASCII to char for 97"
    assert_equal [ascii_to_char 48] "0" "Test case 3: ASCII to char for 48"
    assert_equal [ascii_to_char 64] "@" "Test case 4: ASCII to char for 64"
}

check_ascii_to_char