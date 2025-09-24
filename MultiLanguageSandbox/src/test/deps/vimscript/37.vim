
"
" Given a sequence of student IDs entering a classroom, where the IDs range from 0 to N-1, calculate
" the total number of handshakes that occur. Each student will shake hands with every student already in the
" classroom with a smaller ID number. The sequence represents the order in which students enter the
" classroom.
"
" Examples:
"    >>> countHandshakes(3, [2, 1, 0])
"    0

function CountHandshakes(n, order)
    let total_handshakes = 0
    let students_in_classroom = []

    for student_id in a:order
        " Count the number of students in the classroom with smaller IDs
        let count = 0
        for existing_student in students_in_classroom
            if existing_student < student_id
                let count += 1
            endif
        endfor

        " Add the current student to the classroom
        call add(students_in_classroom, student_id)

        " Update the total number of handshakes
        let total_handshakes += count
    endfor

    return total_handshakes
endfunction

" Example usage:
echo CountHandshakes(3, [2, 1, 0])  " Output should be 0

call assert_equal(CountHandshakes(4, [2, 1, 3, 0]), 2)
call assert_equal(CountHandshakes(6, [0, 1, 2, 3, 4, 5]), 15)
call assert_equal(CountHandshakes(3, [1, 2, 0]), 1)
call assert_equal(CountHandshakes(4, [3, 2, 1, 0]), 0)
call assert_equal(CountHandshakes(4, [0, 1, 2, 3]), 6)
call assert_equal(CountHandshakes(6, [5, 4, 3, 2, 1, 0]), 0)
call assert_equal(CountHandshakes(4, [0, 2, 1, 3]), 5)
call assert_equal(CountHandshakes(5, [3, 1, 4, 2, 0]), 3)
call assert_equal(CountHandshakes(4, [1, 0, 3, 2]), 4)
call assert_equal(CountHandshakes(3, [2, 0, 1]), 1)
call assert_equal(CountHandshakes(5, [1, 3, 0, 2, 4]), 7)
call assert_equal(CountHandshakes(5, [4, 3, 2, 1, 0]), 0)

exit(0)