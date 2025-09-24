is_valid_sudoku() {
    local board=$1
    local -A rowSeen colSeen boxSeen
    
    # Convert string to 2D array
    local -a grid
    IFS=';' read -ra grid <<< "$board"
    
    # Initialize associative arrays for tracking seen numbers
    for ((i=0; i<9; i++)); do
        rowSeen[$i]=""
        colSeen[$i]=""
        boxSeen[$i]=""
    done
    
    # Check each cell in the grid
    for ((row=0; row<9; row++)); do
        IFS=',' read -ra rowValues <<< "${grid[$row]}"
        for ((col=0; col<9; col++)); do
            local num=${rowValues[$col]}
            
            # Skip empty cells (assuming empty cells are represented as 0 or empty string)
            [[ -z $num || $num == 0 ]] && continue
            
            # Check if number is already in current row, column, or 3x3 box
            if [[ ${rowSeen[$row]} =~ $num || \
                  ${colSeen[$col]} =~ $num || \
                  ${boxSeen[$((row/3*3 + col/3))]} =~ $num ]]; then
                return 1  # Invalid Sudoku
            fi
            
            # Mark this number as seen in current row, column, and box
            rowSeen[$row]+=" $num"
            colSeen[$col]+=" $num"
            boxSeen[$((row/3*3 + col/3))]+=" $num"
        done
    done
    
    return 0  # Valid Sudoku
}



# Testing function
test_is_valid_sudoku() {
local validSudoku="5 3 . . 7 . . . .;6 . . 1 9 5 . . .;. 9 8 . . . . 6 .;8 . . . 6 . . . 3;4 . . 8 . 3 . . 1;7 . . . 2 . . . 6;. 6 . . . . 2 8 .;. . . 4 1 9 . . 5;. . . . 8 . . 7 9"
local invalidSudoku="8 3 . . 7 . . . .;6 . . 1 9 5 . . .;. 9 8 . . . . 6 .;8 . . . 6 . . . 3;4 . . 8 . 3 . . 1;7 . . . 2 . . . 6;. 6 . . . . 2 8 .;. . . 4 1 9 . . 5;. . . . 8 . . 7 9"

[[ $(is_valid_sudoku "$validSudoku") == "true" ]] || { echo "Test 1 failed"; exit 1; }
[[ $(is_valid_sudoku "$invalidSudoku") == "false" ]] || { echo "Test 2 failed"; exit 1; }
}

test_is_valid_sudoku