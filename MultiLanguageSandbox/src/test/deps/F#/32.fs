
let validateISBN (isbnInput: string) : string =
    // Remove all separators from the input
    let cleaned = isbnInput.Replace("-", "")
    
    // Check if the cleaned string has exactly 10 characters
    if cleaned.Length <> 10 then
        // If not, we'll assume the check digit is wrong and calculate the correct one
        let digits = isbnInput.ToCharArray() |> Array.filter (fun c -> c <> '-')
        if digits.Length <> 10 then
            // If we can't get exactly 10 digits after removing separators, return the input
            isbnInput
        else
            // Calculate the correct check digit
            let sum = 
                digits 
                |> Array.take 9 
                |> Array.mapi (fun i c -> 
                    match System.Char.IsDigit(c) with
                    | true -> int(c) - int('0') * (i + 1)
                    | false -> 0) // This case shouldn't happen if input is valid
                |> Array.sum
            
            let checkDigit = sum % 11
            let correctCheckDigit = 
                if checkDigit = 10 then 'X'
                else char (checkDigit + int('0'))
            
            // Replace the last character with the correct check digit
            let chars = isbnInput.ToCharArray()
            chars.[chars.Length - 1] <- correctCheckDigit
            new string(chars)
    else
        // Validate the check digit
        let digits = cleaned.ToCharArray()
        let sum = 
            digits 
            |> Array.take 9 
            |> Array.mapi (fun i c -> 
                match System.Char.IsDigit(c) with
                | true -> int(c) - int('0') * (i + 1)
                | false -> 0) // This case shouldn't happen if input is valid
            |> Array.sum
        
        let expectedCheckDigit = sum % 11
        let actualCheckDigit = 
            match digits.[9] with
            | 'X' -> 10
            | c when System.Char.IsDigit(c) -> int(c) - int('0')
            | _ -> -1 // Invalid character
        
        if actualCheckDigit = expectedCheckDigit then
            "Right"
        else
            // Calculate the correct check digit
            let correctCheckDigit = 
                if expectedCheckDigit = 10 then 'X'
                else char (expectedCheckDigit + int('0'))
            
            // Replace the last character with the correct check digit
            let chars = isbnInput.ToCharArray()
            chars.[chars.Length - 1] <- correctCheckDigit
            new string(chars)


let checkISBNValidation () =
    assert (validateISBN("0-670-82162-4") = "Right")
    assert (validateISBN("0-123-45678-9") = "Right")
    assert (validateISBN("0-670-82162-0") = "0-670-82162-4")
    assert (validateISBN("1-234-56789-5") = "1-234-56789-X")
    assert (validateISBN("9-876-54321-1") = "9-876-54321-0")
    assert (validateISBN("5-555-55555-5") = "Right")

checkISBNValidation()