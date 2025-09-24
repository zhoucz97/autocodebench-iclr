<?php
/**
    * Converts a number from one base to another.
    * The function accepts three parameters: the number to convert, the base it's currently in, and the base to convert it to.
    * The bases can range from 2 to 36.
    * The number is assumed to be a string to accommodate larger bases and values.
    * Characters in the number may include 0-9 and A-Z, representing values 0 through 35.
    * Input: A string representing the number, an integer for the current base, and an integer for the target base.
    * Output: The number converted to the target base as a string.
    * Example: convertBase("123AB", 16, 10) returns "74667".
*/

function convertBase($number, $fromBase, $toBase) {

    // First, convert the input number from its original base to decimal (base 10)
    $decimal = 0;
    $length = strlen($number);
    
    for ($i = 0; $i < $length; $i++) {
        $char = strtoupper($number[$i]);
        $value = is_numeric($char) ? intval($char) : ord($char) - ord('A') + 10;
        
        if ($value >= $fromBase) {
            throw new InvalidArgumentException("Invalid character '{$char}' for base {$fromBase}");
        }
        
        $decimal = $decimal * $fromBase + $value;
    }
    
    // Now convert the decimal number to the target base
    if ($toBase == 10) {
        return strval($decimal);
    }
    
    $result = "";
    while ($decimal > 0) {
        $remainder = $decimal % $toBase;
        $result = ($remainder < 10 ? strval($remainder) : chr(ord('A') + $remainder - 10)) . $result;
        $decimal = intval($decimal / $toBase);
    }
    
    return $result === "" ? "0" : $result;
}
function main(){
    assert(convertBase('2345678A123', 18, 18) === '2345678A123');
    assert(convertBase('23456', 15, 18) === '114E0');
    assert(convertBase('2345678', 12, 20) === '22B7A4');
    assert(convertBase('12345678', 16, 23) === '21A976L');
    assert(convertBase('3456AB', 25, 21) === '7C2136');
    assert(convertBase('AB1234567', 18, 22) === '22JF0G367');
    
}

main();
?>