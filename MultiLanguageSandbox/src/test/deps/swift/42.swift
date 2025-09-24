func asciiCode(of character: Character) -> Int {
    return Int(character.unicodeScalars.first!.value)
}


func testAsciiCode() {
    assert(asciiCode(of: "A") == 65, "Test Case 1 Failed")
    assert(asciiCode(of: "a") == 97, "Test Case 2 Failed")
    assert(asciiCode(of: "0") == 48, "Test Case 3 Failed")
    assert(asciiCode(of: "#") == 35, "Test Case 4 Failed")
    assert(asciiCode(of: "Z") == 90, "Test Case 5 Failed")
    assert(asciiCode(of: "z") == 122, "Test Case 6 Failed")
}

testAsciiCode()