func characterForAsciiCode(_ code: Int) -> Character? {
    guard code > 0 && code < 128 else {
        return nil
    }
    return Character(UnicodeScalar(code))
}


func testCharacterForAsciiCode() {
    assert(characterForAsciiCode(65) == "A", "Test Case 1 Failed")
    assert(characterForAsciiCode(97) == "a", "Test Case 2 Failed")
    assert(characterForAsciiCode(48) == "0", "Test Case 3 Failed")
    assert(characterForAsciiCode(35) == "#", "Test Case 4 Failed")
    assert(characterForAsciiCode(90) == "Z", "Test Case 5 Failed")
    assert(characterForAsciiCode(122) == "z", "Test Case 6 Failed")
}

testCharacterForAsciiCode()