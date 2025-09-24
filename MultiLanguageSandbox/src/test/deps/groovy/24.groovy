
class IPAddressGenerator {
    static List<String> generateValidIPAddresses(String s) {
        """
        Generate all possible valid IP addresses by inserting '.' in the given string.
        Each part of the IP address must be a number between 0 and 255 and cannot have leading zeros.
        >>> generateValidIPAddresses("25525511135")
        ["255.255.11.135", "255.255.111.35"]
        >>> generateValidIPAddresses("0000")
        ["0.0.0.0"]
        >>> generateValidIPAddresses("101023")
        ["1.0.10.23", "1.0.102.3", "10.1.0.23", "10.10.2.3", "101.0.2.3"]
        """

    List<String> result = []
    
    // Check if the string length is within valid IP address range (4-12 characters)
    if (s.length() < 4 || s.length() > 12) {
        return result
    }
    
    // Try all possible combinations of 3 dots positions
    for (int i = 1; i <= 3; i++) {
        for (int j = i + 1; j <= i + 3; j++) {
            for (int k = j + 1; k <= j + 3; k++) {
                if (k >= s.length()) continue
                
                String part1 = s.substring(0, i)
                String part2 = s.substring(i, j)
                String part3 = s.substring(j, k)
                String part4 = s.substring(k)
                
                // Validate each part
                if (isValidPart(part1) && isValidPart(part2) && 
                    isValidPart(part3) && isValidPart(part4)) {
                    result.add("$part1.$part2.$part3.$part4")
                }
            }
        }
    }
    
    return result
}

// Helper function to validate a single part of the IP address
static boolean isValidPart(String part) {
    // Check for leading zeros (unless the part is "0" itself)
    if (part.length() > 1 && part.startsWith("0")) {
        return false
    }
    
    // Check if the part is a number between 0 and 255
    try {
        int num = Integer.parseInt(part)
        return num >= 0 && num <= 255
    } catch (NumberFormatException e) {
        return false
    }
}
}
// Test cases
def ipAddressGenerator = new IPAddressGenerator()
assert ipAddressGenerator.generateValidIPAddresses("25525511135") == ["255.255.11.135", "255.255.111.35"]
assert ipAddressGenerator.generateValidIPAddresses("0000") == ["0.0.0.0"]
assert ipAddressGenerator.generateValidIPAddresses("101023") == ["1.0.10.23", "1.0.102.3", "10.1.0.23", "10.10.2.3", "101.0.2.3"]
assert ipAddressGenerator.generateValidIPAddresses("255255255255") == ["255.255.255.255"]
assert ipAddressGenerator.generateValidIPAddresses("1111") == ["1.1.1.1"]
assert ipAddressGenerator.generateValidIPAddresses("010010") == ["0.10.0.10", "0.100.1.0"]