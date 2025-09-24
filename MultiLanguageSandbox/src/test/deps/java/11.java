import java.util.HashMap;
import java.util.Map;
import java.util.*;

class Solution {
    /**
     * The approximate value is calculated by counting the number of common letters
     * and dividing it by the sum of the lengths of the two words.
     *
     * @param word1 The first word.
     * @param word2 The second word.
     * @return The approximate value as a reduced fraction.
     *
     * >>> appx("CAR", "CART")
     * "appx(CAR, CART) = 6/7"
     * >>> appx("TURKEY", "CHICKEN")
     * "appx(TURKEY, CHICKEN) = 4/13"
     * >>> appx("MONEY", "POVERTY")
     * "appx(MONEY, POVERTY) = 1/3"
     * >>> appx("ROUGH", "PESKY")
     * "appx(ROUGH, PESKY) = 0"
     * >>> appx("A", "A")
     * "appx(A, A) = 1"
     */
    public static String appx(String word1, String word2) 
{
        // Count the frequency of each character in both words
        Map<Character, Integer> freq1 = new HashMap<>();
        for (char c : word1.toCharArray()) {
            freq1.put(c, freq1.getOrDefault(c, 0) + 1);
        }
        
        Map<Character, Integer> freq2 = new HashMap<>();
        for (char c : word2.toCharArray()) {
            freq2.put(c, freq2.getOrDefault(c, 0) + 1);
        }
        
        // Calculate the number of common letters
        int common = 0;
        for (char c : freq1.keySet()) {
            if (freq2.containsKey(c)) {
                common += Math.min(freq1.get(c), freq2.get(c));
            }
        }
        
        int sumLengths = word1.length() + word2.length();
        if (sumLengths == 0) {
            return "appx(" + word1 + ", " + word2 + ") = 0";
        }
        
        // Reduce the fraction
        int gcdValue = gcd(common, sumLengths);
        int reducedNum = common / gcdValue;
        int reducedDen = sumLengths / gcdValue;
        
        return "appx(" + word1 + ", " + word2 + ") = " + reducedNum + "/" + reducedDen;
    }
    
    private static int gcd(int a, int b) {
        while (b != 0) {
            int temp = b;
            b = a % b;
            a = temp;
        }
        return a;
    }
    public static void main(String[] args) {
        // Test the function with different test cases
        assert appx("DOG", "CAT").equals("appx(DOG, CAT) = 0/6");
        assert appx("HELLO", "WORLD").equals("appx(HELLO, WORLD) = 3/10");
        assert appx("JAVA", "PYTHON").equals("appx(JAVA, PYTHON) = 0/10");
        System.out.println("All tests passed");
    }
}