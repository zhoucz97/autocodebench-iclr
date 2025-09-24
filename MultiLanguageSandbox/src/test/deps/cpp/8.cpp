#include <bitset>
#include <bits/stdc++.h>
/*
Calculate the Hamming distance between two integers in binary representation.
​    Parameters:
- x (int): The first positive integer (x <= 1,000,000,000).
- y (int): The second positive integer (y <= 1,000,000,000).
​    Returns:
​    int: The Hamming distance between x and y, i.e., the number of bits that need to be flipped
​         to transform x into y in binary.
	>>> hamming_distance(1,2)
	2
*/
int hamming_distance(int x, int y) {
    // XOR the two numbers to get a number where bits are set to 1 where they differ
    int xor_result = x ^ y;
    
    // Count the number of set bits in the XOR result
    int distance = 0;
    while (xor_result != 0) {
        distance += xor_result & 1;  // Check the least significant bit
        xor_result >>= 1;            // Right shift to check the next bit
    }
    
    return distance;
}
int main() {
    assert(hamming_distance(1, 2) == 2); // 01 and 10 have 2 different bits
    assert(hamming_distance(4, 7) == 2); // 100 and 111 have 2 different bits
    assert(hamming_distance(25, 30) == 3); // Additional test: 11001 and 11110 have 3 different bits
    assert(hamming_distance(0, 0) == 0); // Additional test: Same numbers have 0 different bits
    assert(hamming_distance(0xFFFFFFF, 0x0000000) == 28); // Additional test: Max unsigned int and 0 have 32 different bits
    return 0;
}