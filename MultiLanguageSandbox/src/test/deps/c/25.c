#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
/*
Shifts all characters by 5 positions in alphabetical order. Only letters are replaced, and all letters are in uppercase.
​    Parameters:
​    text (str): The input text to be processed.
​
​    Returns:
​    str: The transformed text with characters shifted by 5 positions.
	>>> shift_characters(NS,BFW,,JAJSYX,TK,NRUTWYFSHJ,FWJ,YMJ,WJXZQY,TK,YWNANFQ,HFZXJX)
	IN WAR, EVENTS OF IMPORTANCE ARE THE RESULT OF TRIVIAL CAUSES
*/
char* shift_characters(char* text) {
    for (int i = 0; text[i] != '\0'; i++) {
        if (isupper(text[i])) {
            // Shift the character by 5 positions, wrapping around if necessary
            text[i] = 'A' + (text[i] - 'A' + 5) % 26;
        }
    }
    return text;
}

// Example usage (not part of the solution)
int main()
{
	char test1[] = "NS BFW, JAJSYX TK NRUTWYFSHJ FWJ YMJ WJXZQY TK YWNANFQ HFZXJX";
    char test2[] = "N BTZQI WFYMJW GJ KNWXY NS F QNYYQJ NGJWNFS ANQQFLJ YMFS XJHTSI NS WTRJ";
    char test3[] = "IFSLJW PSTBX KZQQ BJQQ YMFY HFJXFW NX RTWJ IFSLJWTZX YMFS MJ";

    assert(strcmp(shift_characters(test1), "IN WAR, EVENTS OF IMPORTANCE ARE THE RESULT OF TRIVIAL CAUSES") == 0);
    assert(strcmp(shift_characters(test2), "I WOULD RATHER BE FIRST IN A LITTLE IBERIAN VILLAGE THAN SECOND IN ROME") == 0);
    assert(strcmp(shift_characters(test3), "DANGER KNOWS FULL WELL THAT CAESAR IS MORE DANGEROUS THAN HE") == 0);
	return 0;
}