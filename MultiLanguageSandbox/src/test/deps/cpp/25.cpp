#include <string>
#include <bits/stdc++.h>
/*
Shifts all characters by 5 positions in alphabetical order. Only letters are replaced,
and all letters are in uppercase.

Parameters:
text (std::string&): The input text to be processed, modified in place.

Returns:
std::string&: The transformed text with characters shifted by 5 positions.

>>> shift_characters("NS BFW, JAJSYX TK NRUTWYFSHJ FWJ YMJ WJXZQY TK YWNANFQ HFZXJX")
"IN WAR, EVENTS OF IMPORTANCE ARE THE RESULT OF TRIVIAL CAUSES"
*/
using namespace std;

string& shift_characters(string& text) {
    for (char& c : text) {
        if (c >= 'A' && c <= 'Z') {
            c = 'A' + (c - 'A' - 5 + 26) % 26;
        }
    }
    return text;
}
int main()
{
    std::string test1 = "NS BFW, JAJSYX TK NRUTWYFSHJ FWJ YMJ WJXZQY TK YWNANFQ HFZXJX";
    std::string test2 = "N BTZQI WFYMJW GJ KNWXY NS F QNYYQJ NGJWNFS ANQQFLJ YMFS XJHTSI NS WTRJ";
    std::string test3 = "IFSLJW PSTBX KZQQ BJQQ YMFY HFJXFW NX RTWJ IFSLJWTZX YMFS MJ";

    assert(shift_characters(test1) == "IN WAR, EVENTS OF IMPORTANCE ARE THE RESULT OF TRIVIAL CAUSES");
    assert(shift_characters(test2) == "I WOULD RATHER BE FIRST IN A LITTLE IBERIAN VILLAGE THAN SECOND IN ROME");
    assert(shift_characters(test3) == "DANGER KNOWS FULL WELL THAT CAESAR IS MORE DANGEROUS THAN HE");
    return 0;
}