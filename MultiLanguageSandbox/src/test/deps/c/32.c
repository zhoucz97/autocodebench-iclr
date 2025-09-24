
#include <assert.h>
#include <stdio.h>
/*
Given a sequence of n events, each occurring with a periodicity of a_i years, the problem is to find the year Y_n when the n-th event occurs, under the condition that the countdown for event i+1 can only commence in the year immediately following the occurrence of event i. 
    >>> apocalypseYear(6, (int[]){3,2,4,5,9,18})
    36
*/
int apocalypseYear(int n, int signs[]) {
    int year = 0;
    for (int i = 0; i < n; i++) {
        year += signs[i];
    }
    return year;
}
int main() {
    assert(apocalypseYear(6, (int[]){3,2,4,5,9,18}) == 36);
    assert(apocalypseYear(5, (int[]){1, 2,3,4,5}) == 5);
    assert(apocalypseYear(5, (int[]){1,1,1,1,1}) == 5);
    assert(apocalypseYear(6, (int[]){50,30,711,200,503,1006}) == 2012);
    assert(apocalypseYear(2, (int[]){1, 2}) == 2);
    assert(apocalypseYear(3, (int[]){3, 1, 2}) == 6);
    assert(apocalypseYear(3, (int[]){2, 3, 4}) == 4);
    assert(apocalypseYear(4, (int[]){1, 2, 3, 4}) == 4);
    assert(apocalypseYear(4, (int[]){5, 7, 11, 13}) == 13);
    assert(apocalypseYear(5, (int[]){2, 2, 2, 2, 2}) == 10);
    assert(apocalypseYear(3, (int[]){6, 10, 15}) == 15);
    assert(apocalypseYear(3, (int[]){4, 6, 14}) == 14);
    assert(apocalypseYear(4, (int[]){50, 30, 711, 200}) == 800);
    assert(apocalypseYear(6, (int[]){1, 1, 1, 1, 1, 1}) == 6);
    assert(apocalypseYear(2, (int[]){1000000, 999999}) == 1999998);
    return 0;
}