
#include <cassert>
#include <cstdio>
/**
 * Given a sequence of n events, each occurring with a periodicity of a_i years, the task is to find
 * the year Y_n when the n-th event occurs. The countdown for event i+1 only starts in the year
 * immediately following the occurrence of event i.
 * Example usage:
 *     apocalypseYear(6, {3,2,4,5,9,18}) returns 36
 */
int apocalypseYear(int n, int signs[]) {
    int year = 0;
    for (int i = 0; i < n - 1; ++i) {
        year += signs[i];
    }
    return year;
}
int main() {
    int arr1[] = {3,2,4,5,9,18};
    assert(apocalypseYear(6, arr1) == 36);
    int arr2[] = {1, 2,3,4,5};
    assert(apocalypseYear(5, arr2) == 5);
    int arr3[] = {1,1,1,1,1};
    assert(apocalypseYear(5, arr3) == 5);
    int arr4[] = {50,30,711,200,503,1006};
    assert(apocalypseYear(6, arr4) == 2012);
    int arr5[] = {1, 2};
    assert(apocalypseYear(2, arr5) == 2);
    int arr6[] = {3, 1, 2};
    assert(apocalypseYear(3, arr6) == 6);
    int arr7[] = {2, 3, 4};
    assert(apocalypseYear(3, arr7) == 4);
    int arr8[] = {1, 2, 3, 4};
    assert(apocalypseYear(4, arr8) == 4);
    int arr9[] = {5, 7, 11, 13};
    assert(apocalypseYear(4, arr9) == 13);
    int arr10[] = {2, 2, 2, 2, 2};
    assert(apocalypseYear(5, arr10) == 10);
    int arr11[] = {6, 10, 15};
    assert(apocalypseYear(3, arr11) == 15);
    int arr12[] = {4, 6, 14};
    assert(apocalypseYear(3, arr12) == 14);
    int arr13[] = {50, 30, 711, 200};
    assert(apocalypseYear(4, arr13) == 800);
    int arr14[] = {1, 1, 1, 1, 1, 1};
    assert(apocalypseYear(6, arr14) == 6);
    int arr15[] = {1000000, 999999};
    assert(apocalypseYear(2, arr15) == 1999998);
    return 0;
}