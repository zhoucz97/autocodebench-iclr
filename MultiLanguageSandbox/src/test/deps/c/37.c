#include <stdio.h>
#include <assert.h>
#include <stdio.h>
/*
Given a sequence of student IDs entering a classroom, where the IDs range from 0 to N-1, calculate the total number of handshakes that occur. Each student will shake hands with every student already in the classroom with a smaller ID number. The sequence represents the order in which students enter the classroom.

Examples:
    >>> countHandshakes(3, (2, 1, 0))
    0
*/
long long countHandshakes(int n, int order[]) {
    long long total = 0;
    for (int i = 0; i < n; ++i) {
        int current_id = order[i];
        for (int j = 0; j < i; ++j) {
            if (order[j] < current_id) {
                total++;
            }
        }
    }
    return total;
}
int main() {
    int or1[] = {2, 1, 3, 0};
    assert(countHandshakes(4, or1) == 2);
    int or2[] = {0, 1, 2, 3, 4, 5};
    assert(countHandshakes(6, or2) == 15);
    int order1[] = {1, 2, 0};
    assert(countHandshakes(3, order1) == 1);

    int order2[] = {3, 2, 1, 0};
    assert(countHandshakes(4, order2) == 0);

    int order3[] = {0, 1, 2, 3};
    assert(countHandshakes(4, order3) == 6);

    int order4[] = {5, 4, 3, 2, 1, 0};
    assert(countHandshakes(6, order4) == 0);

    int order5[] = {0, 2, 1, 3};
    assert(countHandshakes(4, order5) == 5);

    int order6[] = {3, 1, 4, 2, 0};
    assert(countHandshakes(5, order6) == 3);

    int order7[] = {1, 0, 3, 2};
    assert(countHandshakes(4, order7) == 4);

    int order8[] = {2, 0, 1};
    assert(countHandshakes(3, order8) == 1);

    int order9[] = {1, 3, 0, 2, 4};
    assert(countHandshakes(5, order9) == 7);

    int order10[] = {4, 3, 2, 1, 0};
    assert(countHandshakes(5, order10) == 0);

    return 0;
}