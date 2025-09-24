#include <vector>
#include <unordered_set>
#include <cassert>
#include <vector>
/**
 * Computes the total number of handshakes in a classroom scenario given
 * the sequence of student IDs as they enter the classroom. It assumes
 * that students have IDs ranging from 0 to N-1 and that each student
 * will shake hands only with students already in the classroom with a
 * smaller ID.
 *
 * Examples:
 * >>> countHandshakes(3, {2, 1, 0})
 * 0
 */
using namespace std;

long long countHandshakes(int n, const vector<int>& order) {
    unordered_set<int> present;
    long long total = 0;
    
    for (int id : order) {
        // The number of handshakes is the number of students in 'present' with id < current id
        int count = 0;
        for (int i = 0; i < id; ++i) {
            if (present.find(i) != present.end()) {
                ++count;
            }
        }
        total += count;
        present.insert(id);
    }
    
    return total;
}
int main() {
    assert(countHandshakes(4, {2, 1, 3, 0}) == 2);
    assert(countHandshakes(6, {0, 1, 2, 3, 4, 5}) == 15);
    assert(countHandshakes(3, {1, 2, 0}) == 1);
    assert(countHandshakes(4, {3, 2, 1, 0}) == 0);
    assert(countHandshakes(4, {0, 1, 2, 3}) == 6);
    assert(countHandshakes(6, {5, 4, 3, 2, 1, 0}) == 0);
    assert(countHandshakes(4, {0, 2, 1, 3}) == 5);
    assert(countHandshakes(5, {3, 1, 4, 2, 0}) == 3);
    assert(countHandshakes(4, {1, 0, 3, 2}) == 4);
    assert(countHandshakes(3, {2, 0, 1}) == 1);
    assert(countHandshakes(5, {1, 3, 0, 2, 4}) == 7);
    assert(countHandshakes(5, {4, 3, 2, 1, 0}) == 0);

    return 0;
}