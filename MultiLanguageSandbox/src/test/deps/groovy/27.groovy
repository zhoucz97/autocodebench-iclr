
class VersionComparator {
    static int compareVersions(String version1, String version2) {
        """
        Compare two version numbers version1 and version2.
        Each version number consists of multiple revisions separated by dots.
        Each revision is a sequence of digits.
        Revisions are compared from left to right.
        Leading zeros in each revision are ignored.
        If a version number does not specify a revision at a particular index, it is treated as 0.
        If version1 > version2, return 1.
        If version1 < version2, return -1.
        Otherwise, return 0.
        >>> compareVersions("1.01", "1.001")
        0
        >>> compareVersions("1.0", "1.0.0")
        0
        >>> compareVersions("0.1", "1.1")
        -1
        """

    // Split the version strings into arrays of revisions
    def v1 = version1.split('\\.')
    def v2 = version2.split('\\.')
    
    // Determine the maximum length between the two version arrays
    int maxLength = Math.max(v1.size(), v2.size())
    
    // Compare each revision
    for (int i = 0; i < maxLength; i++) {
        // Get the current revision for each version, or 0 if it doesn't exist
        int num1 = i < v1.size() ? Integer.parseInt(v1[i]) : 0
        int num2 = i < v2.size() ? Integer.parseInt(v2[i]) : 0
        
        // Compare the numbers
        if (num1 > num2) {
            return 1
        } else if (num1 < num2) {
            return -1
        }
    }
    
    // If all revisions are equal
    return 0
}
}
// Test cases
def versionComparator = new VersionComparator()
assert versionComparator.compareVersions("1.01", "1.001") == 0
assert versionComparator.compareVersions("1.0", "1.0.0") == 0
assert versionComparator.compareVersions("0.1", "1.1") == -1
assert versionComparator.compareVersions("1.1", "0.1") == 1
assert versionComparator.compareVersions("1.0.1", "1") == 1
assert versionComparator.compareVersions("1", "1.0.1") == -1
assert versionComparator.compareVersions("1.0.0", "1.0") == 0
assert versionComparator.compareVersions("1.0.0.0", "1.0") == 0
assert versionComparator.compareVersions("1.0.0.1", "1.0") == 1