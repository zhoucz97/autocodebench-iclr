
/**
 * Processes a given 2D array representing an image, applying a series of operations.
 * Each operation is represented by a character:
 * 'A' - Rotate the image 90 degrees clockwise.
 * 'B' - Rotate the image 90 degrees counterclockwise.
 * 'C' - Flip the image horizontally.
 * 'D' - Flip the image vertically.
 *
 * @param image A 2D array of integers representing the image's pixel grayscale values.
 * @param operations A string of characters representing the operations to be applied in sequence.
 * @return A 2D array of integers representing the processed image.
 *
 * Example:
 * >>> processImage(arrayOf(arrayOf(10, 0, 10), arrayOf(100, 100, 10)), "AC")
 *     arrayOf(arrayOf(10, 100), arrayOf(0, 100), arrayOf(10, 10))
 */
fun processImage(image: Array<Array<Int>>, operations: String): Array<Array<Int>> 
{
    var currentImage = image.map { it.clone() }.toTypedArray()
    
    for (op in operations) {
        when (op) {
            'A' -> currentImage = rotateClockwise(currentImage)
            'B' -> currentImage = rotateCounterClockwise(currentImage)
            'C' -> currentImage = flipHorizontal(currentImage)
            'D' -> currentImage = flipVertical(currentImage)
        }
    }
    
    return currentImage
}

private fun rotateClockwise(image: Array<Array<Int>>): Array<Array<Int>> {
    val n = image.size
    val m = image[0].size
    val rotated = Array(m) { IntArray(n) }
    
    for (i in 0 until n) {
        for (j in 0 until m) {
            rotated[j][n - 1 - i] = image[i][j]
        }
    }
    
    return rotated
}

private fun rotateCounterClockwise(image: Array<Array<Int>>): Array<Array<Int>> {
    val n = image.size
    val m = image[0].size
    val rotated = Array(m) { IntArray(n) }
    
    for (i in 0 until n) {
        for (j in 0 until m) {
            rotated[m - 1 - j][i] = image[i][j]
        }
    }
    
    return rotated
}

private fun flipHorizontal(image: Array<Array<Int>>): Array<Array<Int>> {
    val n = image.size
    val m = image[0].size
    val flipped = Array(n) { IntArray(m) }
    
    for (i in 0 until n) {
        for (j in 0 until m) {
            flipped[i][m - 1 - j] = image[i][j]
        }
    }
    
    return flipped
}

private fun flipVertical(image: Array<Array<Int>>): Array<Array<Int>> {
    val n = image.size
    val m = image[0].size
    val flipped = Array(n) { IntArray(m) }
    
    for (i in 0 until n) {
        for (j in 0 until m) {
            flipped[n - 1 - i][j] = image[i][j]
        }
    }
    
    return flipped
}
fun main() {
    val image1 = arrayOf(arrayOf(10, 0, 10), arrayOf(100, 100, 10))
    val result1 = processImage(image1, "AC")
    check(result1.contentDeepEquals(arrayOf(arrayOf(10, 100), arrayOf(0, 100), arrayOf(10, 10))))

    val image2 = arrayOf(arrayOf(1, 2), arrayOf(3, 4))
    val result2 = processImage(image2, "A")
    check(result2.contentDeepEquals(arrayOf(arrayOf(3, 1), arrayOf(4, 2))))

    val result3 = processImage(image2, "B")
    check(result3.contentDeepEquals(arrayOf(arrayOf(2, 4), arrayOf(1, 3))))

    val result4 = processImage(image2, "BD")
    check(result4.contentDeepEquals(arrayOf(arrayOf(1, 3), arrayOf(2, 4))))

}

main()