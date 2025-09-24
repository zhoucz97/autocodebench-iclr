function blur_image(image::Matrix{Int}, n::Int, m::Int) :: Matrix{Int}
    """
    Apply a blur effect to a 2D image represented by a matrix of integers, where each integer represents the pixel's grayscale value. The blur effect is applied to the inner pixels, keeping the outermost pixels unchanged. Each inner pixel's new grayscale value is the average of its own value and the values of its immediate neighbors (up, down, left, right), rounded to the nearest integer.

    # Arguments
    - `image`: A matrix of integers representing the image's pixels.
    - `n`: The number of rows in the image.
    - `m`: The number of columns in the image.

    # Returns
    - A new matrix representing the blurred image.

    # Examples
    blur_image([100 0 100 0 50; 50 100 200 0 0; 50 50 100 100 200; 100 100 50 50 100], 4, 5)
    # Output: [100 0 100 0 50; 50 80 100 60 0; 50 80 100 90 200; 100 100 50 50 100]
    """
    # Create a copy of the original image to store the blurred result
    blurred = copy(image)
    
    # Iterate over each pixel in the image
    for i in 2:n-1  # Skip the first and last rows
        for j in 2:m-1  # Skip the first and last columns
            # Calculate the average of the pixel and its four neighbors
            total = image[i,j] + image[i-1,j] + image[i+1,j] + image[i,j-1] + image[i,j+1]
            average = round(Int, total / 5)
            blurred[i,j] = average
        end
    end
    
    return blurred
end
@assert blur_image([100 0 100 0 50; 50 100 200 0 0; 50 50 100 100 200; 100 100 50 50 100], 4, 5) == [100 0 100 0 50; 50 80 100 60 0; 50 80 100 90 200; 100 100 50 50 100]
@assert blur_image([255 255; 255 255], 2, 2) == [255 255; 255 255] # Edge case: Small image where no blur is applied
@assert blur_image([10 20 30; 40 50 60; 70 80 90], 3, 3) == [10 20 30; 40 50 60; 70 80 90] # Test with different values
@assert blur_image([10 30; 40 60; 70 90], 3, 2) == [10 30; 40 60; 70 90] # Test with different values