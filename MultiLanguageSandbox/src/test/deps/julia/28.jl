function rotate_image_clockwise(image::Matrix{Int})
    """
    Rotate a grayscale image represented by an n x m matrix clockwise by 90 degrees.
    
    # Arguments
    - `image`: Matrix{Int} representing the grayscale image. Each element is a pixel intensity ranging from 0 to 255.
    
    # Returns
    - `Matrix{Int}`: The image rotated clockwise by 90 degrees.
    
    # Examples
    ```julia
    rotate_image_clockwise([1 2 3; 4 5 6; 7 8 9])
    # Output: [7 4 1; 8 5 2; 9 6 3]
    """
    # Transpose the matrix
    transposed = transpose(image)
    
    # Reverse each row to get the 90-degree clockwise rotation
    rotated = reverse(transposed, dims=2)
    
    return rotated
end

# Example usage:
image = [1 2 3; 4 5 6; 7 8 9]
rotated_image = rotate_image_clockwise(image)
@assert rotate_image_clockwise([1 2 3; 4 5 6; 7 8 9]) == [7 4 1; 8 5 2; 9 6 3]
@assert rotate_image_clockwise([1 2; 3 4; 5 6]) == [5 3 1; 6 4 2]
@assert rotate_image_clockwise([0 0; 0 0]) == [0 0; 0 0]
@assert rotate_image_clockwise([255;;]) == [255;;]