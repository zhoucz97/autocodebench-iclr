function calculate_image_similarity(image1::Matrix{Int}, image2::Matrix{Int}) :: Float64
    """
    Calculate the similarity percentage between two binary images represented as matrices of 0s and 1s.
    The similarity is determined by the percentage of matching pixel values at corresponding positions in both images.
    
    Args:
    image1: A matrix of integers (0 or 1) representing the first binary image.
    image2: A matrix of integers (0 or 1) representing the second binary image.
    
    Returns:
    A Float64 representing the similarity percentage between the two images.
    
    Examples:
    >>> calculate_image_similarity([1 0 1; 0 1 0; 1 0 1], [1 1 0; 0 1 0; 1 0 1])
    77.78 
    
    >>> calculate_image_similarity([1 0; 0 1], [0 1; 1 0])
    0.0
    """
    # Check if the matrices have the same dimensions
    if size(image1) != size(image2)
        error("Images must have the same dimensions")
    end
    
    # Calculate the number of matching pixels
    matching_pixels = sum(image1 .== image2)
    
    # Calculate the total number of pixels
    total_pixels = length(image1)
    
    # Calculate the similarity percentage
    similarity = (matching_pixels / total_pixels) * 100.0
    
    return similarity
end
@assert calculate_image_similarity([1 0 1; 0 1 0; 1 0 1], [1 1 0; 0 1 0; 1 0 1]) ≈ 77.78 
@assert calculate_image_similarity([1 0; 0 1], [0 1; 1 0]) ≈ 0.0
@assert calculate_image_similarity([1 1; 1 1], [1 1; 1 1]) ≈ 100.0
@assert calculate_image_similarity([0 0; 0 0], [1 1; 1 1]) ≈ 0.0