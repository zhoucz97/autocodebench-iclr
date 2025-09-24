function find_root_bisection() :: Float64
    """
    Find the root of a polynomial function in the given interval [1.5, 2.4] using the bisection method.
    The polynomial is defined as f(x) = x^5 - 15x^4 + 85x^3 - 225x^2 + 274x - 121. 
    It is known that there is exactly one root in the interval [1.5, 2.4].

    Returns:
    - The root of the polynomial in the interval, rounded to 4 decimal places.

    Example:
    >>> find_root_bisection()
    """
    # Define the polynomial function
    f(x) = x^5 - 15x^4 + 85x^3 - 225x^2 + 274x - 121
    
    # Initial interval
    a = 1.5
    b = 2.4
    
    # Tolerance for stopping criterion (since we need 4 decimal places)
    tolerance = 1e-5  # Slightly smaller than 1e-4 to ensure rounding is correct
    
    # Bisection method
    while (b - a) > tolerance
        c = (a + b) / 2
        if f(c) == 0
            return round(c, digits=4)
        elseif f(a) * f(c) < 0
            b = c
        else
            a = c
        end
    end
    
    # Return the midpoint of the final interval, rounded to 4 decimal places
    root = (a + b) / 2
    return round(root, digits=4)
end
@assert find_root_bisection() ≈ 1.8490