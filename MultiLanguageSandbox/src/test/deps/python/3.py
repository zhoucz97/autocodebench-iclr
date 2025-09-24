

def truncate_number(number: float) -> float:
    """ Given a positive floating point number, it can be decomposed into
    and integer part (largest integer smaller than given number) and decimals
    (leftover part always smaller than 1).
    
    Return the decimal part of the number.
    >>> truncate_number(3.5)
    0.5
    """
    return number - int(number)
def check(truncate_number):
    assert truncate_number(3.5) == 0.5
    assert abs(truncate_number(1.33) - 0.33) < 1e-6
    assert abs(truncate_number(123.456) - 0.456) < 1e-6
 
check(truncate_number)