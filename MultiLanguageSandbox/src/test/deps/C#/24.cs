using System.Numerics;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Numerics;
class Program
{

    

    /* Calculate the value of 2 raised to the power of a given non-negative integer 'n'.
    This function uses high-precision arithmetic to handle large exponents.
    Examples:
    >>> CalculatePowerOfTwo(3)
    8
    >>> CalculatePowerOfTwo(10)
    1024
    >>> CalculatePowerOfTwo(0)
    1
    */

    static BigInteger CalculatePowerOfTwo(int n)
{
        if (n < 0)
        {
            throw new ArgumentException("The exponent must be a non-negative integer.", nameof(n));
        }

        return BigInteger.Pow(2, n);
    }
    static void Main()
    {           
        Debug.Assert(CalculatePowerOfTwo(3).Equals(new BigInteger(8)));
        Debug.Assert(CalculatePowerOfTwo(10).Equals(new BigInteger(1024)));
        Debug.Assert(CalculatePowerOfTwo(15).Equals(new BigInteger(32768)));
        Debug.Assert(CalculatePowerOfTwo(0).Equals(new BigInteger(1)));

    }
}
