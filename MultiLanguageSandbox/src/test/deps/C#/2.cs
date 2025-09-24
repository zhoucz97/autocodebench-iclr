using System;
using System;
using System.Collections.Generic;
using System.Diagnostics;

class Program
{
 /* Converts a given Fahrenheit temperature to Celsius and returns the result as a string formatted to two decimal places.
    The formula used is: Celsius = 5/9 * (Fahrenheit - 32).
    Example usage:
    >>> ConvertFahrenheitToCelsius(32)
    "c=0.00"
    >>> ConvertFahrenheitToCelsius(212)
    "c=100.00"
*/  
static string ConvertFahrenheitToCelsius(double fahrenheitTemperature)
{
        double celsius = 5.0 / 9.0 * (fahrenheitTemperature - 32);
        return $"c={celsius:F2}";
    }
    static void Main()
    {
        Debug.Assert(ConvertFahrenheitToCelsius(32) == "c=0.00");
        Debug.Assert(ConvertFahrenheitToCelsius(212) == "c=100.00");
        Debug.Assert(ConvertFahrenheitToCelsius(-40) == "c=-40.00");
        Debug.Assert(ConvertFahrenheitToCelsius(98.6) == "c=37.00");
        Debug.Assert(ConvertFahrenheitToCelsius(0) == "c=-17.78");

    }
}