using System;
using System;
using System.Collections.Generic;
using System.Diagnostics;

class Program
{
/* Converts a given number of seconds into a formatted time string without leading zeros.
    The format of the output is "<Hours>:<Minutes>:<Seconds>". Each component is an integer without leading zeros.
    Examples:
    >>> FormatTime(0)
    "0:0:0"
    >>> FormatTime(3661)
    "1:1:1"
    >>> FormatTime(5436)
    "1:30:36"
*/
   static string FormatTime(int seconds)
{
        if (seconds < 0)
        {
            throw new ArgumentException("Seconds cannot be negative", nameof(seconds));
        }

        int hours = seconds / 3600;
        int remainingSeconds = seconds % 3600;
        int minutes = remainingSeconds / 60;
        int remainingSecondsFinal = remainingSeconds % 60;

        return $"{hours}:{minutes}:{remainingSecondsFinal}";
    }
    static void Main()
    {
        Debug.Assert(FormatTime(0) == "0:0:0");
        Debug.Assert(FormatTime(3661) == "1:1:1");
        Debug.Assert(FormatTime(5436) == "1:30:36");
        Debug.Assert(FormatTime(3600) == "1:0:0");
        Debug.Assert(FormatTime(7200) == "2:0:0");
        Debug.Assert(FormatTime(86399) == "23:59:59");
        Debug.Assert(FormatTime(12345) == "3:25:45");

    }
}