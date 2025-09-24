

# Converts a positive integer into a unique binary power representation.
# The function takes a single argument, n, which is a positive integer, and
# returns a string representing n as a sum of powers of 2, with the powers 
# themselves also represented as sums of powers of 2 when applicable.
#
# The representation is formatted as '2(b)' where 'b' is the power. If 'b' itself 
# is a power of 2, it's represented in a similar nested format. Spaces are not 
# used in the representation.
#
# Example:
# >>> binary_power_representation(137)
# "2(2(2)+2+2(0))+2(2+2(0))+2(0)"
# >>> binary_power_representation(1315)
# "2(2(2+2(0))+2)+2(2(2+2(0)))+2(2(2)+2(0))+2+2(0)"

def binary_power_representation(n)
  return "2(0)" if n == 1

  powers = []
  remaining = n
  i = 0
  while remaining > 0
    if (remaining & 1) == 1
      powers << i
    end
    remaining >>= 1
    i += 1
  end

  terms = powers.map do |power|
    if power == 0
      "2(0)"
    else
      "2(#{power_representation(power)})"
    end
  end

  terms.join('+')
end

def power_representation(power)
  return "0" if power == 0
  return "1" if power == 1

  # Check if power is a power of 2
  if (power & (power - 1)) == 0
    log2 = Math.log2(power).to_i
    if log2 == 0
      "0"
    else
      "2(#{power_representation(log2)})"
    end
  else
    # Decompose power into sum of powers of 2
    remaining = power
    terms = []
    i = 0
    while remaining > 0
      if (remaining & 1) == 1
        if i == 0
          terms << "2(0)"
        else
          terms << "2(#{power_representation(i)})"
        end
      end
      remaining >>= 1
      i += 1
    end
    terms.join('+')
  end
end

raise 'Test failed' unless binary_power_representation(137) == "2(2(2)+2+2(0))+2(2+2(0))+2(0)"
raise 'Test failed' unless binary_power_representation(1315) == "2(2(2+2(0))+2)+2(2(2+2(0)))+2(2(2)+2(0))+2+2(0)"
raise 'Test failed' unless binary_power_representation(1) == "2(0)"
raise 'Test failed' unless binary_power_representation(3) == "2+2(0)"
raise 'Test failed' unless binary_power_representation(10) == "2(2+2(0))+2"


puts 'All tests passed!'