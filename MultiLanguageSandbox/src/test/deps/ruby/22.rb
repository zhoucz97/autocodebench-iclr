

# This function calculates the number of different prime pairs whose sum equals a given even number.
# The function accepts a single argument, even_number, which is an even number greater than 6 and less than 32767.
# It returns the count of distinct prime pairs whose sum equals the given even number.
# A prime pair is considered distinct if the primes in the pair are different, regardless of their order.
#
# Example:
# >>> count_prime_pairs(10)
# 2 (since 10 can be expressed as 3+7 and 5+5)
# >>> count_prime_pairs(14)
# 2 (since 14 can be expressed as 3+11 and 7+7)
 
def count_prime_pairs(even_number)
  return 0 if even_number <= 6 || even_number >= 32767

  # Sieve of Eratosthenes to find all primes up to even_number
  sieve = Array.new(even_number + 1, true)
  sieve[0] = sieve[1] = false
  (2..Math.sqrt(even_number).to_i).each do |i|
    if sieve[i]
      (i*i).step(even_number, i) { |j| sieve[j] = false }
    end
  end
  primes = (2..even_number).select { |i| sieve[i] }

  count = 0
  primes.each do |p|
    q = even_number - p
    if q >= p && primes.include?(q)
      count += 1
    end
  end

  count
end

raise 'Test failed' unless count_prime_pairs(10) == 2
raise 'Test failed' unless count_prime_pairs(14) == 2
raise 'Test failed' unless count_prime_pairs(20) == 2
raise 'Test failed' unless count_prime_pairs(26) == 3
raise 'Test failed' unless count_prime_pairs(100) == 6
raise 'Test failed' unless count_prime_pairs(1234) == 25


puts 'All tests passed!'