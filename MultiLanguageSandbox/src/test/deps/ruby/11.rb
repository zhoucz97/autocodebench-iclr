

# Calculates the total number of drinks a customer can enjoy under a promotional offer.
# Under this offer, for every 3 bottle caps, the customer can get 1 additional drink.
# The promotion continues as long as the customer has enough caps for exchange.
#
# The function takes a single integer argument, n, which represents the initial number
# of drinks purchased. It returns an integer representing the total number of drinks
# the customer can enjoy, including those received through the promotion.
#
# Examples:
# >>> total_drinks_with_promo(100)
# 149
# >>> total_drinks_with_promo(3)
# 4

def total_drinks_with_promo(n)
  total = n
  caps = n
  
  while caps >= 3
    exchanged = caps / 3
    total += exchanged
    caps = caps % 3 + exchanged
  end
  
  total
end

raise 'Test failed' unless total_drinks_with_promo(100) == 149
raise 'Test failed' unless total_drinks_with_promo(3) == 4
raise 'Test failed' unless total_drinks_with_promo(9) == 13
raise 'Test failed' unless total_drinks_with_promo(10) == 14
raise 'Test failed' unless total_drinks_with_promo(1) == 1
  
puts 'All tests passed!'