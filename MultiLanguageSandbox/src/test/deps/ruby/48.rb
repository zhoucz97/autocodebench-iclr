

# Calculates the total earnings of Deliv-e-droid based on the number of successfully delivered
# and failed deliveries. For each successful delivery, Deliv-e-droid earns 50 units of money,
# and for each failed delivery, it loses 10 units. If the number of successful deliveries is
# greater than the number of failed ones, Deliv-e-droid receives a bonus of 500 units.
#
# Arguments:
# - delivered: Integer representing the number of successfully delivered packages (0 <= delivered <= 100).
# - failed: Integer representing the number of failed deliveries (0 <= failed <= 100).
#
# Returns:
# - Integer: The total earnings of Deliv-e-droid, which can be negative if the losses outweigh the gains.
#
# Examples:
# >>> calculate_droid_earnings(5, 2)
# 730
# >>> calculate_droid_earnings(0, 10)
# -100

def calculate_droid_earnings(delivered, failed)
  base_earnings = delivered * 50 - failed * 10
  bonus = delivered > failed ? 500 : 0
  base_earnings + bonus
end

raise 'Test failed' unless calculate_droid_earnings(5, 2) == 730
raise 'Test failed' unless calculate_droid_earnings(0, 10) == -100
raise 'Test failed' unless calculate_droid_earnings(10, 0) == 1000
raise 'Test failed' unless calculate_droid_earnings(3, 5) == 100
raise 'Test failed' unless calculate_droid_earnings(20, 20) == 800