

# Calculates the total number of cigarettes Peter can smoke given an initial
# amount of cigarettes and a conversion rate of cigarette butts to new cigarettes.
# Peter starts with n cigarettes and can turn every k butts into a new cigarette.
#
# The function takes two arguments, n and k, where n is the initial number of
# cigarettes and k is the number of butts required for a new cigarette. It
# returns the total number of cigarettes Peter can smoke.
#
# Examples:
# >>> total_smoked_cigarettes(4, 3)
# 5
# >>> total_smoked_cigarettes(10, 3)
# 14
def total_smoked_cigarettes(n, k)
  total = 0
  butts = 0

  while n > 0
    # Smoke all available cigarettes
    total += n
    butts += n
    n = 0

    # Convert butts to new cigarettes
    new_cigs = butts / k
    butts %= k
    n = new_cigs
  end

  total
end

raise 'Test failed' unless total_smoked_cigarettes(4, 3) == 5
raise 'Test failed' unless total_smoked_cigarettes(10, 3) == 14
raise 'Test failed' unless total_smoked_cigarettes(20, 4) == 26
raise 'Test failed' unless total_smoked_cigarettes(1, 2) == 1
raise 'Test failed' unless total_smoked_cigarettes(100, 5) == 124
  