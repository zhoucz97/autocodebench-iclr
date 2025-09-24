

# Given a list of time points in a 24-hour format (hour:minute "HH:MM"), find the minimum time difference between any two time points and represent it in minutes.
# @param timePoints A list of time points.
# @return The minimum time difference between any two time points in minutes.
#
# Example 1:
# Input: timePoints = ["23:59","00:00"]
# Output: 1
#
# Example 2:
# Input: timePoints = ["00:00","23:59","00:00"]
# Output: 0
#
# Constraints:
# - 2 <= timePoints.length <= 20000
# - timePoints[i] has the format "HH:MM"
minTimeDifference = (timePoints) ->
  # Convert each time point to total minutes since midnight
  toMinutes = (time) ->
    [hours, minutes] = time.split(':').map (num) -> parseInt(num, 10)
    hours * 60 + minutes

  # Convert all time points to minutes
  minutesList = timePoints.map toMinutes

  # Sort the list of minutes
  minutesList.sort (a, b) -> a - b

  # Initialize minDiff with the difference between the first and last (circular)
  minDiff = (minutesList[0] + 1440 - minutesList[minutesList.length - 1]) % 1440

  # Iterate through the sorted list to find the minimum difference
  for i in [1...minutesList.length]
    diff = (minutesList[i] - minutesList[i - 1]) % 1440
    if diff < minDiff
      minDiff = diff
      # Early exit if possible (smallest possible difference is 0)
      return 0 if minDiff == 0

  minDiff
class AssertionError extends Error

assertEqual = (actual, expected, message = null) ->
  throw new AssertionError(message) unless actual is expected
# Testing
main = ->
  # Test Case 1
  timePoints1 = ["12:00", "13:30", "15:45", "16:00"]
  assertEqual(minTimeDifference(timePoints1), 15, "Test Case 1 Failed")

  # Test Case 2
  timePoints2 = ["00:00", "06:00", "12:00", "18:00"]
  assertEqual(minTimeDifference(timePoints2), 360, "Test Case 2 Failed")

  # Test Case 3
  timePoints3 = ["01:30", "02:00", "03:45", "04:15"]
  assertEqual(minTimeDifference(timePoints3), 30, "Test Case 3 Failed")

  console.log("All tests passed")

main()