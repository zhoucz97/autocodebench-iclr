

# Given a valid IPv4 address, return its defanged version.
# @param address A string representing the IPv4 address.
# @return The defanged version of the IPv4 address.
#
# Example 1:
# Input: address = "1.1.1.1"
# Output: "1[.]1[.]1[.]1"
#
# Example 2:
# Input: address = "255.100.50.0"
# Output: "255[.]100[.]50[.]0"
defangIPaddr = (address) ->
  address.replace(/\./g, '[.]')
class AssertionError extends Error

assertEqual = (actual, expected, message = null) ->
  throw new AssertionError(message) unless actual is expected
# Tests
main = ->
  assertEqual defangIPaddr("192.168.0.1"), "192[.]168[.]0[.]1", "Test case 1 failed"
  assertEqual defangIPaddr("172.16.254.1"), "172[.]16[.]254[.]1", "Test case 2 failed"
  assertEqual defangIPaddr("10.0.0.1"), "10[.]0[.]0[.]1", "Test case 3 failed"
  console.log "All tests passed"

main()