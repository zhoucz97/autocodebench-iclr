assert = require 'assert'

quicksort = (list) ->
  if not list? or not list[1..]? or list.length <= 1
    list or []
  else
    pivot = list[0]
    rest = list[1..]
    less = rest.filter (x) -> x < pivot
    greater = rest.filter (x) -> x >= pivot
    (quicksort less).concat([pivot]).concat(quicksort greater)

checkQuicksort = ->
  assert.deepEqual (quicksort [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5]), [1, 1, 2, 3, 3, 4, 5, 5, 5, 6, 9]
  assert.deepEqual (quicksort [5, 4, 3, 2, 1]), [1, 2, 3, 4, 5]
  assert.deepEqual (quicksort []), []
  assert.deepEqual (quicksort [1]), [1]

checkQuicksort()
