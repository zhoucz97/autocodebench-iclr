-module(array_intersection).
-export([find_intersection/2, test/0]).

find_intersection(Nums1, Nums2) ->
    Set1 = sets:from_list(Nums1),
    Set2 = sets:from_list(Nums2),
    Intersection = sets:intersection(Set1, Set2),
    SortedList = lists:sort(sets:to_list(Intersection)),
    SortedList.
test() ->
[2, 3] = find_intersection([1, 2, 3, 4], [2, 3, 5, 6]),
[] = find_intersection([7, 8, 9], [10, 11, 12]),
[1, 2, 3] = find_intersection([1, 2, 3], [1, 2, 3, 4]),
[] = find_intersection([], [1, 2, 3]),
[] = find_intersection([1, 2, 3], []),
ok.