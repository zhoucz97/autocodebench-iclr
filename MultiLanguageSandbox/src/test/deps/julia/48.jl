function remaining_apples(m::Int, t::Int, s::Int) :: Int
    """
    Determine the number of whole apples remaining after eating them over a given period.

    Yuki loves eating apples and can finish one in `t` minutes, immediately starting the next one after finishing. Given `m` apples and a period of `s` minutes, calculate how many whole apples remain.

    # Examples
    - `remaining_apples(50, 10, 200)` returns `30`.
    - `remaining_apples(5, 5, 20)` returns `1`.
    """
    eaten = s ÷ t
    remaining = m - eaten
    return max(0, remaining)
end
@assert remaining_apples(50, 10, 200) == 30
@assert remaining_apples(5, 5, 20) == 1
@assert remaining_apples(10, 0, 100) == 0
@assert remaining_apples(3, 10, 30) == 0
@assert remaining_apples(4, 10, 25) == 1