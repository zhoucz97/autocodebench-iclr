function calculate_growth(initial_value::Float64, growth_rate::Float64, years::Int) :: Float64
"""
    calculate_growth(initial_value, growth_rate, years)

Calculate the final value after applying a constant growth rate over a number of years.

# Arguments
- `initial_value`: Initial value (e.g., initial amount of money).
- `growth_rate`: Annual growth rate as a decimal (e.g., 0.1 for 10%).
- `years`: Number of years over which the growth is applied.

# Examples
```julia
calculate_growth(10.0, 0.05, 5) # should return approximately 12.7628
calculate_growth(15.0, 0.03, 10) # should return approximately 20.1559
"""
    final_value = initial_value * (1 + growth_rate)^years
    return final_value
end
@assert calculate_growth(10.0, 0.05, 5) ≈ 12.7628
@assert calculate_growth(15.0, 0.03, 10) ≈ 20.1587
@assert calculate_growth(20.0, 0.02, 3) ≈ 21.2242
@assert calculate_growth(5.0, 0.1, 8) ≈ 10.7179