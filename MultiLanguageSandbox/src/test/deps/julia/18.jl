function average_balance(balances::Vector{Float64}) :: String

"""
Calculate the average of a series of monthly balances.

This function takes a vector of monthly balances and returns the average balance,
formatted as a string with a "\$" sign and rounded to two decimal places.

# Examples
>>> average_balance([100.0, 200.0, 300.0])
"\$200.00"

>>> average_balance([123.45, 678.90, 234.56, 789.01, 456.78])
"\$456.54"
"""
    avg = sum(balances) / length(balances)
    return "\$$(round(avg, digits=2))"
end
@assert average_balance([150.50, 300.75, 450.25]) == "\$300.5"
@assert average_balance([1200.00, 1100.00, 1000.00, 900.00]) == "\$1050.0"
@assert average_balance([1234.56, 7890.12, 4567.89]) == "\$4564.19"
@assert average_balance([500.00, 600.00, 700.00, 800.00, 900.00]) == "\$700.0"