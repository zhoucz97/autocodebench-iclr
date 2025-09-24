defmodule InfectionCalculator do
def total_infected(x, n) when x == 1 do
    1 + n
  end

  def total_infected(x, n) do
    ( :math.pow(x, n + 1) - 1 ) / (x - 1) |> round
  end
end



ExUnit.start()
defmodule InfectionCalculatorTest do
use ExUnit.Case

test "calculates total infection correctly" do
assert InfectionCalculator.total_infected(10, 2) == 121
assert InfectionCalculator.total_infected(3, 3) == 64
end

test "handles zero rounds" do
assert InfectionCalculator.total_infected(10, 0) == 1
end

test "handles zero infection rate" do
assert InfectionCalculator.total_infected(0, 5) == 1
end
end