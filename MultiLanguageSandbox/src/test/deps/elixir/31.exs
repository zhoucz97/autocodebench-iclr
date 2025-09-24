defmodule LightSwitch do
def find_open_lights(n) do
    1..n
    |> Enum.filter(fn x -> :math.sqrt(x) == trunc(:math.sqrt(x)) end)
  end
end



ExUnit.start()
defmodule LightSwitchTest do
use ExUnit.Case

test "finds open lights after a series of toggles" do
assert LightSwitch.find_open_lights(3) == [1]
assert LightSwitch.find_open_lights(6) == [1, 4]
end

test "handles no open lights" do
assert LightSwitch.find_open_lights(2) == [1]
end

test "handles larger number of lights" do
assert LightSwitch.find_open_lights(10) == [1, 4, 9]
end
end