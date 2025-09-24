defmodule BombingAnalysis do
def analyze_bombings(bomb_coords, target_coords) do
    # Process each target coordinate
    Enum.map(target_coords, fn target ->
      hits = 
        bomb_coords
        |> Enum.with_index(1)  # Add round number (1-based index)
        |> Enum.filter(fn {bomb, _round} -> point_in_bomb?(target, bomb) end)
        |> Enum.map(fn {_bomb, round} -> round end)
      
      case hits do
        [] -> {false, 0, 0}
        _ -> {true, length(hits), List.last(hits)}
      end
    end)
  end

  defp point_in_bomb?({x, y}, {bl_x, bl_y, tr_x, tr_y}) do
    x >= bl_x and x <= tr_x and y >= bl_y and y <= tr_y
  end
end



ExUnit.start()

defmodule BombingAnalysisTest do
use ExUnit.Case

test "analyzes bombing impacts on target points" do
assert BombingAnalysis.analyze_bombings([{1, 1, 5, 5}, {5, 5, 10, 10}], [{3, 2}, {5, 5}, {7, 1}]) ==
[{true, 1, 1}, {true, 2, 2}, {false, 0, 0}]
assert BombingAnalysis.analyze_bombings([{2, 2, 4, 4}], [{1, 1}, {3, 3}]) ==
[{false, 0, 0}, {true, 1, 1}]
end
end