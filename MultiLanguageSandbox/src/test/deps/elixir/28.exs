defmodule LightMatrix do
def count_monster_spawns(n, torch_positions, glowstone_positions) do
    # Initialize the grid with all cells set to false (unilluminated)
    grid = Enum.reduce(0..(n - 1), %{}, fn x, acc ->
      Enum.reduce(0..(n - 1), acc, fn y, inner_acc ->
        Map.put(inner_acc, {x, y}, false)
      end)
    end)

    # Mark glowstone positions and their surrounding cells as illuminated
    grid = Enum.reduce(glowstone_positions, grid, fn {x, y}, acc ->
      mark_surrounding_cells(acc, x, y, true)
    end)

    # Mark torch positions and their adjacent cells as illuminated
    grid = Enum.reduce(torch_positions, grid, fn {x, y}, acc ->
      mark_adjacent_cells(acc, x, y, true)
    end)

    # Count the number of unilluminated cells
    Enum.count(grid, fn {_pos, illuminated} -> not illuminated end)
  end

  defp mark_surrounding_cells(grid, x, y, value) do
    # Mark the 8 surrounding cells
    directions = [
      {-1, -1}, {-1, 0}, {-1, 1},
      {0, -1},          {0, 1},
      {1, -1},  {1, 0}, {1, 1}
    ]

    Enum.reduce(directions, grid, fn {dx, dy}, acc ->
      nx = x + dx
      ny = y + dy
      if nx >= 0 and nx < n and ny >= 0 and ny < n do
        Map.put(acc, {nx, ny}, value)
      else
        acc
      end
    end)
  end

  defp mark_adjacent_cells(grid, x, y, value) do
    # Mark the 4 adjacent cells
    directions = [
      {-1, 0},
      {1, 0},
      {0, -1},
      {0, 1}
    ]

    Enum.reduce(directions, grid, fn {dx, dy}, acc ->
      nx = x + dx
      ny = y + dy
      if nx >= 0 and nx < n and ny >= 0 and ny < n do
        Map.put(acc, {nx, ny}, value)
      else
        acc
      end
    end)
  end
end

# Example usage:


ExUnit.start()

defmodule LightMatrixTest do
use ExUnit.Case

test "counts monster spawn points with only torches" do
assert LightMatrix.count_monster_spawns(5, [{3, 3}], []) == 12
end

test "counts monster spawn points with torches and glowstones" do
assert LightMatrix.count_monster_spawns(5, [{2, 2}], [{4, 4}]) == 4
end

test "counts monster spawn points in small grid" do
assert LightMatrix.count_monster_spawns(3, [{2, 2}], []) == 0
end
end