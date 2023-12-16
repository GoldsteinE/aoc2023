defmodule Day16 do
  def offset(direction) do
    case direction do
      :right -> { 1,  0}
      :down  -> { 0,  1}
      :left  -> {-1,  0}
      :up    -> { 0, -1}
    end
  end

  def in_bounds(tiles, x, y) do
    x >= 0 && y >= 0 && x < Arrays.size(tiles[0]) && y < Arrays.size(tiles)
  end

  def seen_map(state) do
    receive do
      {:check, ray, sender} ->
        send(sender, MapSet.member?(state, ray))
        seen_map(MapSet.put(state, ray))
      {:aggregate, tiles, sender} ->
        send(sender, state
          |> Enum.map(fn({x, y, _}) -> {x, y} end)
          |> Enum.filter(fn({x, y}) -> in_bounds(tiles, x, y) end)
          |> Enum.into(MapSet.new())
          |> MapSet.size)
    end
  end

  def split_ray(tiles, x, y, dir1, dir2, seen) do
    # You'd think that doing these two concurrently would be faster,
    # but alas, it's actually more than 10x slower.
    cast_ray(tiles, {x, y, dir1}, seen)
    cast_ray(tiles, {x, y, dir2}, seen)
  end

  def cast_ray(tiles, ray, seen) do
    send(seen, {:check, ray, self()})
    receive do
      true -> nil
      false ->
        {x, y, direction} = ray
        {dx, dy} = offset(direction)
        {nx, ny} = {x + dx, y + dy}
        if in_bounds(tiles, nx, ny) do
          cell = tiles[ny][nx]
          case {cell, dx, dy} do
            { ".",  _,  _ } -> cast_ray(tiles, {nx, ny, direction}, seen)
            { "/",  1,  0 } -> cast_ray(tiles, {nx, ny, :up}, seen)
            { "/", -1,  0 } -> cast_ray(tiles, {nx, ny, :down}, seen)
            { "/",  0,  1 } -> cast_ray(tiles, {nx, ny, :left}, seen)
            { "/",  0, -1 } -> cast_ray(tiles, {nx, ny, :right}, seen)
            {"\\",  1,  0 } -> cast_ray(tiles, {nx, ny, :down}, seen)
            {"\\", -1,  0 } -> cast_ray(tiles, {nx, ny, :up}, seen)
            {"\\",  0,  1 } -> cast_ray(tiles, {nx, ny, :right}, seen)
            {"\\",  0, -1 } -> cast_ray(tiles, {nx, ny, :left}, seen)
            { "-",  _,  0 } -> cast_ray(tiles, {nx, ny, direction}, seen)
            { "|",  0,  _ } -> cast_ray(tiles, {nx, ny, direction}, seen)
            { "-",  _,  _ } -> split_ray(tiles, nx, ny, :left, :right, seen)
            { "|",  _,  _ } -> split_ray(tiles, nx, ny, :up, :down, seen)
          end
        end
    end
  end

  def inputs_set(tiles) do
    mx = Arrays.size(tiles) - 1
    my = Arrays.size(tiles[0]) - 1
    vertical = 0..mx |> Enum.flat_map(fn(x) -> [{x, -1, :down}, {x, my + 1, :up}] end)
    horizontal = 0..my |> Enum.flat_map(fn(y) -> [{-1, y, :right}, {mx + 1, y, :left}] end)
    Enum.concat(vertical, horizontal)
  end

  def main() do
    parse_line = fn(line) -> line |> String.codepoints |> Arrays.new end
    tiles = IO.read(:stdio, :all) |> String.trim |> String.split |> Enum.map(parse_line) |> Arrays.new
    rays = if List.first(System.argv) == "1" do
      [{-1, 0, :right}]
    else
      inputs_set(tiles)
    end

    res = rays
      |> Task.async_stream(fn ray ->
        seen = spawn(fn() -> seen_map(MapSet.new()) end)
        cast_ray(tiles, ray, seen)
        send(seen, {:aggregate, tiles, self()})
        receive do num -> num end
      end)
      |> Enum.reduce(0, fn {:ok, num}, acc -> max(num, acc) end)

    IO.puts(res)
  end
end
