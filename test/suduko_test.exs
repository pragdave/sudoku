defmodule SudukoTest do
  use ExUnit.Case
  doctest Suduko

  alias Suduko, as: S
  test "basic setup" do

    assert length(S.squares) == 81
    assert length(S.unitlist) == 27
    for s <- S.squares do
      assert length(S.units[s]) == 3
      assert length(S.peers[s]) == 20
    end
    
    assert S.units['C2'] == [['A2', 'B2', 'C2', 'D2', 'E2', 'F2', 'G2', 'H2', 'I2'],
                             ['C1', 'C2', 'C3', 'C4', 'C5', 'C6', 'C7', 'C8', 'C9'],
                             ['A1', 'A2', 'A3', 'B1', 'B2', 'B3', 'C1', 'C2', 'C3']]
    assert S.peers['C2'] == ['A2', 'B2', 'D2', 'E2', 'F2', 'G2', 'H2', 'I2',
                             'C1', 'C3', 'C4', 'C5', 'C6', 'C7', 'C8', 'C9',
                             'A1', 'A3', 'B1', 'B3']
  end

  test "initial grid" do
    initial = S.initial_grid
    assert Enum.count(initial) == 81
    assert initial['C2'] == '123456789'
  end

  test "extracting grid values" do
    spec = [ ?. | List.duplicate('1234567890', 8) |> List.flatten ]
    result = S.extract_grid_values(spec)
    assert Enum.count(result) == 81
    assert result['A1'] == '.'
    assert result['A2'] == '1'
    assert result['I9'] == '0'
  end

  test "empty assign leaves initial grid wide open" do
    spec = List.duplicate(?., 81)
    assert {:ok, values} = S.parse_grid(spec)
    assert Enum.count(values) == 81
    for {_k,v} <- values do
      assert v == '123456789'
    end
  end

  test "A single '1' eliminates that 1 from the peers" do
    spec = '1' ++ List.duplicate(?., 80)
    assert {:ok, values} = S.parse_grid(spec)
    assert Enum.count(values) == 81

    for r <- 'ABCDEFGHI', c <- '123456789' do
      cond do
        c == ?1 && r == ?A ->
          assert values[[r,c]] == '1'
        r < ?D && c < ?4 ->
          assert values[[r,c]] == '23456789'
        r == ?A ->
          assert values[[r,c]] == '23456789'
        c == ?1 ->
          assert values[[r,c]] == '23456789'
        true ->
          assert values[[r,c]] == '123456789'
      end
    end
  end

  test "An impossible initial state is detected" do
    spec = '11' ++ List.duplicate(?., 79)
    assert {:conflict, message } = S.parse_grid(spec)
    assert message == "No choices left for A2"
  end

  test "We can complete the top row" do
    spec = '123456789' ++ List.duplicate(?., 72)
    assert {:ok, values} = S.parse_grid(spec)
    assert Enum.count(values) == 81

    for r <- 'ABCDEFGHI', c <- '123456789' do
      cond do
        r == ?A ->
          assert values[[r,c]] == [c]
        r <= ?C && c <= ?3 ->
          assert values[[r,c]] == '456789'
        r <= ?C && c <= ?6 ->
          assert values[[r,c]] == '123789'
        r <= ?C && c <= ?9 ->
          assert values[[r,c]] == '123456'
        true ->
             assert values[[r,c]] == '123456789' -- [c]
      end
    end
  end
  
end
