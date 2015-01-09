defmodule Helpers do
  @moduledoc """
  A separate module of functions we need at compile time.
  """
  @doc "Cross product of elements in `as` and `bs`"
  @spec cross(list(t1), list(t2)) :: list([char,...]) when t1: char, t2: char
  def cross(as, bs) do
    for a <- as, b <- bs, do: [a, b]
  end

  def merge([l1,l2,l3]), do: l1 ++ l2 ++ l3
end

  
defmodule Sudoku do

  @moduledoc File.read!("README.md")
  
  @type message        :: binary
  @type digit          :: ?1..?9
  @type square_key     :: char_list  # A1 .. I9
  @type square_content :: [ digit, ... ]
  @type grid           :: %{ square_key => square_content }
  @type reason         :: binary
  @type maybe_grid     :: { :ok, grid } | { :conflict, reason }
  @type unit_list      :: [[square_key]]
  
  import Helpers

  @digits '123456789'
  @rows   'ABCDEFGHI'
  @cols   @digits

  @squares cross(@rows, @cols)  # A1, A2, .. I9

  # the 9 squares
  @peer_groups (for r <- ['ABC', 'DEF', 'GHI'],
                    c <- ['123', '456', '789'],
                    do: cross(r,c)
  )


  # the list of all units
  @unitlist  (for c <- @cols, do: cross(@rows, [c])) ++
             (for r <- @rows, do: cross([r], @cols)) ++
             @peer_groups

  # the units for each square
  @units  (for s <- @squares, into: %{} do
             { s, (for u <- @unitlist, s in u, do: u) }
           end)

  # the peers for each square
  @peers (for s <- @squares, into: %{} do
            { s, merge(@units[s]) |> Enum.uniq |> Enum.reject(fn (val) -> val == s end)}
          end)


  @spec squares() :: [[1..255,...], ...]
  def squares,  do: @squares

  @spec unitlist() :: unit_list
  def unitlist, do: @unitlist

  @spec units() :: %{ square_key => unit_list }
  def units,    do: @units

  @spec peers() :: %{ square_key => unit_list }
  def peers,    do: @peers

  # ################ Parse a Grid ################

  @spec initial_grid() :: grid
  def initial_grid do
    for s <- @squares, into: %{}, do: { s, @digits }
  end

  @spec parse_grid(char_list) :: maybe_grid
  def parse_grid(grid_spec) do
    grid_spec
    |> extract_grid_values
    |> assign_into(initial_grid)
  end

  @spec extract_grid_values(char_list) :: grid
  def extract_grid_values(grid_spec) do
    acceptable_characters = [ ?., ?0 | @digits ]
    normalized_spec = for c <- grid_spec, c in acceptable_characters, do: [c]
    unless length(normalized_spec) == 81, do: raise("Invalid grid spec: #{grid_spec}") 
    Enum.zip(@squares, normalized_spec) |> Enum.into(%{})
  end

  @spec assign_into(grid, grid) :: maybe_grid
  def assign_into(grid_values, initial_grid) do
    Enum.reduce(grid_values, { :ok, initial_grid }, &assign_into_square/2)
  end

  @spec assign_into_square({square_key, square_content}, {:ok, grid}) :: maybe_grid
  def assign_into_square({s, [d]}, {:ok, grid}) when d in @digits do
    assign(grid, s, [d])
  end

  def assign_into_square({_s, [_d]}, grid = {:ok, _}) do
    grid
  end

  @spec assign_into_square({}, {:conflict, message}) :: {:conflict, message}
  def assign_into_square(_, error) do
    error
  end


  ######################################################################
  # The `assign` and `eliminate` functions work to set a value into a  #
  # square. Along the way, they also propagate the implications of     #
  # doing the assignment. In particular, the units related to the      #
  # square cannot have the value that is being assigned into the       #
  # square. If as part of this process we reduce the value in a square #
  # to a single digit, we assign it recursively.                       #
  #                                                                    #
  # Throughout this code, we deal with a `maybe_grid`, which is a      #
  # tuple of `{:ok, grid}` if `grid` contains no contradictions or     #
  # `{:conflict, message}` if the grid cannot be solved. Returning the #
  # conflict case lets us prune the search trees, as we no longer need #
  # to recurse.                                                        #
  ######################################################################
                                                                             
  @spec assign(grid, square_key, square_content) :: maybe_grid
  def assign(grid, s, digits) do
    unless is_list(grid[s]), do: raise("s = #{s}\n#{inspect grid}") 
    all_but_d = grid[s] -- digits
    Enum.reduce(all_but_d, {{:ok, grid}, s}, &eliminate_by_d/2)
    |> elem(0)
  end


  # ################ Constraint Propagation ################

  @doc """
  Eliminate all the other values (except d) from values[s] and propagate.
  Return `{:ok, values}`, or `{:conflict, message}` if a 
  contradiction is detected.
  """
  
  @spec eliminate_by_d(digit, {maybe_grid, square_key}) :: {maybe_grid, square_key}
  
  def eliminate_by_d(d, {{:ok, grid}, s}) when is_integer(d) do
    { eliminate(grid, s, d), s}
  end
  
  def eliminate_by_d(_d, other = {{:conflict, _message}, _s}) do
    other
  end

  @spec eliminate_by_d(square_key, {maybe_grid, digit}) :: {maybe_grid, digit}

  def eliminate_by_s(s, {{:ok, grid}, d}) when is_integer(d) do
    { eliminate(grid, s, d), d}
  end
  
  def eliminate_by_s(_s, other = {{:conflict, _message}, _d}) do
    other
  end
  
  
  @doc """
  Eliminate d from values[s]; propagate when values or places <= 2.
  Return {:ok,values}; return { :conflict, msg } if a contradiction is detected.
  """
  @spec eliminate(grid, square_key, digit) :: maybe_grid
  def eliminate(grid = %{}, s, digit) when is_integer(digit) do
    if already_eliminated(grid, s, digit) do
      { :ok, grid }
    else
      grid
      |> check_if_this_square_is_now_defined(s, digit)
      |> update_related_units(s, digit)
    end
  end

  @spec already_eliminated(grid, square_key, digit) :: grid | nil
  def already_eliminated(grid, s, digit) when is_list(s) and is_integer(digit) do
    cond do
      digit in grid[s] -> nil
      true             -> grid
    end
  end

  # (1) If a square s is reduced to one value d2, then eliminate
  # d2 from the peers

  @spec check_if_this_square_is_now_defined(grid, square_key, digit) :: maybe_grid
  
  def check_if_this_square_is_now_defined(grid, s, digit)
  when is_list(s) and is_integer(digit) do
    values =  update_in(grid, [s], &List.delete(&1, digit))
    update_target_square(values, s, values[s])
  end

  @spec update_target_square(grid, square_key, square_content) :: maybe_grid
  
  def update_target_square(_, s, []) do  # Contradiction: removed last value
    {:conflict, "No choices left for #{s}"}
  end

  # Single value—must be the one
  def update_target_square(grid, s, [d2])  when is_list(s) and is_integer(d2) do
    Enum.reduce(@peers[s], {{:ok, grid}, d2}, &eliminate_by_s/2)
    |> elem(0)
  end

  def update_target_square(grid, s, ds)  when is_list(s) and is_list(ds) do
    {:ok, grid} ## nothing special
  end
  
  
  # look for a place to put the digit among the related units

  @spec update_related_units(maybe_grid, square_key, digit) :: maybe_grid
  def update_related_units(other = {:conflict, _message}, _s, _d), do: other
  def update_related_units({:ok, grid}, s, d) when is_list(s) and is_integer(d) do
    units = @units[s]
    Enum.reduce(units, {{:ok, grid}, s, d}, &update_a_unit/2)
    |> elem(0)
  end


  @spec update_a_unit(unit_list, {maybe_grid, square_key, digit}) ::
     {maybe_grid, square_key, digit}
  
  def update_a_unit(u, {{:ok, grid}, s, d})  when is_list(s) and is_integer(d) do
    dplaces = (for s <- u, d in grid[s], do: s)
    case length(dplaces) do
      0 -> { {:conflict, "No options available, updating unit #{inspect u}"}, s, d}
      1 -> { assign(grid, hd(dplaces), [d]), s, d }
      _ -> { {:ok, grid}, s, d }
    end
  end
  
  def update_a_unit(_u, {other = {:conflict, _message}, s, d}) do
    {other, s, d}
  end

  ###################################################
  # Solve a grid by performing an exhaustive search #
  ###################################################

  @spec solve(char_list) :: maybe_grid
  def solve(grid_spec), do: search(parse_grid(grid_spec))

  @spec search(maybe_grid) :: maybe_grid
  def search(failed = {:conflict, _message}), do: failed
  def search({:ok, grid}) do
    cond do
      Enum.all?(grid, fn {_k, v} -> length(v) == 1 end) ->
        { :ok, grid }
      true ->
        # find a square with the shortest list of candidate digits,
        # and try each in turn, exiting when we find the first success
        {key, contents} = Enum.filter(grid, fn {_k, v} -> length(v) > 1 end)
                          |> Enum.min_by(fn {_k, v} -> length(v) end)

        loop_until_ok_returned(contents, fn (d) -> search(assign(grid, key, [d])) end)
    end
  end

  def loop_until_ok_returned([d|rest], searcher) do
    maybe_loop(searcher.(d), rest, searcher)
  end
  
  def loop_until_ok_returned([], _searcher) do
    { :conflict, "Not solved" }
  end

  # if we have a valid grid, no need to continue searching
  def maybe_loop({:ok, grid}, _rest, _searcher) do
    {:ok, grid}
  end

  def maybe_loop({:conflict, _message}, rest, searcher) do
    loop_until_ok_returned(rest, searcher)
  end
  
  ####################
  # Display the grid #
  ####################

  def display(grid) do
    for r <- @rows do
      (for c <- @cols, do: grid[[r,c]])
      |> Enum.map(&square_to_string/1)
      |> Enum.chunk(3)
      |> Enum.intersperse('|')
      |> List.flatten
      |> IO.puts
      if r in 'CF' do
        IO.puts "---------+---------+---------"
      end
    end
    :ok
  end

  def square_to_string([d]), do: [?\s, d, ?\s]
  def square_to_string(_),   do: ' . '
end
