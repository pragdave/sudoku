Suduko
======

An implementation of Peter Norvig's Sudoku solver.
(see http://norvig.com/sudoku.html)

A Sudoku puzzle is a grid of 81 squares; the majority of enthusiasts
label the columns 1-9, the rows A-I, and call a collection of nine
squares (column, row, or box) a unit and the squares that share a unit
the peers. A puzzle leaves some squares blank and fills others with
digits, and the whole idea is: A puzzle is solved if the squares in
each unit are filled with a permutation of the digits 1 to 9.

Call it using

    {:ok, grid} = Sudoku.solve('4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......')
    Sudoku.display grid

Note that you pass in a charlist, not a string.

Copyright © 2015 Dave Thomas, dave@pragprog.org
Licensed under the terms of the MIT license.

