# CRS
Repo for CSC 430

Assignment #5

What's done so far:
- Store has been added
- ':=' operator has been added
- begin primitive implemented
- ArrayV added
- new-array primitive implemented

Some helpers I made:
- allocate -- allocates a sequence of locations in the store, initializes
all of the cells to the given value, and returns the location of the first
- single-alloc -- allocates a single location in the store, and returns its location

Notes:
- The Store is a struct with two attributes, a mutable hashtable and a Natural number.
The number is used as a count for the number of items in the store.
-Whenever you add a primitive, you have to create the racket function, and then
add it to BOTH the top-env and top-store, in the same way the others are added,
make sure the location value for a specific primitive is the same in both top-env
and top-store.