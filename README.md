# Funki Crab

Funki Crab is an optimising Brainfuck compiler written in Rust.

# Purpose

I created Funki Crab as an exercise in learning about compiler development, Immediate Representation (IR) techniques and optimisation. Brainfuck struck me as a 
sensible language for such a project given its simplicity, Turing-completeness and wealth of potential optimisations.

Funki Crab is an anagram of Brainfuck. 'Crab' is a reference to Ferris, the Rust mascot.

# Compilation

Currently, Funki Crab can only transpile Brainfuck to C. In the future, it will be capable of compiling to assembly or even raw binaries.

# Optimisations

## Combining Shifts, Increments And Decrements

Funki Crab can combine consecutive shifts, increments and decrements together into a single IR instruction.

Example:

`++++>>>><<---+<`

Becomes:

```
*ptr += 4;
ptr += 2;
*ptr -= 2;
--ptr;
```

## Copy-And-Multiply Loop Elision

Funki Crab can unroll loops that count down one cell in order to add-and-multiply other cells into single IR instructions.

Example:

`[->+++>>>--<<<<]`

Becomes:

```
*(ptr + 1) += *ptr * 3;
*(ptr + 4) -= *ptr * 2;
*ptr = 0;
```

## Zeroing And Set Loop Elision

Funki Crab can transform loops that decrement to zero and add into a single IR instruction.

Example:

`[-]++++`

Becomes:

`*ptr = 4;`

## Shift Elision

Funki Crab can elide shift operations by keeping track of the increment and passing it on to future operations.

Example:

`+>>>+`

Becomes:

```
++*ptr;
++*(ptr + 3);
```

## Inter-Loop Shift Elision

Funki Crab can elide shifts across loop boundaries.

Example:

`+<<[>]`

```
++*ptr;
while (*(ptr - 2)) {
	++ptr;
}
```

## Dead Code Elimination

Funki Crab can eliminate many common instances of dead code.

Example:

`+,>[-],`

Becomes:

```
*ptr = getchar();
++ptr;
*ptr = getchar();
```

# Combining Optimisations

Funki Crab is capable of combining the optimisations listed above, and more, through multiple transformations of the IR.

# Speedup

Typical speedup with Funki Crab optimisations enabled vs disabled for substantial Brainfuck programs is ~100x.
