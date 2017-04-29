# Emulator

A software implementation of the BCKW reducer machine defined at
[Combinatron/Specification](https://github.com/Combinatron/Specification).

This is currently only a working prototype and may have bugs. It should be good
enough to play around with though.

## Programming

You can write your own programs if you'd like to experiment with programming in
my modified BCKW calculus. The language is pretty simple.

* Case-insensitive
* Whitespace-insensitive (so long as primitives are separated by whitespace).
* Parentheses are used for grouping
* Simple primitives are just `b` `c` `k` `w`.
* Side effecting primitives include the index of the sentence they operate on,
  1-based, not separated by a space e.g. `g1`, `p3`.

Check out the examples directory for example programs.

## Compiling and Running

To run a written program it must first be compiled. You can compile a program
with the following command:

```bash
cabal run Combinatron-Compiler -- input_file_name output_file_name
```

You then run the output file like so:

```bash
cabal run Combinatron-Emulator -- output_file_name
```

This will run the program and print the final machine state to standard output.

## Debugging (TODO: Make a dedicated debugger)

Debugging a written program is slightly more involved right now. For now you can
just change the invocation of `run` in `src/Main.hs` to `runDebug`. Run the
program as before and now each intermediate state will be printed to standard
output.

## Contributing

You can use Nix and `nix-shell` to get a working environment that can be messed
around in. The project is also buildable with cabal.

The primitive ops are pretty thoroughly tested. The operations defined in
Combinatron.hs are written to closely match the specification, so should be
obviously correct.
