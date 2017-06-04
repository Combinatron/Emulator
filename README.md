# Emulator

A software implementation of the BCKW reducer machine defined at
[Combinatron/Specification](https://github.com/Combinatron/Specification).

This is currently only a working prototype and may have bugs. It should be good
enough to play around with though.

## Programming

You can write your own programs if you'd like to experiment with programming in
my modified BCKW calculus. The language is pretty simple.

* Case-insensitive
* Whitespace-insensitive
* Parentheses are used for grouping
* Simple primitives are just `b` `c` `k` `w` `y`.
* Comments are indicated by a `~`, anything on the line after the `~` is ignored.
* Side effecting primitives include the index of the sentence they operate on,
  1-based, not separated by a space e.g. `g1`, `p3`.
* Expressions can be assigned names like so `expr:= b c k w`. Named expressions
  can be used in other expressions by enclosing it in colons like so: `b c :expr:`

Check out the examples directory for example programs.

## Compiling and Running

To run a written program it must first be compiled. You can compile a program
with the following command:

```bash
cabal run Combinatron-Compiler -- input_file_name output_file_name
```

You then run the output file like so:

```bash
cabal run Combinatron-Emulator -- run output_file_name
```

This will run the program and print the final machine state to standard output.

Print intermediate states with:

```bash
cabal run Combinatron-Emulator -- debug output_file_name
```

Get help with:

```bash
cabal run Combinatron-Emulator -- --help
```

## Contributing

You can use Nix and `nix-shell` to get a working environment that can be messed
around in. The project is also buildable with cabal.

The primitive ops are pretty thoroughly tested. The operations defined in
Combinatron.hs are written to closely match the specification, so should be
obviously correct.
