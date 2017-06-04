# Emulator

A software implementation of the BCKW reducer machine defined at
[Combinatron/Specification](https://github.com/Combinatron/Specification). This
implementation may be ahead of the formal specification in some areas.

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

## Compiler

The compiler is at this point naive, simple, and, frankly, bad. My focus right
now is not on making the compiler good, but instead on wrestling the programming
strategies required for programming on an architecture like this. There is one
point to mention though. Usage of a named expression inlines it directly at the
usage site. This is subject to change, as it is a relatively straightforward
matter to replace usages with a pointer to the named expression instead, and
such a strategy would reduce duplication of work during execution. This would
normally cause no problems unless you use some of the side effecting combinators
in a named expression. At this time, those combinators would be executed once
for each usage of the named expression. In the future that will not necessarily
hold.

## Contributing

You can use Nix and `nix-shell` to get a working environment that can be messed
around in. The project is also buildable with cabal.

The primitive ops are pretty thoroughly tested. The operations defined in
Combinatron.hs are written to closely match the specification, so should be
obviously correct.

There are a number of things I am specifically looking for help with. Talk to me
if you're interested in any of these.

* A higher-level language that compiles to the basic combinators.
* Exploration of the possible programming strategies and paradigms for this
  architecture.
* Exploration of the different strategies for distributing work across multiple
  Combinatron processors. I have a number of ideas for this. Talk to me if
  you're interested.
* Exploration of how to wrangle hardware level details like interacting with
  peripherals.
* Strategies for hardware assisted garbage collection.
