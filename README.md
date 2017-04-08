# Emulator
A software implementation of the BCKW reducer machine defined at
[Combinatron/Specification](https://github.com/Combinatron/Specification).

This is currently only a working prototype and may have bugs. It should be good
enough to play around with though.

## Playing With the Examples

The Main module is currently hardcoded to a specific example program. You can
either change that, or copy what it's doing in a repl session. See also
`loopDebug` to get info on each state of the machine.

## Contributing

You can use Nix and `nix-shell` to get a working environment that can be messed
around in. The project is also buildable with cabal.

The primitive ops are pretty thoroughly tested. The operations defined in
Combinatron.hs are written to closely match the specification, so should be
obviously correct.
