{ mkDerivation, base, stdenv, vector, lens, buildTools ? [] }:
mkDerivation {
  pname = "Combinatron-Emulator";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base vector lens ];
  homepage = "http://github.com/Combinatron/Emulator";
  description = "A software implementation of the combinatron virtual machine";
  license = stdenv.lib.licenses.unfree;
  buildTools = buildTools;
}