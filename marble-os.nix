{ mkDerivation, attoparsec, base, bytestring, hspec
, optparse-applicative, stdenv, text, time
}:
mkDerivation {
  pname = "marble-os";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base bytestring optparse-applicative text time
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec ];
  license = stdenv.lib.licenses.mit;
}
