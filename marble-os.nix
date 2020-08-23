{ mkDerivation, attoparsec, base, bytestring, cereal, containers
, cursor, daemons, data-default, hspec, optparse-applicative, pipes
, stdenv, text, time, transformers
}:
mkDerivation {
  pname = "marble-os";
  version = "0.1.2.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base bytestring cereal containers cursor daemons
    data-default optparse-applicative pipes text time transformers
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec ];
  license = stdenv.lib.licenses.mit;
}
