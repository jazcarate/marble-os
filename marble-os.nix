{ mkDerivation, attoparsec, base, bytestring, cereal, containers
, daemons, data-default, editor-open, hspec, microlens
, optparse-applicative, pipes, stdenv, text, time, transformers
}:
mkDerivation {
  pname = "marble-os";
  version = "0.1.3.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base bytestring cereal containers daemons data-default
    editor-open microlens optparse-applicative pipes text time
    transformers
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec ];
  license = stdenv.lib.licenses.mit;
}
