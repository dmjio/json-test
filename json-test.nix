{ mkDerivation, aeson, alex, array, base, bytestring, criterion
, happy, hw-json, QuickCheck, quickcheck-instances, scientific
, stdenv, text, unordered-containers, vector
}:
mkDerivation {
  pname = "json-test";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson array base bytestring criterion hw-json QuickCheck
    quickcheck-instances scientific text unordered-containers vector
  ];
  libraryToolDepends = [ alex happy ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/urbint/json-test";
  description = "Testing a faster json parser with happy";
  license = stdenv.lib.licenses.bsd3;
}
