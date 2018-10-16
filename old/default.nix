{ mkDerivation, base, filepath, hakyll, hakyll-favicon, stdenv }:
mkDerivation {
  pname = "p7";
  version = "0.2.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base filepath hakyll hakyll-favicon ];
  homepage = "anr.p7.co.nz";
  description = "Post-Netrunner Rotation";
  license = stdenv.lib.licenses.bsd3;
}
