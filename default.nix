{ mkDerivation, base, pure-core, pure-txt, stdenv, tagsoup }:
mkDerivation {
  pname = "pure-tagsoup";
  version = "0.7.0.0";
  src = ./.;
  libraryHaskellDepends = [ base pure-core pure-txt tagsoup ];
  homepage = "github.com/grumply/pure-tagsoup";
  license = stdenv.lib.licenses.bsd3;
}
