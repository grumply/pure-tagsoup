{ mkDerivation, base, containers, ef, ef-base, pure, stdenv
, tagsoup
}:
mkDerivation {
  pname = "pure-tagsoup";
  version = "0.6.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base containers ef ef-base pure tagsoup
  ];
  description = "Tagsoup compatability for pure";
  license = stdenv.lib.licenses.bsd3;
}
