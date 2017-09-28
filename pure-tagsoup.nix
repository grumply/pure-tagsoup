{ mkDerivation, pure, base, containers, ef-base, ef, tagsoup, stdenv
}:
mkDerivation {
  pname = "atomic-tagsoup";
  version = "0.6.0.0";
  src = ./.;
  libraryHaskellDepends = [
    pure base containers ef-base ef tagsoup
  ];
  license = stdenv.lib.licenses.bsd3;
}
