{ compiler ? "ghc821" }:

let
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = new: old: rec {

              tlc =
                new.callPackage ./deps/tlc/tlc.nix { };

              ef =
                new.callPackage ./deps/ef/ef.nix { };

              ef-base =
                new.callPackage ./deps/ef-base/ef-base.nix { };

              trivial =
                new.callPackage ./deps/trivial/trivial.nix { };

              pure =
                new.callPackage ./deps/pure/pure.nix { };

              pure-tagsoup =
                new.callPackage ./pure-tagsoup.nix { };

            };
          };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { ef = pkgs.haskell.packages.${compiler}.ef;
    ef-base = pkgs.haskell.packages.${compiler}.ef-base;
    atomic = pkgs.haskell.packages.${compiler}.atomic;
    tlc = pkgs.haskell.packages.${compiler}.tlc;
    trivial = pkgs.haskell.packages.${compiler}.trivial;
    pure-tagsoup = pkgs.haskell.packages.${compiler}.pure-tagsoup;
  }

