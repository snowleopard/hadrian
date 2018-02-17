# Invoking nix-shell sets up an environment where we can build ghc
# by only invoking hadrian.


{ nixpkgs ? import <nixpkgs> {}
, boot-ghc ? "ghc821" }:

let
  ourtexlive = nixpkgs.texlive.combine
    { inherit (nixpkgs.texlive) scheme-small fncychap; };
  haskellPackages = nixpkgs.haskell.packages.${boot-ghc};
  removeBuild = path: type:
    let baseName = baseNameOf (toString path);
    in
        ! (baseName == "_build"
           || baseName == "dist"
           || baseName == "dist-newstyle"
           || baseName == ".stack-work"
           || baseName == "config.log"
           || baseName == "config.status"
           || baseName == "shell.nix"
           || nixpkgs.lib.hasSuffix ".sh" baseName
           || !(nixpkgs.lib.cleanSourceFilter path type)) ;

  filterSrc = path: builtins.filterSource removeBuild path ;

  hadrianPackages = haskellPackages.override {
    overrides = self: super: let
        localPackage = name: path: self.callCabal2nix name (filterSrc path) {} ;
	noCheck = nixpkgs.haskell.lib.dontCheck ;
      in {
        hadrian = localPackage "hadrian" ./. ;
        shake = noCheck (self.callHackage "shake" "0.16" {}) ;
        Cabal = noCheck (localPackage "Cabal" ./../libraries/Cabal/Cabal) ;
        filepath = noCheck (localPackage "filepath" ./../libraries/filepath) ;
        text = noCheck (localPackage "text" ./../libraries/text) ;
        hpc = noCheck (localPackage "hpc" ./../libraries/hpc) ;
        parsec = noCheck (localPackage "parsec" ./../libraries/parsec) ;
        HUnit = noCheck (self.callHackage "HUnit" "1.3.1.2" {}) ;
        process = noCheck (localPackage "process" ./../libraries/process) ;
        directory = noCheck (localPackage "directory" ./../libraries/directory) ;
      }; };

in
  nixpkgs.lib.overrideDerivation
    (nixpkgs.haskell.compiler.ghcHEAD.override {
      bootPkgs = haskellPackages;
    })
    (drv: {
      name = "ghc-dev";
      buildInputs = drv.buildInputs ++ [
                    hadrianPackages.hadrian
                    nixpkgs.arcanist
                    haskellPackages.alex
                    haskellPackages.happy
                    nixpkgs.python3
                    nixpkgs.git
                    nixpkgs.autoconf
                    nixpkgs.automake
                    nixpkgs.perl
                    nixpkgs.gcc
                    nixpkgs.python3Packages.sphinx
                    nixpkgs.ncurses
                    nixpkgs.m4
                    nixpkgs.gmp
                    nixpkgs.file
                    nixpkgs.llvm_5
		    ourtexlive
		  ];
  })
