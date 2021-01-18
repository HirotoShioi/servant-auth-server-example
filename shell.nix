{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, bytestring, containers, hpack
      , hspec, hspec-wai, hspec-wai-json, mtl, persistent
      , persistent-mysql, QuickCheck, servant-auth-server, servant-server
      , stdenv, stm, text, uuid, wai, warp
      }:
      mkDerivation {
        pname = "fun-servant";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson base bytestring containers mtl persistent persistent-mysql
          QuickCheck servant-auth-server servant-server stm text uuid wai
          warp
        ];
        libraryToolDepends = [ hpack ];
        executableHaskellDepends = [
          aeson base bytestring containers mtl persistent persistent-mysql
          QuickCheck servant-auth-server servant-server stm text uuid wai
          warp
        ];
        testHaskellDepends = [
          aeson base bytestring containers hspec hspec-wai hspec-wai-json mtl
          persistent persistent-mysql QuickCheck servant-auth-server
          servant-server stm text uuid wai warp
        ];
        prePatch = "hpack";
        homepage = "https://github.com/githubuser/fun-servant#readme";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
