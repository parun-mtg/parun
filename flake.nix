{
  inputs = {
    cabal-gild.inputs.nixpkgs.follows = "nixpkgs";
    cabal-gild.url = "github:tfausak/cabal-gild-nix";
    ghc-wasm-meta.inputs.nixpkgs.follows = "nixpkgs";
    ghc-wasm-meta.url = "github:haskell-wasm/ghc-wasm-meta";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs =
    {
      cabal-gild,
      ghc-wasm-meta,
      nixpkgs,
      ...
    }:
    let
      forAllSystems = nixpkgs.lib.genAttrs [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];

      inherit (nixpkgs.lib) fileset;

      source = fileset.toSource {
        root = ./.;
        fileset = fileset.unions [
          ./cabal.project
          ./CHANGELOG.md
          ./data
          ./LICENSE.txt
          ./parun.cabal
          ./README.md
          ./source
        ];
      };
    in
    {
      packages = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          parun = pkgs.lib.pipe (pkgs.haskellPackages.callCabal2nix "parun" source { }) [
            pkgs.haskell.lib.compose.doBenchmark
            # Nothing in the tree uses Template Haskell, so the dynamic objects
            # GHC builds alongside the static ones are never loaded. cabal.project
            # says the same thing for local builds; this builder runs Setup
            # directly and never reads that file.
            pkgs.haskell.lib.compose.disableSharedExecutables
            pkgs.haskell.lib.compose.disableSharedLibraries
            (pkgs.haskell.lib.compose.enableCabalFlag "pedantic")
            (pkgs.haskell.lib.compose.overrideCabal (old: {
              postInstall = (old.postInstall or "") + ''
                install -D -m 755 dist/build/parun-benchmark/parun-benchmark "$out/bin/parun-benchmark"
              '';

              testFlags = (old.testFlags or [ ]) ++ [
                "--timeout"
                "120s"
              ];
            }))
          ];
        in
        {
          default = parun;
          benchmark = pkgs.runCommand "parun-benchmark-results" { } ''
            mkdir -p "$out"
            ${parun}/bin/parun-benchmark --json "$out/bench.json" +RTS -T
          '';
        }
      );

      checks = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          cabal = pkgs.runCommand "parun-cabal-check" { nativeBuildInputs = [ pkgs.cabal-install ]; } ''
            cd ${source}
            cabal check --ignore=option-optimize
            touch "$out"
          '';

          gild =
            pkgs.runCommand "parun-gild-check"
              { nativeBuildInputs = [ cabal-gild.packages.${system}.default ]; }
              ''
                cd ${source}
                cabal-gild --mode check parun.cabal
                touch "$out"
              '';

          hlint = pkgs.runCommand "parun-hlint-check" { nativeBuildInputs = [ pkgs.hlint ]; } ''
            cd ${source}
            hlint --hint=${./.hlint.yaml} --threads="$NIX_BUILD_CORES" source
            touch "$out"
          '';

          nixfmt = pkgs.runCommand "parun-nixfmt-check" { nativeBuildInputs = [ pkgs.nixfmt ]; } ''
            nixfmt --check ${./flake.nix}
            touch "$out"
          '';

          ormolu = pkgs.runCommand "parun-ormolu-check" { nativeBuildInputs = [ pkgs.ormolu ]; } ''
            cd ${source}
            find source -name '*.hs' -print0 | xargs -0 ormolu --mode check
            touch "$out"
          '';
        }
      );

      devShells = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          ci = pkgs.mkShell {
            nativeBuildInputs = [
              pkgs.cabal-install
              pkgs.ghc
            ];
          };

          # The GHC wasm backend, for the web client (#8007). Its flavour
          # tracks the native GHC's major version, so both build one set of
          # boot libraries.
          wasm =
            let
              meta = ghc-wasm-meta.packages.${system};
            in
            pkgs.mkShell {
              # The WASI imports a page gives the module; script/build-web.sh
              # copies it into the site.
              BROWSER_WASI_SHIM = pkgs.fetchzip {
                url = "https://registry.npmjs.org/@bjorn3/browser_wasi_shim/-/browser_wasi_shim-0.4.2.tgz";
                hash = "sha256-lfKZk1ZFchBCqon+Pr+PoT1Kb6Hq8O+0NKsalWEfCnI=";
              };

              # Caddy serves the built site over https (script/serve-web.sh).
              nativeBuildInputs = [
                pkgs.caddy
                meta.binaryen
                meta.nodejs
                meta.wasi-sdk
                meta.wasm32-wasi-cabal-9_10
                meta.wasm32-wasi-ghc-9_10
                meta.wasmtime
              ];
            };
        in
        {
          inherit ci wasm;

          default = pkgs.mkShell {
            inputsFrom = [
              ci
              wasm
            ];

            nativeBuildInputs = [
              cabal-gild.packages.${system}.default
              pkgs.bash
              pkgs.coreutils
              pkgs.fzf
              pkgs.gh
              pkgs.ghcid
              pkgs.git
              pkgs.haskellPackages.hasktags
              pkgs.hlint
              pkgs.hooky
              pkgs.jq
              pkgs.nixfmt
              pkgs.ormolu
              pkgs.ripgrep
              pkgs.xdg-utils
            ];

            shellHook = ''
              if test -d .git && ! test -f .git/hooks/pre-commit
              then hooky install
              fi
            '';
          };
        }
      );
    };
}
