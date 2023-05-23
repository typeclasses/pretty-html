{
  inputs = {
    "nixos-22.11".url = "github:NixOS/nixpkgs/nixos-22.11";
    "nixos-unstable".url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = inputs@{ self, ... }:
    let packageName = "pretty-html";
    in inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        nixpkgs = {
          "nixos-22.11" = import inputs."nixos-22.11" { inherit system; };
          "nixos-unstable" = import inputs."nixos-unstable" { inherit system; };
        };
        pkgs = nixpkgs."nixos-22.11";
        project = pkgs.haskellPackages.developPackage {
          root = ./pretty-html;
          name = packageName;
        };
        inherit (pkgs.lib) fold composeExtensions concatMap attrValues;
      in {
        defaultPackage = self.packages.${system}.${packageName};

        packages = {
          "${packageName}" = project;

          testConfigurations = let

            inherit (pkgs.haskell.lib) dontCheck;
            makeTestConfiguration = let defaultPkgs = pkgs;
            in { pkgs ? defaultPkgs, ghcVersion }:
            let inherit (pkgs.haskell.lib) dontCheck packageSourceOverrides;
            in (pkgs.haskell.packages.${ghcVersion}.override (old: {
              overrides = (packageSourceOverrides { pretty-html = ./pretty-html; });
            })).pretty-html;
          in rec {
            ghc-9-0 = makeTestConfiguration {
              pkgs = nixpkgs."nixos-22.11";
              ghcVersion = "ghc90";
            };
            ghc-9-2 = makeTestConfiguration {
              pkgs = nixpkgs."nixos-22.11";
              ghcVersion = "ghc92";
            };
            ghc-9-4 = makeTestConfiguration {
              pkgs = nixpkgs."nixos-22.11";
              ghcVersion = "ghc94";
            };
            ghc-9-6 = makeTestConfiguration {
              ghcVersion = "ghc96";
              pkgs = nixpkgs."nixos-unstable";
            };
            all = pkgs.symlinkJoin {
              name = packageName;
              paths = [ ghc-9-0 ghc-9-2 ghc-9-4 ghc-9-6 ];
            };
          };
        };
      });
}
