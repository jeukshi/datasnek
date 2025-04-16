{
  description = "browser, multiplayer, snake-like game";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem = { self', inputs', pkgs, system, config, lib, ... }: {

        haskellProjects.default = {
          devShell = {
           enable = true;
           tools = hp: { fourmolu = hp.fourmolu; };

           hlsCheck.enable = true;
          };

          autoWire = [ "packages" "apps" "checks" ];
        };
        packages.default = self'.packages.datasnek;

        devShells.default = pkgs.mkShell {
          name = "datasnek";
          meta.description = "datasnek";
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
          ];
          nativeBuildInputs = with pkgs; [
            gnuplot
            just
            zlib
            (python3.withPackages (ps: with ps; [
              requests
              sseclient-py
              html5lib
            ]))
          ];
          shellHook = ''
          '';
        };
      };
    };
}
