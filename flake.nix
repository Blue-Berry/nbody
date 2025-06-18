{
  description = "Simulate n-body problem";

  inputs = {
    nixpkgs.url = "github:nix-ocaml/nix-overlays";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs @ {flake-parts, ...}:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin"];

      perSystem = {
        self',
        pkgs,
        ...
      }: let
        inherit (pkgs) mkShell;
        ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_3;
        inherit (ocamlPackages) buildDunePackage;
        name = "nbody";
        version = "0.0.1";
      in {
        devShells = {
          default = mkShell {
            inputsFrom = [self'.packages.default];
            buildInputs = [ocamlPackages.utop ocamlPackages.ocaml-lsp ocamlPackages.ocamlformat ];
          };
        };

        packages = {
          default = buildDunePackage {
            inherit version;
            pname = name;
            src = ./.;
            buildInputs = with ocamlPackages; [
                eio
                graphics
                owl
                ppx_jane
            ];
          };
        };
      };
    };
}
