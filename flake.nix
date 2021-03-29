{
  description = "KK's collection of flake templates";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = import nixpkgs { inherit system; };

        marp = pkgs.mkYarnPackage {
          name = "marp";
          src = ./.;
          publishBinsFor = [ "@marp-team/marp-cli" ];
        };

        chromium-no-sandbox = pkgs.runCommand "no-sandbox-chromium" {
          nativeBuildInputs = with pkgs; [ makeWrapper ];
        } ''
          mkdir $out
          mkdir $out/bin
          makeWrapper ${pkgs.chromium}/bin/chromium $out/bin/chromium --add-flags "--no-sandbox"
        '';

      in {
        devShell = pkgs.mkShell {
          nativeBuildInputs = with pkgs; [ yarn nodejs chromium-no-sandbox marp ];
        };
      });
}
