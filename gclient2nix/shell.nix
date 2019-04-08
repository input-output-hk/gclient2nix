let
  self = import ../.;
  pkgs = self.pkgs;
  nix-prefetch-src = pkgs.fetchFromGitHub {
    owner = "samueldr";
    repo = "nix-universal-prefetch";
    rev = "829e7d56510af144ed4c000d378355a0ebae6072";
    sha256 = "09465fpc74g3waxx88yqjk5sj5vs523c0kvil6fkivbwyyjpzmvf";
  };
  nix-prefetch = pkgs.callPackage nix-prefetch-src {};
in self.gclient2nix'.env.overrideAttrs (old: { buildInputs = old.buildInputs ++ [ self.adapter nix-prefetch ]; })
