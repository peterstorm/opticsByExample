let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  self = {
    inherit pkgs;
  };
in
 self
