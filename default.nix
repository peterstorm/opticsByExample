{ nixpkgs ? import ./nixpkgs.nix
, compiler ? "ghc884"
, doBenchmark ? false }:
let
  inherit (nixpkgs) pkgs;
  inherit (nixpkgs) unstable;
  name = "lensBook";
  haskellPackages = unstable.haskell.packages.${compiler};
  variant = if doBenchmark
            then pkgs.haskell.lib.doBenchmark
            else pkgs.lib.id;
  drv = haskellPackages.callCabal2nix name ./. {};
in
{
  my_project = drv;
  shell = haskellPackages.shellFor {
    packages = p: [drv];
    # packages dependencies (by default haskellPackages)
    buildInputs = with haskellPackages;
      [ hlint
        ghcid
        cabal-install
        cabal2nix
        unstable.haskellPackages.haskell-language-server
        # # if you want to add some system lib like ncurses
        # # you could by writing it like:
        # pkgs.ncurses
      ];
    # nice prompt for the nix-shell
    shellHook = ''
     export PS1="\n\[[${name}:\033[1;32m\]\W\[\033[0m\]]> "
  '';
  };
}
