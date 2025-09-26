{ inputs, pkgs, lib, project, utils, ghc }:

let

  allTools = {
    "ghc967".cabal = project.projectVariants.ghc967.tool "cabal" "latest";
    "ghc967".cabal-fmt = project.projectVariants.ghc967.tool "cabal-fmt" "latest";
    "ghc967".haskell-language-server = project.projectVariants.ghc967.tool "haskell-language-server" "latest";
    "ghc967".stylish-haskell = project.projectVariants.ghc967.tool "stylish-haskell" "latest";
    "ghc967".fourmolu = project.projectVariants.ghc967.tool "fourmolu" "0.16.2.0";
    "ghc967".hlint = project.projectVariants.ghc967.tool "hlint" "latest";
  };

  tools = allTools.${ghc};

  preCommitCheck = inputs.pre-commit-hooks.lib.${pkgs.system}.run {

    src = lib.cleanSources ../.;

    hooks = {
      nixpkgs-fmt = {
        enable = true;
        package = pkgs.nixpkgs-fmt;
      };
      cabal-fmt = {
        enable = false;
        package = tools.cabal-fmt;
      };
      stylish-haskell = {
        enable = false;
        package = tools.stylish-haskell;
        args = [ "--config" ".stylish-haskell.yaml" ];
      };
      fourmolu = {
        enable = true;
        package = tools.fourmolu;
        # this will add these args duplicate args = [ "--mode" "inplace" ];
        # see https://github.com/input-output-hk/iogx/issues/116
        args = [ ];
      };
      hlint = {
        enable = false;
        package = tools.hlint;
        args = [ "--hint" ".hlint.yaml" ];
      };
      shellcheck = {
        enable = false;
        package = pkgs.shellcheck;
      };
    };
  };

  linuxPkgs = lib.optionals pkgs.hostPlatform.isLinux [
  ];

  darwinPkgs = lib.optionals pkgs.hostPlatform.isDarwin [
  ];

  commonPkgs = [
    tools.haskell-language-server
    tools.stylish-haskell
    tools.fourmolu
    tools.cabal
    tools.hlint
    tools.cabal-fmt

    pkgs.shellcheck
    pkgs.nixpkgs-fmt
    pkgs.github-cli
    pkgs.act
    pkgs.bzip2
    pkgs.gawk
    pkgs.zlib
    pkgs.cacert
    pkgs.curl
    pkgs.bash
    pkgs.git
    pkgs.which
  ];

  shell = project.shellFor {
    name = "kes-agent-shell-${project.args.compiler-nix-name}";

    buildInputs = lib.concatLists [
      commonPkgs
      darwinPkgs
      linuxPkgs
    ];

    withHoogle = true;

    shellHook = ''
      ${preCommitCheck.shellHook}
      export PS1="\n\[\033[1;32m\][nix-shell:\w]\$\[\033[0m\] "
    '';
  };

in

shell
