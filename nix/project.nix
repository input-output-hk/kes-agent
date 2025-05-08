{ repoRoot, inputs, pkgs, system, lib }:

let

  cabalProject' = pkgs.haskell-nix.cabalProject' ({ pkgs, config, ... }:
    let
      # When `isCross` is `true`, it means that we are cross-compiling the project.
      # WARNING You must use the `pkgs` coming from cabalProject' for `isCross` to work.
      isCross = pkgs.stdenv.hostPlatform != pkgs.stdenv.buildPlatform;
    in {
      src = ../.;

      shell.withHoogle = false;

      inputMap = {
        "https://chap.intersectmbo.org/" = inputs.iogx.inputs.CHaP;
      };

      name = "my-project";

      compiler-nix-name = lib.mkDefault "ghc96";
      modules = [
        ({ pkgs, lib, config, ... }:
          let
            postInstallCompletion = exeName:
              "  BASH_COMPLETIONS=$out/share/bash-completion/completions\n  ZSH_COMPLETIONS=$out/share/zsh/site-functions\n  mkdir -p $BASH_COMPLETIONS $ZSH_COMPLETIONS\n  $out/bin/${exeName} --bash-completion-script ${exeName} > $BASH_COMPLETIONS/${exeName}\n  $out/bin/${exeName} --zsh-completion-script ${exeName} > $ZSH_COMPLETIONS/_${exeName}\n";
          in lib.mkIf (!pkgs.stdenv.hostPlatform.isWindows) {
            packages.kes-agent.components.library.build-tools =
              lib.mkForce [ pkgs.git ];

            # Wrap the test binary to have CLI tools in PATH
            packages.kes-agent.components.tests.kes-agent-tests.postInstall =
              "  wrapProgram $out/bin/kes-agent-tests --set PATH ${
                  lib.makeBinPath [
                    config.hsPkgs.kes-agent.components.exes.kes-agent
                    config.hsPkgs.kes-agent.components.exes.kes-agent-control
                    config.hsPkgs.kes-agent.components.exes.kes-service-client-demo
                  ]
                }\n";

            # Add completions for all CLI tools
            packages.kes-agent.components.exes.kes-agent.postInstall =
              postInstallCompletion "kes-agent";
            packages.kes-agent.components.exes.kes-agent-control.postInstall =
              postInstallCompletion "kes-agent-control";
            packages.kes-agent.components.exes.kes-service-client-demo.postInstall =
              postInstallCompletion "kes-service-client-demo";
          })
      ];

      # modules = [
      #   ({ pkgs, lib, config, ... }: {
      #     packages.kes-agent.components.library.build-tools =
      #       lib.mkForce [ pkgs.git ];
      #     packages.kes-agent.components.tests.kes-agent-tests.postInstall = ''
      #       wrapProgram $out/bin/kes-agent-tests --set PATH ${
      #         lib.makeBinPath
      #         [
      #           config.hsPkgs.kes-agent.components.exes.kes-agent
      #           config.hsPkgs.kes-agent.components.exes.kes-agent-control
      #         ]
      #       }
      #     '';
      #   })
      # ];
    });

  cabalProject = cabalProject'.appendOverlays [ ];

  # Docs for mkHaskellProject: https://github.com/input-output-hk/iogx/blob/main/doc/api.md#mkhaskellproject
  project = lib.iogx.mkHaskellProject {
    inherit cabalProject;

    shellArgs = repoRoot.nix.shell;

    # includeMingwW64HydraJobs = false;

    # includeProfiledHydraJobs = false;

    # readTheDocs = {
    #   enable = false;
    #   siteFolder = "doc/read-the-docs-site";
    #   sphinxToolchain = null;
    # };

    # combinedHaddock = {
    #   enable = false;
    #   prologue = "";
    #   packages = [];
    # };
  };

in project
