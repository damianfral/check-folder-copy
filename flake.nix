{
  description = "TBD";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
    pre-commit-hooks.url = "github:cachix/git-hooks.nix";
    pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";
    feedback.url = "github:NorfairKing/feedback";
    purescript-overlay.url = "github:thomashoneyman/purescript-overlay";
    purescript-overlay.inputs.nixpkgs.follows = "nixpkgs";
    mkSpagoDerivation.url = "github:jeslie0/mkSpagoDerivation";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    nix-filter,
    pre-commit-hooks,
    feedback,
    ...
  } @ inputs: let
    pkgsFor = system:
      import nixpkgs {
        inherit system;
        overlays = [
          self.overlays.default
          inputs.mkSpagoDerivation.overlays.default
          inputs.purescript-overlay.overlays.default
        ];
      };
    src = nix-filter.lib {
      root = ./.;
      include = [
        "assets/"
        "src/"
        "test/"
        "package.json"
        "yarn.lock"
        "spago.yaml"
        "spago.lock"
        "index.html"
        "vite.config.js"
      ];
    };
  in
    {
      overlays.default = final: prev: rec {
        frontend-purs = final.mkSpagoDerivation {
          name = "check-folder-copy-purs";
          src = src;
          nativeBuildInputs = with final; [purs-unstable spago-unstable esbuild];
          buildPhase = ''
            ls
            spago build --strict
          '';
          installPhase = "ls -l; mkdir $out; cp -r output $out/";
        };
        frontend = final.mkYarnPackage {
          version = "1.0.1";
          name = "frontend";
          yarnLock = ./yarn.lock;
          packageJSON = ./package.json;
          src = src;
          nativeBuildInputs = with final; [cacert yarn nodejs esbuild git tailwindcss_4];
          doDist = false;
          # unpackPhase = ''
          # '';
          configurePhase = ''
            cp -r $src/* .
            cp -r ${frontend-purs}/* .
            ln -s $node_modules node_modules
            export PATH=$PATH:$node_modules/.bin/
          '';
          buildPhase = ''
            set -e
            esbuild --bundle output/Main/index.js \
              --loader:.js=jsx \
              --preserve-symlinks \
              --minify --format=esm \
              --outfile=index.js
            tailwindcss -i ./assets/style.css -o ./tailwind.css
            chmod 777 index.html
            mkdir -p dist
            yarn --offline run vite build
          '';
          installPhase = ''
            mkdir -p $out
            cp -r -t $out/ ./dist/*
          '';
        };
      };
    }
    // flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = pkgsFor system;
        precommitCheck = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            actionlint.enable = true;
            alejandra.enable = true;
            markdownlint.enable = true;
            nil.enable = true;
            ripsecrets.enable = true;
          };
        };
      in rec {
        packages.default = packages.frontend;
        packages.frontend-purs = pkgs.frontend-purs;
        packages.frontend = pkgs.frontend;
        packages.deploy =
          pkgs.writeShellApplication
          {
            name = "deploy-check-folder-copy";
            runtimeInputs = [pkgs.awscli2];
            text = ''
              set -e
              aws s3 cp ${packages.frontend}/index.html \
                s3://check-folder-copy.damianfral.com/ \
                --content-type text/html

              aws s3 cp ${packages.frontend}/main.js \
                s3://check-folder-copy.damianfral.com/ \
                --content-type text/javascript

              aws s3 website s3://check-folder-copy.damianfral.com/ \
                --index-document index.html
            '';
          };
        devShells.default = pkgs.mkShell {
          name = "esphome-dev-shell";
          packages = with pkgs;
          with pkgs.haskellPackages;
            [
              actionlint
              alejandra
              awscli2
              feedback.packages.${system}.default
              nil
              pkgs.helix
              purs
              spago-unstable
              statix
              tailwindcss_4
              tailwindcss-language-server
              typescript-language-server
              vscode-langservers-extracted
              yaml-language-server
            ]
            ++ packages.frontend.nativeBuildInputs;

          inherit (precommitCheck) shellHook;
        };

        checks = {pre-commit-check = precommitCheck;};
      }
    );

  nixConfig = {
    extra-substituters = [
      "https://opensource.cachix.org"
      "https://haskell-language-server.cachix.org"
      "https://feedback.cachix.org"
    ];
    extra-trusted-public-keys = [
      "opensource.cachix.org-1:6t9YnrHI+t4lUilDKP2sNvmFA9LCKdShfrtwPqj2vKc="
      "haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8="
      "feedback.cachix.org-1:8PNDEJ4GTCbsFUwxVWE/ulyoBMDqqL23JA44yB0j1jI="
    ];
  };
}
