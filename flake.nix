{
  description = "check-folder-copy - Frontend with PureScript";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-25.11";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
    pre-commit-hooks.url = "github:cachix/git-hooks.nix";
    pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";
    feedback.url = "github:NorfairKing/feedback";
    purescript-overlay.url = "github:thomashoneyman/purescript-overlay";
    purescript-overlay.inputs.nixpkgs.follows = "nixpkgs";
    mkSpagoDerivation.url = "github:jeslie0/mkSpagoDerivation";
    bun2nix.url = "github:nix-community/bun2nix";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    nix-filter,
    pre-commit-hooks,
    feedback,
    mkSpagoDerivation,
    purescript-overlay,
    bun2nix,
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
        "assets"
        "src"
        "package.json"
        "yarn.lock"
        "spago.yaml"
        "spago.lock"
        "index.html"
        "vite.config.js"
        "bun.lock"
      ];
    };
  in
    {
      overlays.default = final: prev: rec {
        frontend-purs = final.mkSpagoDerivation {
          name = "check-folder-copy-purs";
          src = src;
          nativeBuildInputs = with final; [purs spago esbuild];
          buildPhase = "spago build --strict";
          installPhase = "mkdir $out; cp -r output $out/";
        };
        frontend = let
          bunNix = import ./bun.nix;
          bunDeps = bun2nix.packages.${final.system}.default.fetchBunDeps {bunNix = bunNix;};
        in
          bun2nix.packages.${final.system}.default.mkDerivation {
            pname = "check-folder-copy";
            version = "1.0.0";
            inherit src bunDeps;
            nativeBuildInputs = with final; [frontend-purs git spago purs];
            buildPhase = ''
              set -xue
              export PATH=$PATH:$(pwd)/node_modules/.bin
              cp -r ${frontend-purs}/output ./output
              bun build --minify --outfile=index.js output/Main/index.js
              vite build
              mkdir -p $out
              cp -r dist/* $out/
            '';
            installPhase = "mkdir -p $out";
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
        packages.default = pkgs.frontend;
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
          name = "check-folder-copy";
          packages = with pkgs; [
            actionlint
            alejandra
            feedback.packages.${system}.default
            nil
            purs
            spago
            statix
            tailwindcss_4
            bun
            bun2nix.packages.${system}.default
          ];
          shellHook = precommitCheck.shellHook;
        };
        checks = {pre-commit-check = precommitCheck;};
      }
    );

  nixConfig = {
    extra-substituters = [
      "https://cache.nixos.org"
      "https://cache.garnix.io"
    ];
    extra-trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g="
    ];
  };
}
