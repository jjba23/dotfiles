# Joe's dotfiles
# Copyright (C) 2023  Josep Jesus Bigorra Algaba (jjbigorra@gmail.com)

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

{
  description = "Joe's NixOS configuration flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "/nixpkgs";
    };
  };

  outputs = { self, nixpkgs, sops-nix, home-manager, ... }@inputs:
    let
      inherit (nixpkgs) lib;
      inherit (self) outputs;
      my-home-manager = {
        home-manager = {
          backupFileExtension = "backup";
          verbose = true;
          useGlobalPkgs = true;
          useUserPackages = true;
          users = {
            joe = if true then
              import ./users/joe/home.nix { inherit inputs lib; }
            else
              { };
            manon = if true then import ./users/manon/home.nix else { };
            claudio = if true then import ./users/claudio/home.nix else { };
          };
        };
      };
      flakeModules = [
        (import ./configuration.nix)
        sops-nix.nixosModules.sops
        home-manager.nixosModules.home-manager
        my-home-manager
      ];

      allSystems = [ "x86_64-linux" "aarch64-linux" ];

      forAllSystems = fn:
        nixpkgs.lib.genAttrs allSystems
        (system: fn { pkgs = import nixpkgs { inherit system; }; });

    in {
      nixosConfigurations.nixos = nixpkgs.lib.nixosSystem {
        specialArgs = { inherit inputs outputs nixpkgs; };
        modules = flakeModules;
      };
      devShells = forAllSystems ({ pkgs }: {
        default = pkgs.mkShell {
          nativeBuildInputs = with pkgs; [
            stack
            cabal-install
            ghc
            haskell-language-server
            gnumake
            cachix
            ormolu
            nixfmt
            ghcid
            statix
            deadnix
            jq
            awscli2
          ];
        };
      });
    };
}

