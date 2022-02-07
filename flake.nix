{
  description = "My config";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable-small";

    neovim = {
      url = "github:neovim/neovim?dir=contrib";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager/release-21.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, neovim, ... }:
    let
      system = "x86_64-linux";

      pkgs = import nixpkgs {
        inherit system;
        config = { allowUnfree = true; };
      };

      lib = nixpkgs.lib;
    in {
      homeConfigurations = {
        eli = home-manager.lib.homeManagerConfiguration {
          inherit system pkgs;
          username = "eli";
          homeDirectory = "/home/eli";
          configuration = { imports = [ ./home.nix ]; };
        };
      };

      nixosConfigurations = {
        eliPC = lib.nixosSystem {
          inherit system;

          modules = [ ./modules/common.nix ];
        };
      };

      eli = self.homeConfigurations.eli.activationPackage;
      defaultPackage.x86_64-linux = self.eli;
    };
}
