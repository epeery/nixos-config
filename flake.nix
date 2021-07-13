{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-20.03";
    home-manager.url = "github:nix-community/home-manager";
  };

  outputs = { self, nixpkgs, home-manager }: {

    nixosConfigurations.container = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ({ pkgs, ... }: {
          boot.isContainer = true;

          # Let 'nixos-version --json' know about the Git revision
          # of this flake.
          system.configurationRevision = nixpkgs.lib.mkIf (self ? rev) self.rev;

          # Network configuration.
          networking.useDHCP = false;
          networking.firewall.allowedTCPPorts = [ 80 ];

          # Enable a web server.
          services.httpd = { enable = true; };
        })

        home-manager.nixosModules.home-manager
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.users.jdoe = import ./home.nix;
        }
      ];
    };

  };
}
