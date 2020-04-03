{ config, pkgs, ...}:
{
  imports = [
    (import "${builtins.fetchTarball https://github.com/rycee/home-manager/archive/master.tar.gz}/nixos")
  ];

  home-manager.users.eli = (import ./home.nix);
}
