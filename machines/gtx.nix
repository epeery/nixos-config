{ ... }:

{
  imports = [
    ./common.nix
  ];

  networking.hostName = "eliPC";

  boot.plymouth.enable = true;

  security.sudo.wheelNeedsPassword = false;
}

