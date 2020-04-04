{ ... }:

{
  imports = [
    ./common.nix
  ];

  networking.hostName = "eliPC";

  security.sudo.wheelNeedsPassword = false;
}

