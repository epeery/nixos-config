{ ... }:

{
  imports = [ ./common.nix ];

  networking.hostName = "eliLaptop";

  # Enable touchpad support.
  services.xserver.synaptics = {
    enable = true;
    twoFingerScroll = true;
  };
}
