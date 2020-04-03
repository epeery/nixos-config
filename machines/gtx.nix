{ config, pkgs, ... }:

{
  imports = [
    /etc/nixos/hardware-configuration.nix
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "eliPC";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp7s0.useDHCP = true;
  networking.networkmanager.enable = true;

  time.timeZone = "America/New_York";

  environment.systemPackages = with pkgs; [
    bash
    git
    xst
  ];

  fonts = {
    enableFontDir = true;
    fontconfig.enable = true;

    fonts = with pkgs; [
      gohufont
      inter-ui
      emacs-all-the-icons-fonts
    ];
  };

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  services.xserver = {
    enable = true;
    displayManager.startx.enable = true;

    # Enable touchpad support.
    synaptics = {
      enable = true;
      twoFingerScroll = true;
    };
  };

  users = {
    defaultUserShell = "/var/run/current-system/sw/bin/zsh";

    motd = with config; ''
      OS:      NixOS ${system.nixos.release} (${system.nixos.codeName})
      Version: ${system.nixos.version}
      Kernel:  ${boot.kernelPackages.kernel.version}
    '';

    users.eli = {
      isNormalUser = true;
      uid = 1000;
      extraGroups = [ "wheel" "networkmanager" ];
      openssh.authorizedKeys.keys = [
        (builtins.readFile (builtins.fetchurl {
          url = "https://github.com/epeery.keys";
          sha256 = "1kf1b883iwnx0bqpvyad80vwpnlg912pbr1jmi07lyyy768h2ryn";
        }))
    ];

    };
  };

  security.sudo.wheelNeedsPassword = false;

  programs.zsh.enable = true;

  services.openssh.enable = true;

  system.stateVersion = "19.09";
}

