{ config, pkgs, ... }:

{
  imports = [
    /etc/nixos/hardware-configuration.nix
    ../eli-home.nix
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "eliPC"; # Define your hostname.

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp7s0.useDHCP = true;

  time.timeZone = "America/New_York";

  environment.systemPackages = with pkgs; [
    bash
    xst
  ];

  fonts = {
    enableFontDir = true;
    fontconfig.enable = true;

    fonts = with pkgs; [
      gohufont
      emacs-all-the-icons-fonts
    ];
  };

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable the X11 windowing system.
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";

  services.xserver = {
    enable = true;
    displayManager.startx.enable = true;
  };


  # Enable touchpad support.
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
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
      extraGroups = [ "wheel" ];
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

