{ config, pkgs, ... }:

{
  imports = [ /etc/nixos/hardware-configuration.nix /etc/nixos/cachix.nix ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.supportedFilesystems = [ "ntfs" ];

  # fileSystems."/path/to/mount/to" = {
  #   device = "/path/to/the/device";
  #   fsType = "ntfs";
  #   options = [ "rw" "uid=theUidOfYourUser" ];
  # };

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp7s0.useDHCP = true;
  networking.networkmanager.enable = true;
  networking.hostName = "eliPC";

  time.timeZone = "America/New_York";

  nix.package = pkgs.nixUnstable;
  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

  environment.systemPackages = with pkgs; [ bash git xst ];

  fonts = let
  in {
    fontDir.enable = true;
    fontconfig.enable = true;
    fonts = with pkgs; [
      # (iosevka.override {
      #   set = "custom";
      #   privateBuildPlan = {
      #     family = "Iosevka";
      #     design = [ "common styles" "sans" "ligset-haskell" ];
      #   };
      # })

      gohufont
      iosevka
      inter-ui
      emacs-all-the-icons-fonts
    ];
  };

  sound.enable = true;

  hardware.pulseaudio = { enable = true; };
  hardware.opengl.enable = true;

  services.xserver = {
    enable = true;

    displayManager.startx.enable = false;

    displayManager.lightdm = {
      enable = true;
      background = ../config/wallpaper;
    };

    windowManager.session = [{
      name = "home-manager";
      start = ''
        ${pkgs.runtimeShell} $HOME/.xinitrc &
        waitPID=$!
      '';
    }];
    displayManager.defaultSession = "none+home-manager";
  };

  users = {
    defaultUserShell = "/var/run/current-system/sw/bin/zsh";

    motd = with config; ''

        \\  \\ //
       ==\\__\\/ //    Welcome back, Eli
         //   \\//     OS:      NixOS ${system.nixos.release} (${system.nixos.codeName})
      ==//     //==    Version: ${system.nixos.version}
       //\\___//       Kernel:  ${boot.kernelPackages.kernel.version}
      // /\\  \\==
        // \\  \\


    '';

    users.eli = {
      isNormalUser = true;
      uid = 1000;
      extraGroups = [ "wheel" "networkmanager" ];
      openssh.authorizedKeys.keys = [
        (builtins.readFile (builtins.fetchurl {
          url = "https://github.com/epeery.keys";
          sha256 = "0mdqa9w1p6cmli6976v4wi0sw9r4p5prkj7lzfd1877wk11c9c73";
        }))
      ];

    };
  };

  nix.trustedUsers = [ "root" "@wheel" ];

  programs.zsh.enable = true;

  services.openssh.enable = true;

  system.stateVersion = "19.09";
}
