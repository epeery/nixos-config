{ config, pkgs, ... }:

{
  imports = [ /etc/nixos/hardware-configuration.nix /etc/nixos/cachix.nix ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp7s0.useDHCP = true;
  networking.networkmanager.enable = true;

  time.timeZone = "America/New_York";

  environment.systemPackages = with pkgs; [ bash git xst ];

  fonts = let
  in {
    enableFontDir = true;
    fontconfig.enable = true;
    fonts = with pkgs; [
      (iosevka.override {
        set = "custom";
        privateBuildPlan = {
          family = "Iosevka";
          design = [ "common styles" "sans" "ligset-haskell" ];
        };
      })

      gohufont
      iosevka
      inter-ui
      emacs-all-the-icons-fonts
    ];
  };

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  services.xserver = {
    enable = true;
    displayManager.startx.enable = true;
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
          sha256 = "1kf1b883iwnx0bqpvyad80vwpnlg912pbr1jmi07lyyy768h2ryn";
        }))
      ];

    };
  };

  programs.zsh.enable = true;

  services.openssh.enable = true;

  system.stateVersion = "19.09";
}
