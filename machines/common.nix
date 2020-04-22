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
    cv = [
      "v-f-tailed"
      "v-i-italic"
      "v-l-italic"
      "v-k-curly"
      "v-w-curly"
      "v-x-curly"
      "v-y-straight"
      "v-turn-v-curly"
      "v-capital-g-toothless"
      "v-capital-r-curly"
      "v-capital-y-curly"
      "v-nine-turned-six"
      "v-lambda-curly"
      "v-lig-ltgteq-slanted"
    ];
  in {
    enableFontDir = true;
    fontconfig.enable = true;
    fontconfig.defaultFonts = {
      monospace = [ "Iosevka" ];
      sansSerif = [ "Iosevka Sparkle" ];
      serif = [ "Iosevka Etoile" ];
      emoji = [ "Material Icons" ];
    };
    fonts = with pkgs; [
      (iosevka.override {
        set = "custom";
        privateBuildPlan = {
          family = "Iosevka";
          design = [ "common styles" "sans" "ligset-haskell" ] ++ cv;
        };
      })

      (iosevka.override {
        set = "term";
        privateBuildPlan = {
          family = "Iosevka Term";
          design = [ "common styles" "sans" "sp-term" "ligset-haskell" ] ++ cv;
        };
      })

      (iosevka.override {
        set = "fixed";
        privateBuildPlan = {
          family = "Iosevka Fixed";
          design = [ "common styles" "sans" "sp-fixed" ] ++ cv;
        };
      })

      (iosevka.override {
        set = "etoile";
        privateBuildPlan = {
          family = "Iosevka Etoile";
          design = [
            "type"
            "slab"
            "v-at-fourfold"
            "v-j-serifed"
            "no-cv-ss"
            "no-ligation"
          ];
          upright = [ "v-i-serifed" "v-l-serifed" ];
          italic = [ "v-i-italic" "v-l-italic" ];
          oblique = [ "v-i-serifed" "v-l-serifed" ];
          post.design = [ "diversity-1" ];
          widths.normal = {
            shape = 7;
            menu = 5;
            css = "normal";
          };
        };
      })

      (iosevka.override {
        set = "sparkle";
        privateBuildPlan = {
          family = "Iosevka Sparkle";
          design = [
            "type"
            "v-at-fourfold"
            "v-j-narrow-serifed"
            "no-cv-ss"
            "no-ligation"
          ];
          upright = [ "v-i-serifed" "v-l-serifed" "v-f-serifed" "v-r-serifed" ];
          italic = [ "v-i-italic" "v-l-italic" "v-f-tailed" "v-r-top-serifed" ];
          oblique = [ "v-i-serifed" "v-l-serifed" "v-f-serifed" "v-r-serifed" ];
          post.design = [ "diversity-1" ];
          widths.normal = {
            shape = 7;
            menu = 5;
            css = "normal";
          };
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
