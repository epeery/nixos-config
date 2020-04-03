{ pkgs, ... }:

let
  name = "Eli Peery";
  email = "eli.g.peery@gmail.com";

  # Paths
  config = "$HOME/.config";
  share = "$HOME/.local/share";

in
{
  imports = [
    ./scripts.nix
  ];

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.firefox.enableAdobeFlash = false;
  nixpkgs.config.pulseaudio = true;

  # fonts.fontconfig.enable = true;

  programs = {
    home-manager = {
      enable = true;
      path = "$HOME/.config/home-manager";
    };

    zsh = {
      enable = true;
      enableAutosuggestions = true;
      dotDir = ".config/zsh";
      oh-my-zsh = {
        enable = true;
        custom = "${config}/zsh_custom";
        theme = "terminalpartied";
        plugins = [
          "extract"
        ];
      };

      shellAliases = {
        # Modified versions of programs that don't pollute $HOME
        startx = "sx";
        wget = "wget --hsts-file=\"$XDG_CACHE_HOME/wget-hsts\"";

        v = "$EDITOR";
        cfv = "$EDITOR ~/Git/neovim-config/init.vim";

        h = "cd ${config}/nixpkgs";
        P = "cd ~/Projects";
        G = "cd ~/Git";
        D = "cd ~/Downloads";
        d = "cd ~/Documents";
      };
    };

    git = {
      enable = true;
      userName = name;
      userEmail = email;
    };

    direnv.enable = true;

    neovim = {
      enable = true;
      viAlias = true;
      vimAlias = true;
      withNodeJs = true;
    };

    emacs = {
      enable = true;
      extraPackages = epkgs:
        with epkgs; [
          nix-mode
          magit
          evil
        ];
    };
  };

  services = {
    redshift = {
      enable = true;
      latitude = "42.443962";
      longitude = "-76.501884";
      brightness.night = "0.7";
      temperature.night = 4000;
      temperature.day = 6000;
    };

    picom = {
      enable = true;
      blur = true;
    };

    lorri.enable = true;

    dunst = {
      enable = true;
      iconTheme = {
        package = pkgs.papirus-icon-theme;
        name = "papirus";
        size = "32x32";
      };
      settings = {
        global = {
          geometry = "300x5-30+50";
          padding = 32;
          horizontal_padding = 8;
          transparency = 20;
          frame_color = "#CDCBCD";
          separator_color = "frame";
          vertical_alighment = "center";
          font = "Inter 12";
        };

        urgency_low = {
          background = "#CDCBCD";
          foreground = "#000000";
          timeout = 5;
        };

        urgency_normal = {
          background = "#ffffff";
          foreground = "#000000";
          timeout = 10;
        };

        urgency_critical = {
          background = "#D56162";
          foreground = "#ffffff";
        };
      };
    };

    unclutter.enable = true;

    screen-locker = {
      enable = true;
      lockCmd = "\${pkgs.i3lock}/bin/i3lock -n -c CDCBCD";
    };
  };


  xsession = {
    enable = true;
    scriptPath = ".xinitrc";
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = pkgs.writeText "xmonad.hs" (builtins.readFile ./home/xmonad/xmonad.hs);
    };
  };

  xdg = {
    enable = true;
    configFile =
      let
        nvim = builtins.fetchGit {
          url = "https://github.com/epeery/neovim-config";
        };

        vim-plug = builtins.fetchGit {
          url = "https://github.com/junegunn/vim-plug";
          rev = "b6050d6f03f3e2792589535249e3c997d3e94461";
        };
      in
      {
      "wallpaper.png".source = ./config/wallpaper.png;
      "zsh_custom".source = ./config/zsh_custom;
      "pulse/default.pa".source = ./config/pulse/default.pa;
      "wget".source = ./config/wget;
      "npm".source = ./config/npm;
      "fontconfig/fonts.conf".source = ./config/fontconfig/fonts.conf;
      "nvim/init.vim".source = "${nvim}/init.vim";
      "nvim/ftplugin".source = "${nvim}/ftplugin";
      "nvim/coc-settings.json".source = "${nvim}/coc-settings.json";
      "nvim/autoload/plug.vim".source = "${vim-plug}/plug.vim";
    };
  };

  home = {
    file = {
      ".config/Xresources".text = ''
        *foreground:   #ffffff
        *background:   #000000
        *color0:       #000000
        *color1:       #D56162
        *color2:       #83FA62
        *color3:       #D5D2FF
        *color4:       #0081D5
        *color5:       #cd00cd
        *color6:       #00A9AC
        *color7:       #e5e5e5
        *color8:       #7f7f7f
        *color9:       #D56162
        *color10:      #00A900
        *color11:      #D5D2FF
        *color12:      #00A9FF
        *color13:      #AC61FF
        *color14:      #00D2D5
        *color15:      #ffffff
        st*font:       GohuFont:size=11
        st*bold_font:  1
        st.borderpx:   20
      '';

    };

    sessionVariables = {
      # Default tools
      BROWSER = "brave";
      TERMINAL = "xst";
      FILE = "ranger";
      READER = "zathura";
      EDITOR = "nvim";

      # Cleaning up $HOME
      XAUTHORITY="$XDG_RUNTIME_DIR/Xauthority";
      STACK_ROOT="$XDG_DATA_HOME/stack";
      NPM_CONFIG_USERCONFIG= "$XDG_CONFIG_HOME/npm/npmrc";
      LESSHISTFILE="-";
      HISTFILE="${share}/bash/history";
      WGETRC="${config}/wget/wgetrc";
      INPUTRC="${config}/inputrc";
    };

    packages = with pkgs; [
      brave
      cabal-install
      cabal2nix
      dmenu
      electron
      feh
      gimp
      haskellPackages.xmobar
      i3lock
      insomnia
      killall
      libnotify
      mpv
      nitrogen
      nix-prefetch-git
      nodejs
      papirus-icon-theme
      pavucontrol
      ranger
      rofi
      silver-searcher
      slack
      spotify
      sxiv
      unzip
      wget
      xclip
      zathura
      zoom-us
    ];
  };
}
