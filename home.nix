{ pkgs, ... }:

let
  style = import ./style.nix;

  name = "Eli Peery";
  email = "eli.g.peery@gmail.com";

  # Paths
  home = "/home/eli";
  files = "$HOME/files";
in {
  imports = [ ./scripts.nix ];

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.pulseaudio = true;

  fonts.fontconfig.enable = true;

  programs = {
    home-manager = {
      enable = true;
      path = "$XDG_CONFIG_HOME/home-manager";
    };

    git = {
      enable = true;
      userName = name;
      userEmail = email;
    };

    alacritty = {
      enable = true;
      settings = {
        window.padding = {
          x = 20;
          y = 20;
        };
        background_opacity = 0.5;
        colors = {
          cursor = {
            cursor = "#ffffff";
          };
          primary = {
            background = "#000000";
            foreground = "#ffffff";
          };
          normal = {
            black = "#000000";
            red = "#d56162";
            green = "#83fa62";
            yellow = "#d5d2ff";
            blue = "#0081d5";
            magenta = "#cd00cd";
            cyan = "#00a9ac";
            white = "#e5e5e5";
          };
          bright = {
            black = "#7f7f7f";
            red = "#d56162";
            green = "#00a900";
            yellow = "#d5d2ff";
            blue = "#00a9ff";
            magenta = "#ac61ff";
            cyan = "#00d2d5";
            white = "#ffffff";
          };
        };
        font = {
          size = 11;
          normal = {
            family = style.font-term;
            style = "Regular";
          };
          bold = {
            family = style.font-term;
            style = "Bold";
          };
          italic = {
            family = style.font-term;
            style = "Italic";
          };
          bold_italic = {
            family = style.font-term;
            style = "Bold Italic";
          };
        };
      };
    };

    direnv.enable = true;

    neovim = {
      enable = true;
      viAlias = true;
      vimAlias = true;
      withNodeJs = true;
      configure = {
        customRC = ''
          let mapleader=","

          set clipboard=unnamedplus
          set nocompatible

          set undofile
          set undodir=~/.config/nvim/undodir

          set nobackup
          set nowritebackup

          filetype plugin on
          set encoding=utf-8

          colorscheme ugly
          set termguicolors

          set tabstop=4
          set expandtab
          set softtabstop=2
          set shiftwidth=2
          set shiftround

          set splitbelow splitright

          " Make whitespace visible
          set list
          set listchars=tab:··,trail:·

          " Better searching
          set ignorecase
          set smartcase

          " Enable background buffers
          set hidden

          " Enable Normal more inside of :terminal
          tnoremap <Esc> <C-\><C-n>

          " Scroll faster
          nnoremap <C-e> 3<C-e>
          nnoremap <C-y> 3<C-y>

          " Replace all
          nnoremap S :%s//g<Left><Left>

          " Delete whitespace on save
          autocmd BufWritePre * %s/\s\+$//e

          map <leader>f :Files<CR>
          map <leader>b :Buffers<CR>

          " Toggle spellcheck
          map <leader>s :setlocal spell! spelllang=en_us<CR>

          " Prettify
          map <leader>p :Neoformat<CR>

          " Start interactive EasyAlign in visual mode (e.g. vipga)
          xmap ga <Plug>(EasyAlign)

          " Start interactive EasyAlign for a motion/text object (e.g. gaip)
          nmap ga <Plug>(EasyAlign)

          " Center view after search
          noremap <Plug>(slash-after) zz

          " Toggle hidden
          function! ToggleHiddenAll()
              if s:hidden_all  == 0
                  let s:hidden_all = 1
                  set nonumber
                  set relativenumber!
                  set noshowmode
                  set noruler
                  set laststatus=0
                  set noshowcmd
              else
                  let s:hidden_all = 0
                  set number relativenumber
                  set showmode
                  set ruler
                  set laststatus=2
                  set showcmd
              endif
          endfunction
          nnoremap <M-h> :call ToggleHiddenAll()<CR>:<Del>

          let s:hidden_all = 0
          set number relativenumber
          call ToggleHiddenAll()

          " Rearrange lines
          xmap <DOWN> <Plug>(textmanip-move-down)
          xmap <UP> <Plug>(textmanip-move-up)
          xmap <LEFT> <Plug>(textmanip-move-left)
          xmap <RIGHT> <Plug>(textmanip-move-right)
        '';

        packages.customVim = let
          vim-textmanip = pkgs.vimUtils.buildVimPlugin {
            pname = "vim-textmanip";
            version = "2.0";
            src = builtins.fetchGit {
              url = "https://github.com/t9md/vim-textmanip";
              rev = "1948542d12e37f286ef4edd87db4f29e4c7fd771";
            };
          };
        in with pkgs.vimPlugins; {
          start = [
            ReplaceWithRegister
            fzf-vim
            haskell-vim
            neoformat
            targets-vim
            ultisnips
            vim-commentary
            vim-easy-align
            vim-exchange
            vim-nix
            vim-pencil
            vim-slash
            vim-snippets
            vim-surround
            vim-textmanip
          ];
          opt = [ ];
        };
      };
    };

    emacs = {
      enable = true;
      extraPackages = epkgs: with epkgs; [ nix-mode magit evil ];
    };

    zsh = {
      enable = true;
      enableAutosuggestions = true;
      dotDir = ".config/zsh";
      oh-my-zsh = {
        enable = true;
        custom = "$HOME/.config/zsh_custom";
        theme = "terminalpartied";
        plugins = [ "extract" ];
      };

      shellAliases = {
        # Modified versions of programs so they don't pollute $HOME
        startx = "sx";
        wget = ''wget --hsts-file="$XDG_CACHE_HOME/wget-hsts"'';

        v = "$EDITOR";
        m = "$MUSIC";

        h = "cd $HOME/.config/nixpkgs";
        P = "cd $HOME/files/Projects";
        G = "cd $HOME/files/Git";
        D = "cd $XDG_DOCUMENTS_DIR";
        d = "cd $XDG_DOWNLOAD_DIR";
      };

      initExtra = ''
        source $XDG_CONFIG_HOME/user-dirs.dirs
      '';
    };
  };

  services = {
    redshift = {
      enable = true;
      latitude = "42.443962";
      longitude = "-76.501884";
      brightness.night = "0.55";
      temperature.night = 2000;
      temperature.day = 6000;
    };

    picom = let
      picomBlur = pkgs.picom.overrideAttrs (old: rec {
        name = "picom-custom";
        version = "1.0";
        src = builtins.fetchGit {
          url = "https://github.com/tryone144/compton";
          ref = "feature/dual_kawase";
          rev = "c67d7d7b2c36f29846c6693a2f39a2e191a2fcc4";
        };
      });
    in {
      package = picomBlur;
      enable = true;
      blur = true;
      experimentalBackends = true;
      extraOptions = ''
        blur-method = "dual_kawase";
        blur-strength = 10;
      '';
    };

    dunst = {
      enable = true;
      iconTheme = {
        package = pkgs.papirus-icon-theme;
        name = "papirus";
        size = "32x32";
      };
      settings = {
        global = {
          geometry = "400x5-9+30";
          padding = 8;
          horizontal_padding = 8;
          frame_width = 0;
          separator_color = "frame";
          vertical_alighment = "center";
          font = "Suisse Int'l 12";
          word_wrap = "yes";
          line_height = 4;
          icon_position = "left";
          max_icon_size = 128;
        };

        urgency_low = {
          background = "#ffffff";
          foreground = "#000000";
          timeout = 10;
        };

        urgency_normal = {
          background = "#ffffff";
          foreground = "#000000";
          timeout = 10;
        };

        urgency_critical = {
          background = "#D56162";
          foreground = "#ffffff";
          frame_color = "#D56162";
        };
      };
    };

    screen-locker = {
      enable = true;
      lockCmd =
        "${pkgs.i3lock}/bin/i3lock -n -c CDCBCD -i ${home}/.config/lockscreen.png -p win";
      inactiveInterval = 5;
    };

    mpd = {
      enable = true;
      network = {
        listenAddress = "127.0.0.1";
        port = 6600;
      };
      musicDirectory = "${home}/files/Music";
    };

    udiskie.enable = true;

    unclutter.enable = true;

    lorri.enable = true;
  };

  xsession = {
    enable = true;
    scriptPath = ".xinitrc";
    initExtra = "exec dbus-launch --exit-with-session xmonad";

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config =
        pkgs.writeText "xmonad.hs" (builtins.readFile ./home/xmonad/xmonad.hs);
    };
  };

  xdg = {
    enable = true;
    userDirs = {
      enable = true;
      desktop = "$HOME/files/Desktop";
      documents = "$HOME/files/Documents";
      download = "$HOME/files/Downloads";
      pictures = "$HOME/files/Pictures";
      videos = "$HOME/files/Videos";
      music = "$HOME/files/Music";
      publicShare = "$HOME/files/Public";
      templates = "$HOME/files/Templates";
    };

    configFile = let
      ugly = builtins.fetchGit {
        url = "https://github.com/epeery/vim-ugly";
        rev = "9e00e2207adeea1cd237d574f4ca023ba539eb8c";
      };
    in {
      "wallpaper.png".source = ./config/wallpaper.png;
      "lockscreen.png".source = ./config/lockscreen.png;
      "zsh_custom".source = ./config/zsh_custom;
      "pulse/default.pa".source = ./config/pulse/default.pa;
      "ncmpcpp/config".source = ./config/ncmpcpp/config;
      "ncmpcpp/bindings".source = ./config/ncmpcpp/bindings;
      "wget".source = ./config/wget;
      "npm".source = ./config/npm;
      "fontconfig/fonts.conf".source = ./config/fontconfig/fonts.conf;
      "xmobar".source = ./config/xmobar;
      "nvim/colors/ugly.vim".source = "${ugly}/colors/ugly.vim";
    };
  };

  home = {
    sessionVariables = {
      # Default tools
      BROWSER = "brave";
      TERMINAL = "alacritty";
      FILE = "ranger";
      READER = "zathura";
      EDITOR = "nvim";
      MUSIC = "ncmpcpp";

      # Cleaning up $HOME
      XAUTHORITY = "$XDG_RUNTIME_DIR/Xauthority";
      STACK_ROOT = "$XDG_DATA_HOME/stack";
      NPM_CONFIG_USERCONFIG = "$XDG_CONFIG_HOME/npm/npmrc";
      LESSHISTFILE = "-";
      HISTFILE = "$HOME/.local/share/bash/history";
      WGETRC = "$HOME/.config/wget/wgetrc";
      INPUTRC = "$HOME/.config/inputrc";
    };

    packages = let
      dunst = pkgs.dunst.override { dunstify = true; };
      trigger = pkgs.callPackage ./packages/trigger { };
    in with pkgs; [
      brave
      cabal-install
      cabal2nix
      cachix
      dmenu
      dunst
      feh
      ffmpeg
      fzf
      gimp
      haskellPackages.xmobar
      i3lock
      inkscape
      insomnia
      killall
      libnotify
      mpc_cli
      mpv
      ncmpcpp
      nix-prefetch-git
      nixfmt
      nodejs
      pandoc
      papirus-icon-theme
      pavucontrol
      ranger
      ripgrep
      rofi
      slack
      spotify
      sxiv
      texlive.combined.scheme-small
      transmission-gtk
      trigger
      unzip
      wget
      xclip
      yarn
      zathura
      zoom-us
    ];
  };
}
