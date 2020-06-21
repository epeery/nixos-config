{ pkgs, ... }:

let
  home_directory = builtins.getEnv "HOME";
  files = "${home_directory}/files";
in rec {
  imports = [ ./scripts.nix ];

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.pulseaudio = true;

  fonts.fontconfig.enable = true;

  programs = {
    home-manager = {
      enable = true;
      path = "${xdg.configHome}/home-manager";
    };

    git = {
      enable = true;
      userName = "Eli Peery";
      userEmail = "eli.g.peery@gmail.com";
    };

    direnv.enable = true;

    zathura.enable = true;

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
          set undodir=${xdg.configHome}/nvim/undodir

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

          let g:pencil#wrapModeDefault = 'soft'

          " Enable writer mode
          augroup pencil
            autocmd!
            autocmd FileType markdown,mkd call pencil#init()
            autocmd FileType text         call pencil#init()
          augroup END

          " File formatting
          let g:neoformat_haskell_ormolu = { 'exe': 'ormolu', 'args': [] }
          let g:neoformat_enabled_haskell = ['ormolu']
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
            goyo
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
        custom = "${./config/zsh_custom}";
        theme = "terminalpartied";
        plugins = [ "extract" ];
      };

      shellAliases = {
        v = "$EDITOR";
        vx = "$EDITOR ~/.config/nixpkgs/home/xmonad/xmonad.hs";

        h = "cd ${xdg.configHome}/nixpkgs";
        hms = "home-manager switch";
        hme = "home-manager edit";
        P = "cd ${files}/Projects";
        G = "cd ${files}/Git";
        M = "cd ${files}/Music";
        D = "cd ${xdg.userDirs.documents}";
        d = "cd ${xdg.userDirs.download}";

        cp = "cp -iv";

        # Modified versions of programs that don't pollute $HOME
        startx = "sx";
        wget = ''wget --hsts-file="${xdg.cacheHome}/wget-hsts"'';
      };

      initExtra = ''
        [[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec sx

        source ${xdg.configHome}/user-dirs.dirs
      '';
    };

    pazi = {
      enable = true;
      enableZshIntegration = true;
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
        frame-opacity = 0.7;
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
          font = "Inter UI 12";
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
        "${pkgs.i3lock}/bin/i3lock -n -c CDCBCD -i ${xdg.configHome}/lockscreen.png -p win";
      inactiveInterval = 5;
    };

    mpd = {
      enable = true;
      network = {
        listenAddress = "127.0.0.1";
        port = 6600;
      };
      musicDirectory = xdg.userDirs.music;
      extraConfig = ''
        audio_output {
              type            "pulse"
              name            "Local Pulse Audio"
        }
      '';
    };

    clipmenu.enable = true;

    lorri.enable = true;

    udiskie.enable = true;

    unclutter.enable = true;
  };

  xsession = {
    enable = true;
    scriptPath = ".xinitrc";
    initExtra = ''
      if test -z "$DBUS_SESSION_BUS_ADDRESS"; then
          eval $(dbus-launch --exit-with-session --sh-syntax)
      fi
      systemctl --user import-environment DISPLAY XAUTHORITY

      if command -v dbus-update-activation-environment >/dev/null 2>&1; then
              dbus-update-activation-environment DISPLAY XAUTHORITY
      fi
    '';

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config =
        pkgs.writeText "xmonad.hs" (builtins.readFile ./home/xmonad/xmonad.hs);
    };
  };

  xdg = {
    enable = true;
    configHome = "${home_directory}/.config";
    dataHome = "${home_directory}/.local/share";
    cacheHome = "${home_directory}/.cache";

    userDirs = {
      enable = true;
      desktop = "${files}/Desktop";
      documents = "${files}/Documents";
      download = "${files}/Downloads";
      pictures = "${files}/Pictures";
      videos = "${files}/Videos";
      music = "${files}/Music";
      publicShare = "${files}/Public";
      templates = "${files}/Templates";
    };

    configFile = let
      ugly = builtins.fetchGit {
        url = "https://github.com/epeery/vim-ugly";
        rev = "35d269b55c9b4e1649dab497c53d8dcde894bda0";
      };
    in {
      "fontconfig/fonts.conf".source = ./config/fontconfig/fonts.conf;
      "lockscreen.png".source = ./config/lockscreen.png;
      "ncmpcpp/bindings".source = ./config/ncmpcpp/bindings;
      "ncmpcpp/config".source = ./config/ncmpcpp/config;
      "npm".source = ./config/npm;
      "nvim/colors/ugly.vim".source = "${ugly}/colors/ugly.vim";
      "pulse/default.pa".source = ./config/pulse/default.pa;
      "wget".source = ./config/wget;

      "xmobar" = {
        source = ./config/xmobar;
        onChange = ''
          if [[ -v DISPLAY ]] ; then
            $DRY_RUN_CMD xmonad --restart
          fi
        '';
      };

      "wallpaper" = {
        source = ./config/wallpaper;
        onChange = ''
          if [[ -v DISPLAY ]] ; then
            $DRY_RUN_CMD setbg
          fi
        '';
      };

      "Xresources" = {
        text = ''
          *foreground:   #FFFFFF
          *background:   #000000
          *color0:       #000000
          *color1:       #D56162
          *color2:       #83FA62
          *color3:       #D5D2FF
          *color4:       #00A9FF
          *color5:       #AC61FF
          *color6:       #00D2D5
          *color7:       #ACA9AC
          *color8:       #ACA9AC
          *color9:       #D56162
          *color10:      #83FA62
          *color11:      #D5D2FF
          *color12:      #00A9FF
          *color13:      #AC61FF
          *color14:      #00D2D5
          *color15:      #FFFFFF
          st*font:       Iosevka:size=12:antialias=true:autohint=true
          st*opacity:    150
          st*bold_font:  0
          st.borderpx:   20
        '';
        onChange = ''
          if [[ -v DISPLAY ]] ; then
            $DRY_RUN_CMD ${pkgs.xorg.xrdb}/bin/xrdb -merge ${xdg.configHome}/Xresources
          fi
        '';
      };
    };
  };

  home = {
    sessionVariables = {
      # Default tools
      BROWSER = "brave";
      TERMINAL = "xst";
      FILE = "ranger";
      READER = "zathura";
      EDITOR = "nvim";
      MUSIC = "ncmpcpp";

      # Cleaning up $HOME
      XAUTHORITY = "$XDG_RUNTIME_DIR/Xauthority";
      STACK_ROOT = "${xdg.dataHome}/stack";
      NPM_CONFIG_USERCONFIG = "${xdg.configHome}/npm/npmrc";
      LESSHISTFILE = "-";
      HISTFILE = "${xdg.dataHome}/bash/history";
      WGETRC = "${xdg.configHome}/wget/wgetrc";
      INPUTRC = "${xdg.configHome}/inputrc";
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
      imagemagick
      inkscape
      insomnia
      killall
      libnotify
      mpc_cli
      mpv
      ncmpcpp
      niv
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
      yarn
      zoom-us
    ];
  };
}
