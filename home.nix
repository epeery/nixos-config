{ pkgs, ... }:

let
  pkgsUnstable = import <nixpkgs> { };
  home_directory = builtins.getEnv "HOME";
in rec {
  imports = [ ./scripts.nix ];

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.pulseaudio = true;
  nixpkgs.overlays = [
    (self: super: {
      dunst = pkgsUnstable.dunst.overrideAttrs
        (old: {
          name = "dunst-custom";
          version = "1.0";
          src = builtins.fetchGit {
            url = "https://github.com/dunst-project/dunst";
            rev = "0de8610b6715697cc044816358193856115dff40";
          };
        });
    })
  ];

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
      extraConfig = ''
        let mapleader=","

        set clipboard=unnamedplus
        set nocompatible

        set undofile
        set undodir=${xdg.configHome}/nvim/undodir

        set nobackup
        set nowritebackup

        filetype plugin on
        set encoding=utf-8

        colorscheme new
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

        " Disable automatic commenting:
        autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

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

        " ALE
        let g:ale_linters_explicit = 1

        map <leader>e :ALEDetail<CR>

        let g:ale_sign_error = '✖ '
        let g:ale_sign_warning = '⚠ '
        highlight clear ALEErrorSign
        highlight clear ALEWarningSign

        " Coc.nvim
        " Use K to show documentation in preview window
        nnoremap <silent> K :call <SID>show_documentation()<CR>

        function! s:show_documentation()
          if (index(['vim','help'], &filetype) >= 0)
            execute 'h '.expand('<cword>')
          else
            call CocActionAsync('doHover')
          endif
        endfunction

        " Remap for do codeAction of selected region, ex: `<leader>aap` for current paragraph
        xmap <leader>a  <Plug>(coc-codeaction-selected)
        nmap <leader>a  <Plug>(coc-codeaction-selected)

        " Remap for do codeAction of current line
        nmap <leader>ac  <Plug>(coc-codeaction)

        " Remap keys for gotos
        nmap <silent> <leader>d <Plug>(coc-definition)
        nmap <silent> <leader>r <Plug>(coc-references)

        " Remap for rename current word
        nmap <leader>rn <Plug>(coc-rename)
      '';

      plugins = let
        vim-textmanip = pkgs.vimUtils.buildVimPlugin {
          pname = "vim-textmanip";
          version = "2.0";
          src = builtins.fetchGit {
            url = "https://github.com/t9md/vim-textmanip";
            rev = "1948542d12e37f286ef4edd87db4f29e4c7fd771";
          };
        };

        vim-hexokinase = pkgs.vimUtils.buildVimPlugin {
          pname = "vim-hexokinase";
          version = "1.0";
          src = builtins.fetchGit {
            url = "https://github.com/RRethy/vim-hexokinase";
            rev = "1788753bd7eb713f1eab089796e1b70c2e410ec5";
          };
          buildPhase = let
            hexokinase = pkgs.buildGoPackage {
              name = "hexokinase";
              version = "1.0";
              goPackagePath = "github.com/RRethy/hexokinase";
              src = builtins.fetchGit {
                url = "https://github.com/RRethy/hexokinase";
                rev = "b3057127451ab2ca9b7011c76e84b29fd44b703f";
              };
            };
          in ''
            cp ${hexokinase}/bin/hexokinase ./hexokinase/hexokinase
          '';
        };

        markdown-preview = let
          src = pkgs.fetchFromGitHub {
            owner = "iamcco";
            repo = "markdown-preview.nvim";
            rev = "d319eaac9ef155d2e2cb846c6754a22e9e8a494a";
            sha256 = "1mch9s2fbk068q2hrxz5ksrw4b15h8vdxg1zi9ads9s4v8rb3hcg";
          };
          app = pkgs.mkYarnPackage {
            name = "markdown-preview-app";
            src = "${src}/app";
            packageJSON = ./plugins/markdown-preview/package.json;
            yarnLock = ./plugins/markdown-preview/yarn.lock;
            yarnNix = ./plugins/markdown-preview/yarn.nix;
            installPhase = ''
              mkdir $out
              mv node_modules $out/node_modules
              mv deps $out/deps
            '';
            distPhase = ''
              true
            '';
          };

        in pkgs.vimUtils.buildVimPluginFrom2Nix {
          pname = "markdown-preview";
          version = "2020-09-09";
          src = src;
          patches = [
            (pkgs.substituteAll {
              src = ./plugins/markdown-preview/node-path.patch;
              node = "${pkgs.nodejs}/bin/node";
            })
          ];
          buildPhase = ''
            rm -r app
            ln -s ${app}/deps/markdown-preview-vim app
          '';
        };
      in with pkgs.vimPlugins; [
        ReplaceWithRegister
        ale
        coc-nvim
        fzf-vim
        goyo
        haskell-vim
        markdown-preview
        matchit-zip
        neoformat
        targets-vim
        typescript-vim
        ultisnips
        vim-commentary
        vim-easy-align
        vim-exchange
        vim-hexokinase
        vim-javascript
        vim-jsx-typescript
        vim-nix
        vim-pencil
        vim-repeat
        vim-slash
        vim-snippets
        vim-surround
        vim-textmanip
      ];
    };

    emacs = {
      enable = true;
      extraPackages = epkgs: with epkgs; [ nix-mode magit evil ];
    };

    zsh = {
      enable = true;
      enableAutosuggestions = true;
      dotDir = ".config/zsh";
      autocd = true;

      shellAliases = {
        v = "$EDITOR";
        vx = "$EDITOR ~/.config/nixpkgs/home/xmonad/xmonad.hs";
        td = "cd $TODOS; bash ./todos";

        h = "cd ${xdg.configHome}/nixpkgs";
        hms = "home-manager switch";
        hme = "home-manager edit";

        P = "cd ${home_directory}/Projects";
        G = "cd ${home_directory}/Git";
        M = "cd ${home_directory}/Music";
        V = "cd ${home_directory}/Videos";
        D = "cd ${xdg.userDirs.download}";
        d = "cd ${xdg.userDirs.documents}";

        cp = "cp -iv";

        # Modified versions of programs that don't pollute $HOME
        startx = "sx";
        wget = ''wget --hsts-file="${xdg.cacheHome}/wget-hsts"'';
      };

      initExtra = ''
        # [[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec sx

        source ${xdg.configHome}/user-dirs.dirs

        setopt PROMPT_SUBST
        autoload colors
        colors

        bindkey "^A" vi-beginning-of-line
        bindkey "^E" vi-end-of-line

        # Taken from https://github.com/ohmyzsh/ohmyzsh/blob/master/lib/git.zsh

        function __git_prompt_git() {
          GIT_OPTIONAL_LOCKS=0 command git "$@"
        }

        function git_prompt_info() {
          local ref
          if [[ "$(__git_prompt_git config --get oh-my-zsh.hide-status 2>/dev/null)" != "1" ]]; then
            ref=$(__git_prompt_git symbolic-ref HEAD 2> /dev/null) || \
            ref=$(__git_prompt_git rev-parse --short HEAD 2> /dev/null) || return 0
            echo "$ZSH_THEME_GIT_PROMPT_PREFIX''${ref#refs/heads/}$(parse_git_dirty)$ZSH_THEME_GIT_PROMPT_SUFFIX"
          fi
        }

        # Checks if working tree is dirty
        function parse_git_dirty() {
          local STATUS
          local -a FLAGS
          FLAGS=('--porcelain')
          if [[ "$(__git_prompt_git config --get oh-my-zsh.hide-dirty)" != "1" ]]; then
            if [[ "''${DISABLE_UNTRACKED_FILES_DIRTY:-}" == "true" ]]; then
              FLAGS+='--untracked-files=no'
            fi
            case "''${GIT_STATUS_IGNORE_SUBMODULES:-}" in
              git)
                # let git decide (this respects per-repo config in .gitmodules)
                ;;
              *)
                # if unset: ignore dirty submodules
                # other values are passed to --ignore-submodules
                FLAGS+="--ignore-submodules=''${GIT_STATUS_IGNORE_SUBMODULES:-dirty}"
                ;;
            esac
            STATUS=$(__git_prompt_git status ''${FLAGS} 2> /dev/null | tail -n1)
          fi
          if [[ -n $STATUS ]]; then
            echo "$ZSH_THEME_GIT_PROMPT_DIRTY"
          else
            echo "$ZSH_THEME_GIT_PROMPT_CLEAN"
          fi
        }

        PROMPT='%(?,%{$fg[cyan]%},%{$fg[red]%})  '
        RPS1='%{$fg_bold[black]%}%2~/$(git_prompt_info)%{$reset_color%}'

        ZSH_THEME_GIT_PROMPT_PREFIX=" %{$fg[white]%}"
        ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
        ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[cyan]%}  "
        ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[red]%}  "
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
      brightness.night = "0.75";
      temperature.night = 3000;
      temperature.day = 6000;
    };

    picom = let
      picomBlur = pkgs.picom.overrideAttrs (old: rec {
        name = "picom-custom";
        version = "1.0";
        src = builtins.fetchGit {
          url = "https://github.com/ibhagwan/picom";
          ref = "next";
          rev = "82ecc90b51fa2489d26ef3f08abe1f06efcb53d8";
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
        background = false;
        background-frame = false;
        background-fixed = false;
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
          allow-markup = "yes";
          geometry = "400x5-13+41";
          padding = 7;
          horizontal_padding = 7;
          frame_width = 7;
          frame_color = "#ffffff20";
          separator_color = "frame";
          vertical_alighment = "center";
          font = "Inter UI 12";
          word_wrap = "yes";
          line_height = 4;
          icon_position = "left";
          max_icon_size = 128;
        };

        urgency_low = {
          background = "#ffffff50";
          foreground = "#00000";
          timeout = 10;
        };

        urgency_normal = {
          background = "#ffffff50";
          foreground = "#000000";
          timeout = 10;
        };

        urgency_critical = {
          background = "#ffffff50";
          foreground = "#000000";
        };
      };
    };

    screen-locker = {
      enable = false;
      lockCmd = "${pkgs.betterlockscreen}/bin/betterlockscreen -l";
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

    fluidsynth.enable = true;

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

    windowManager = {
      xmonad = {
        enable = true;
        enableContribAndExtras = true;
        config = pkgs.writeText "xmonad.hs"
          (builtins.readFile ./home/xmonad/xmonad.hs);
      };
    };
  };

  xdg = {
    enable = true;
    configHome = "${home_directory}/.config";
    dataHome = "${home_directory}/.local/share";
    cacheHome = "${home_directory}/.cache";

    userDirs = {
      enable = true;
      desktop = "${home_directory}/Desktop";
      documents = "${home_directory}/Documents";
      download = "${home_directory}/Downloads";
      pictures = "${home_directory}/Pictures";
      videos = "${home_directory}/Videos";
      music = "${home_directory}/Music";
      publicShare = "${home_directory}/Public";
      templates = "${home_directory}/Templates";
    };

    configFile = let
      ugly = builtins.fetchGit {
        url = "https://github.com/epeery/vim-ugly";
        rev = "493a5a6ee10b30bbf494475cf3a34cf8dc5de1b3";
      };
    in {
      "fontconfig/fonts.conf".source = ./config/fontconfig/fonts.conf;
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
            $DRY_RUN_CMD xmonad --restart
          fi
        '';
      };

      "Xresources" = {
        text = ''
          ! special
          *foreground:    #FFFFFF
          *background:    #312968
          *cursorColor:   #FFFFFF

          ! black
          *color0:        #000000
          *color8:        #ACA9AC

          ! red
          *color1:        #F45B69
          *color9:        #F45B69

          ! green
          *color2:        #40F99B
          *color10:       #40F99B

          ! yellow
          *color3:        #F9DC5C
          *color11:       #F9DC5C

          ! blue
          *color4:        #0ABEFF
          *color12:       #0ABEFF

          ! magenta
          *color5:        #C65CFD
          *color13:       #C65CFD

          ! cyan
          *color6:        #14FFF7
          *color14:       #14FFF7

          ! white
          *color7:        #ACA9AC
          *color15:       #FFFFFF
          st*font:        Iosevka:size=12:antialias=true:autohint=true
          st*opacity:     225
          st*disablebold: 1
          st.borderpx:    20
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

      # Common locations
      TODOS = "~/Documents/todos";

      # Cleaning up $HOME
      # XAUTHORITY = "$XDG_RUNTIME_DIR/Xauthority";
      STACK_ROOT = "${xdg.dataHome}/stack";
      NPM_CONFIG_USERCONFIG = "${xdg.configHome}/npm/npmrc";
      LESSHISTFILE = "-";
      HISTFILE = "${xdg.dataHome}/bash/history";
      WGETRC = "${xdg.configHome}/wget/wgetrc";
      INPUTRC = "${xdg.configHome}/inputrc";

      # Other variables
      FZF_DEFAULT_COMMAND =
        "rg --files --follow --no-ignore-vcs --hidden -g '!{node_modules/*,.git/*}'";
    };

    packages = let trigger = pkgs.callPackage ./packages/trigger { };
    in with pkgs; [
      # betterlockscreen
      binutils
      brave
      cabal-install
      cabal2nix
      cachix
      cinnamon.nemo
      dmenu
      dunst
      feh
      ffmpeg
      fzf
      gimp
      haskellPackages.xmobar
      imagemagick
      inkscape
      insomnia
      kdenlive
      killall
      libnotify
      mpc_cli
      mpv
      ncmpcpp
      niv
      nix-prefetch-git
      nixfmt
      nodePackages.prettier
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
      tor-browser-bundle-bin
    ];
  };
}
