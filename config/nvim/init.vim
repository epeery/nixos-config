""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                              Eli's VIM Config                              "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"""""""""""""
"  GENERAL  "
"""""""""""""
"" Basic
let mapleader =","

set mouse=a

set hidden

" Persist undo:
" g+ in normal mode to go forward in history
" g- in normal mode to go backward in history
" earlier 20s to go back in time by 20s or earlier 10m to go back in time by 10min etc
" later 20s to go forward in time by 20s or later 10m to go forward in time by 10min etc
" Use undol to get a list of undo changes
set undofile
set undodir=~/.config/nvim/undodir

" Some servers have issues with backup files
set nobackup
set nowritebackup

" set nohlsearch

set clipboard=unnamedplus
set nocompatible

filetype plugin on
set encoding=utf-8

tnoremap <Esc> <C-\><C-n>

" Tab specific option
set tabstop=4                   "A tab is 4 spaces
set expandtab                   "Always uses spaces instead of tabs
set softtabstop=2               "Insert 2 spaces when tab is pressed
set shiftwidth=2                "An indent is 2 spaces
set shiftround                  "Round indent to nearest shiftwidth multiple

" Splits open at the bottom and right, which is non-retarded, unlike vim defaults.
set splitbelow splitright

" Disables automatic commenting on newline:
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

" Enable autocompletion:
set wildmode=longest,list,full

" Show whitespace
set list
" set listchars=trail:•
set listchars=tab:··,trail:·

" Better '/' searching
set ignorecase
set smartcase

" Start scrolling sooner
" set scrolloff=3

" Scroll faster
nnoremap <C-e> 3<C-e>
nnoremap <C-y> 3<C-y>

" Backup directory
set backupdir=~/.config/nvim/vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set directory=~/.config/nvim/vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set backupext=string

"" Folds

" Autofolding .vimrc
function! VimFolds(lnum)
  let s:thisline = getline(a:lnum)
  if match(s:thisline, '^"" ') >= 0
    return '>2'
  endif
  if match(s:thisline, '^""" ') >= 0
    return '>3'
  endif
  let s:two_following_lines = 0
  if line(a:lnum) + 2 <= line('$')
    let s:line_1_after = getline(a:lnum+1)
    let s:line_2_after = getline(a:lnum+2)
    let s:two_following_lines = 1
  endif
  if !s:two_following_lines
    return '='
  endif
else
  if (match(s:thisline, '^"""""') >= 0) &&
        \ (match(s:line_1_after, '^"  ') >= 0) &&
        \ (match(s:line_2_after, '^""""') >= 0)
    return '>1'
  else
    return '='
  endif
endif
endfunction

" defines a foldtext
function! VimFoldText()
  " handle special case of normal comment first
  let s:info = '('.string(v:foldend-v:foldstart).' l)'
  if v:foldlevel == 1
    let s:line = ' ◇ '.getline(v:foldstart+1)[3:-2]
  elseif v:foldlevel == 2
    let s:line = '   ●  '.getline(v:foldstart)[3:]
  elseif v:foldlevel == 3
    let s:line = '     ▪ '.getline(v:foldstart)[4:]
  endif
  if strwidth(s:line) > 80 - len(s:info) - 3
    return s:line[:79-len(s:info)-3+len(s:line)-strwidth(s:line)].'...'.s:info
  else
    return s:line.repeat(' ', 80 - strwidth(s:line) - len(s:info)).s:info
  endif
endfunction

" set foldsettings automatically for vim files
augroup fold_vimrc
  autocmd!
  autocmd FileType vim
        \ setlocal foldmethod=expr |
        \ setlocal foldexpr=VimFolds(v:lnum) |
        \ setlocal foldtext=VimFoldText() |
  "              \ set foldcolumn=2 foldminlines=2
augroup END


"" Toggle hidden
let s:hidden_all = 1

set noshowmode
set noruler
set laststatus=0
set noshowcmd

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

nnoremap <M-h> :call ToggleHiddenAll()<CR>
"""""""""""""
"  PLUGINS  "
"""""""""""""
call plug#begin('~/.config/nvim/plugged')

"" Files
" Plug 'vimwiki/vimwiki'
Plug 'iamcco/markdown-preview.nvim', { 'do': 'cd app & yarn install' }
"" UI
Plug 'epeery/ugly'
Plug 'junegunn/goyo.vim'
Plug 'sheerun/vim-polyglot'
" Plug 'rrethy/vim-hexokinase', { 'do': 'make hexokinase' }
Plug 'chrisbra/Colorizer'
Plug 'reedes/vim-colors-pencil'
Plug 'pgdouyon/vim-yin-yang'
"" Convenience
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'junegunn/vim-easy-align'
" Plug 'vim-scripts/mru.vim'
" Plug 'easymotion/vim-easymotion'
Plug 't9md/vim-textmanip'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'jreybert/vimagit'
Plug 'junegunn/fzf', { 'do': './install --bin' }
Plug 'junegunn/fzf.vim'
Plug 'reedes/vim-pencil'
Plug 'tommcdo/vim-exchange'
Plug 'wellle/targets.vim'
Plug 'junegunn/vim-slash'
Plug 'vim-scripts/ReplaceWithRegister'
Plug 'reedes/vim-wordy'
"" Languages
""" Haskell
Plug 'neovimhaskell/haskell-vim'
" Plug 'enomsg/vim-haskellConcealPlus'
""" JavaScript
Plug 'pangloss/vim-javascript'
Plug 'prettier/vim-prettier', {
  \ 'do': 'yarn install',
  \ 'for': ['javascript', 'typescript', 'css', 'less', 'scss', 'json', 'graphql', 'markdown', 'vue', 'yaml', 'html'] }
""" JSX
Plug 'mxw/vim-jsx'
""" JSON
Plug 'elzr/vim-json'
""" LaTeX
Plug 'lervag/vimtex'
"" Linting / autocompletion
Plug 'neoclide/coc.nvim', {'do': './install.sh nightly'}
Plug 'w0rp/ale'

"" Plug end
call plug#end()
"""""""""""""""""""""
"  PLUGIN SETTINGS  "
"""""""""""""""""""""
"" Snippets
" UltiSnips
let g:UltiSnipsSnippetDirectories = ['~/.config/nvim/UltiSnips', 'UltiSnips']

"" ALE
let g:ale_linters_explicit = 1

" let b:ale_fixers = {'haskell': ['ormolu']}

map <leader>e :ALEDetail<CR>
" map <leader>d :ALEGoToDefinition<CR>

nnoremap <silent> K :call <SID>show_documentation()<CR>

" function! s:show_documentation()
"   if (index(['vim','help'], &filetype) >= 0)
"     execute 'h '.expand('<cword>')
"   else
"     execute 'ALEHover'
"   endif
" endfunction

" Use a slightly slimmer error pointer
let g:ale_sign_error = '✖'
let g:ale_sign_warning = '⚠'

highlight clear ALEErrorSign
highlight clear ALEWarningSign

"" Coc.nvim
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

"" Languages
""" Haskell
let g:haskell_enable_quantification = 1   " to enable highlighting of `forall`
let g:haskell_enable_recursivedo = 1      " to enable highlighting of `mdo` and `rec`
let g:haskell_enable_arrowsyntax = 1      " to enable highlighting of `proc`
let g:haskell_enable_pattern_synonyms = 1 " to enable highlighting of `pattern`
let g:haskell_enable_typeroles = 1        " to enable highlighting of type roles
let g:haskell_enable_static_pointers = 1  " to enable highlighting of `static`
let g:haskell_backpack = 1                " to enable highlighting of backpack keywords
let g:haskell_indent_disable = 1   " to disable automatic indentation

"" Airline
" let g:airline_theme='lucius'
let g:airline_theme='pencil'

"" Goyo
" Goyo plugin makes text more readable when writing prose:
map <leader>y :Goyo \| set linebreak <CR>

" Enable Goyo by default for mutt writting
" Goyo's width will be the line limit in mutt.
autocmd BufRead,BufNewFile /tmp/neomutt* let g:goyo_width=80
autocmd BufRead,BufNewFile /tmp/neomutt* :Goyo \| set bg=light

"" Nerd tree
" let NERDTreeShowHidden=1
" let NERDTreeQuitOnOpen = 1
" let NERDTreeAutoDeleteBuffer = 1
" let NERDTreeMinimalUI = 1
" let NERDTreeDirArrows = 1
" map <C-n> :NERDTreeToggle<CR>
" autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

"" Rearrange lines
xmap <DOWN> <Plug>(textmanip-move-down)
xmap <UP> <Plug>(textmanip-move-up)
xmap <LEFT> <Plug>(textmanip-move-left)
xmap <RIGHT> <Plug>(textmanip-move-right)

xmap <Space>d <Plug>(textmanip-duplicate-down)
nmap <Space>d <Plug>(textmanip-duplicate-down)
xmap <Space>D <Plug>(textmanip-duplicate-up)
nmap <Space>D <Plug>(textmanip-duplicate-up)

nmap <F10> <Plug>(textmanip-toggle-mode)
xmap <F10> <Plug>(textmanip-toggle-mode)

"" EasyAlign
" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

"" Most recently used
" map <leader>f :MRU<CR>

"" FZF
map <leader>f :Files<CR>
map <leader>b :Buffers<CR>

"" Color Highlighting
let g:Hexokinase_refreshEvents = ['BufWritePost'] " Default event to trigger and update
let g:Hexokinase_refreshEvents = ['TextChanged', 'TextChangedI'] " To make it almost live
let g:Hexokinase_virtualText = '██████'
" let g:Hexokinase_highlighters = ['foreground']
" Enable for all filetypes
let g:Hexokinase_ftAutoload = ['*']

" Enable auto-saving for LaTex docs
augroup ft_tex
  au!
  au FileType tex let b:auto_save = 1
augroup END

" Set theme based on terminal
if $TERMINAL_LIGHT == "true"
    set bg=light
    colorscheme pencil
else
    set bg=dark
    colorscheme ugly
endif

set termguicolors

"" Vimtex
let g:tex_flavor='latex'
let g:vimtex_view_method='zathura'
let g:vimtex_quickfix_mode=0
set conceallevel=1
let g:tex_conceal='abdmg'

"" Markdown
let g:markdown_syntax_conceal = 0

"" Vim-Slash
noremap <plug>(slash-after) zz

"""""""""""
"  FILES  "
"""""""""""
"" Xdefaults
" Run xrdb whenever Xdefaults or Xresources are updated.
autocmd BufWritePost ~/.Xresources,~/.Xdefaults !xrdb %

"" Bash shortcuts
" When shortcut files are updated, renew bash and ranger configs with new material:
autocmd BufWritePost ~/.bmdirs,~/.bmfiles !shortcuts

"" Auto-delete whitespace
autocmd BufWritePre * %s/\s\+$//e

"" Cleans build when .tex file is closed.
autocmd VimLeave *.tex !texclear %

"" Manually assign files
let g:vimwiki_ext2syntax = {'.Rmd': 'markdown', '.rmd': 'markdown','.md': 'markdown', '.markdown': 'markdown', '.mdown': 'markdown'}
let g:vimwiki_list = [{'path': '~/vimwiki', 'syntax': 'markdown', 'ext': '.md'}]

autocmd BufRead, BufNewFile *.svelte set ft=html
autocmd BufRead,BufNewFile /tmp/calcurse*,~/.calcurse/notes/* set filetype=markdown
autocmd BufRead,BufNewFile *.ms,*.me,*.mom,*.man set filetype=groff
autocmd BufRead,BufNewFile *.tex set filetype=tex

"" Compile package.yaml with HPack on save
autocmd BufWritePost package.yaml call Hpack()

function Hpack()
  let err = system('hpack ' . expand('%'))
  if v:shell_error
    echo err
  endif
endfunction

""""""""""""""
"  MAPPINGS  "
""""""""""""""
"" Split navigation
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

"" Tab navigation
map  <M-k> :tabnext<CR>
map  <M-j> :tabprevious<CR>

"" Bibliography
" map <leader>b :vsp<space>$BIB<CR>

"" Replace all
nnoremap S :%s//g<Left><Left>

"" Compile document
map <leader>c :w! \| !compiler <c-r>%<CR>

"" Open file preview
map <leader>p :!opout <c-r>%<CR><CR>

"" Improve copy/paste
vnoremap <C-c> "+y
map <C-p> "+P

"" Split mappings
map <leader>v :vsplit<CR>

"" Spell-check set to <leader>o, 'o' for 'orthography':
map <leader>o :setlocal spell! spelllang=en_us<CR>
