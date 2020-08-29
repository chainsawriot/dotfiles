alias e='emacs -nw'
alias dote='e ~/dev/dotfiles/emacs.org'
## alias emacs='emacs -nw'
if [[ "$OSTYPE" == "linux-gnu" ]]; then
    alias ls='ls --color=auto -l --block-size=M'
else
    alias ls='ls -lartG'
fi
alias c='clear'
alias gpom='git push origin master'
alias gca='git commit -am'
alias rr='Rscript -e'
alias rdoc='rr "devtools::document()"'
alias rcheck='rr "devtools::check()"'
alias rmegacheck='rr "devtools::check(manual = TRUE, remote = TRUE)"'
alias rtest='rr "devtools::test()"'
alias pweb='python -m SimpleHTTPServer 8000'
alias refresh='source ~/.zshrc'
alias editalias='emacs ~/.myaliases.sh'
alias rbuild='rr "devtools::build()"'
alias rspell='rr "devtools::spell_check()"'
alias rrelease='rr "devtools::release()"'
alias rin='rr "devtools::document(); devtools::install(quick = TRUE, upgrade = FALSE)"'
alias tma='tmux attach -t '
alias tmn='tmux new -s '
alias gephi='~/tools/gephi-0.9.2/bin/gephi'
alias zotero='~/tools/Zotero_linux-x86_64/zotero'
alias makepkg='rr "usethis::create_package(\"`pwd`\")"'
alias g++-6='g++'

function rmd() {
    rr "rmarkdown::render('$1')"
}

function rio() {
    rr "rio::convert('$1', '$2')"
}

function cran() {
    rr "install.packages('$1')"
}

function rh() {
    rr "?$1"
}

function make_mzes() {
    rr "dir.create('$1') ; rmarkdown::draft('$1/index.rmd', template = 'mzesalike', package = 'mzesalike', edit = FALSE)"
}

function papaja() {
    rr "rmarkdown::draft('$1', template = 'apa6', create_dir = FALSE, package = 'papaja', edit = FALSE)"
}

function tangle() {
    emacs --batch -l org --eval "(org-babel-tangle-file \"$1\")"
}

function docx2md() {
    pandoc -s $1 -t markdown --wrap=none -o $2
}

function mov2gif() {
    ffmpeg -i $1 -vf scale=480:-1 -r 5 -f gif $2
}
