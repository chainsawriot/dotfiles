# alias e='emacsclient --tty'
alias e='emacsclient --c &'
alias dote='e ~/dev/dotfiles/emacs.org'
alias emacs='e'
if [[ "$OSTYPE" == "linux-gnu" ]]; then
    alias ls='ls --color=auto -l --block-size=M'
else
    alias ls='ls -lartG'
fi
alias c='clear'
alias gpom='git push origin master'
alias gpo='git push origin'
alias gpum='git push upstream master'
alias gdag="git log --all --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit"
alias master='git checkout master'
alias gca='git commit -am'
alias gnbr='git checkout -b'
alias rr='Rscript -e'
alias rdoc='rr "devtools::document()"'
alias rcheck='rr "devtools::check()"'
alias rmegacheck='rr "devtools::check(manual = TRUE, remote = TRUE)"'
alias rtest='rr "devtools::test()"'
alias rsnap='rr "testthat::snapshot_accept()"'
alias pweb='python -m SimpleHTTPServer 8000'
alias refresh='source ~/.zshrc'
alias editalias='e ~/.myaliases.sh'
alias rbuild='rr "devtools::build()"'
alias rspell='rr "devtools::spell_check()"'
alias rrelease='rr "devtools::release()"'
alias rin='rr "devtools::document(); devtools::install(quick = TRUE, upgrade = FALSE)"'
alias tma='tmux attach -t '
alias tmn='tmux new -s '
# alias gephi='~/tools/gephi-0.9.2/bin/gephi'
# alias zotero='~/tools/Zotero_linux-x86_64/zotero'
alias g++-6='g++'
alias killemacs="emacsclient -e '(kill-emacs)'"
alias bfg="java -jar ~/tools/bfg-1.13.0.jar"
alias x="xdg-open"

function rpkg() {
    rr "usethis::create_package(\"`pwd`\")"
    rr "usethis::use_readme_rmd(open = FALSE)"
    rr "usethis::use_testthat()"
    rr "usethis::use_gpl_license(version = 3, include_future = TRUE)"
    git init
}

function rmd() {
    rr "rmarkdown::render('$1', output_format = 'all')"
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

function rt1() {
    rr "devtools::load_all(); Sys.setenv(NOT_CRAN = \"true\"); testthat::test_file('$1')"
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
    ffmpeg -i $1 -vf scale=720:-1 -r 5 -f gif $2
}

function extractbib() {
    rr "condensebib::reduce_bib('$1', master_bib = '/home/chainsawriot/dev/dotfiles/bib.bib', out_bib = '$2')"
}
