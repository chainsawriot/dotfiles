alias emacs='emacs -nw'
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
alias rin='rr "devtools::document(); devtools::install(quick = TRUE)"'
alias tma='tmux attach -t '
alias tmn='tmux new -s '
alias gephi='~/tools/gephi-0.9.2/bin/gephi'
alias zotero='~/tools/Zotero_linux-x86_64/zotero'
alias resteco='ssh chainsawriot@resteco.mzes.uni-mannheim.de'
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
