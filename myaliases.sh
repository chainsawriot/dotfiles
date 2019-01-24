alias emacs='emacs -nw'
alias ls='ls --color=auto -l --block-size=M'
alias c='clear'
alias l='ls -lart'
alias gpom='git push origin master'
alias gca='git commit -am'
alias rr='Rscript -e'
alias rdoc='rr "devtools::document()"'
alias rcheck='rr "devtools::check()"'
alias rtest='rr "devtools::test()"'
alias pweb='python -m SimpleHTTPServer 8000'
alias refresh='source ~/.zshrc'
alias editalias='emacs ~/.myaliases.sh'
alias rbuild='rr "devtools::build()"'
alias rin='rr "devtools::install()"'
alias tma='tmux attach -t '
alias tmn='tmux new -s '

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
