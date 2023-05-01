options(repos=c("https://cran.rstudio.com/"))
Sys.setenv(R_HISTSIZE='100000')
options(usethis.full_name = "Chung-hong Chan")

warn <- function() {
    print("It's from your .Rprofile. Don't use it in production code.")
}

fuck <- function() {
    if (grepl("there is no package", names(last.warning))) {
        pkg_name <- stringr::str_remove_all(stringr::str_extract(names(last.warning), "‘[a-z\\._A-Z0-9]+’"), "[‘’]")
        install.packages(pkg_name)
        require(pkg_name, character.only = TRUE)
        warn()
    }
}

pwd <- function() {
    getwd()
}

cd <- function(where = here::here()) {
    setwd(where)
}

tt <- function() {
    cd(here::here("tests/testthat"))
}
