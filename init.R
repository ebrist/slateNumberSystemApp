# init.R
my_packages = c("stringr", "data.table", "dplyr", "shiny",
                "shinyWidgets", "DT", "waiter", "shinythemes")
install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}
invisible(sapply(my_packages, install_if_missing))