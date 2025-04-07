# This should install all needed packages if not already installed.

if (!require("pacman")) {
  install.packages("pacman")
}

if (!require("pak")) {
  install.packages("pak")
}

pacman::p_load(
  shiny,
  ellmer,
  stringr,   
  glue,
  dplyr,
  duckdb,
  bslib ,     
  reactable,  
  markdown,  
  htmltools,
  DBI
)

if(!require("shinychat")){
  pak::pak("posit-dev/shinychat")
}

if(!require("ragnar")){
  pak::pak("tidyverse/ragnar")
}