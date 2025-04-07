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
  stringr,   # Needed for string manipulation
  glue,
  dplyr,
  bslib ,     # Using for page_navbar
  reactable,  
  markdown,  # Added for rendering Markdown content in details
  htmltools
)

if(!require("shinychat")){
  pak::pak("posit-dev/shinychat")
}

if(!require("ragnar")){
  pak::pak("tidyverse/ragnar")
}