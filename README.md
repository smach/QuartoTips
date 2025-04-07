# Shiny app for Quarto Tips that's searchable with an LLM

This app uses the [ragnar](https://tidyverse.github.io/ragnar/), [ellmer](https://ellmer.tidyverse.org/), and [shinychat](https://posit-dev.github.io/shinychat/) R packages to create an [R Shiny Web app](https://shiny.posit.co/r/getstarted/shiny-basics/lesson1/) that lets a user ask natural language questions about Quarto tips I've collected over several years through attending presentations and conferences, reading blog posts, watching videos, and looking up questions online.

It was technically written by me, but largely with the help of generative AI: mostly [Gemini Pro 2.5](https://gemini.google.com/app) and [Posit's Shiny Assistant](https://gallery.shinyapps.io/assistant/#), but also other LLMs including the Custom GTP [R and RStudio Tutor](https://chatgpt.com/g/g-iedyANQtM-r-and-r-studio-tutor) as well as [Claude](https://claude.ai/). The app itself uses OpenAI's 

I kept the tips in a spreadsheet so they were easier to search -- back in the days before LLMs let you ask and answer questions so easily from unstructured data! The app still has a tab that lets you see them all in the searchable table.

The app code is, unsurprisingly, in the app.R file. Quarto tips are in QuartoTips.xlsx. Tips as an R data frame are in tips_df.Rds. The tips as an HTML file are in QuartoTips.html, because that seems to be a good way to have ragnar process them. That processing is done in the process_quart_tips.R script.

To run this locally, you'd need to have an .Renviron file that sets values for an OPENAI_API_KEY and QUARTO_TIPS -- the latter is the variable I use to hold the complete-path location of the duckdb database used by ragnar to search and retrieve relevant text for a user's query. If you are running this remotely on a Shiny server, that .Renviron file should be in the same directory as your app.R file.

The file _setup_package_installation.R_ should set up your system to install any needed packages you don't already have, but I haven't tested it extensively to make sure. If something else turns out to be missing, just install it manually!

