# Written with Gemini 2.5 Pro (much of the functionality) and Posit's Shiny Assistant (help getting the sample question cards to be halfway decent) along with some help from ChatGPT and Claude.


# --- 0. Load necessary libraries ---
# Ensure these are installed: install.packages(c("shiny", "shinychat", "ragnar", "ellmer", "stringr", "glue", "dplyr", "bslib", "reactable", "markdown", "htmltools"))
library(shiny)
library(shinychat)
library(ragnar)
library(ellmer)
library(stringr)    # Needed for string manipulation
library(glue)
library(dplyr)
library(bslib)      # Using for page_navbar
library(reactable)  
library(markdown)   # Added for rendering Markdown content in details
library(htmltools)  # Added for htmlEscape function

# --- Load Data  ---
tips_df_path <- "tips_df.Rds"
if (file.exists(tips_df_path)) {
  tips_df <- readRDS(tips_df_path)
} else {
  # Create a placeholder dataframe if the file doesn't exist to avoid app crashing on startup
  warning(paste("File not found:", tips_df_path, ". Using an empty placeholder dataframe."), immediate. = TRUE)
  tips_df <- data.frame(
    What = character(0),
    Categories = character(0),
    Code = character(0),
    Source = character(0),
    Notes = character(0),
    stringsAsFactors = FALSE
  )
}


# --- 1. Configuration and Setup ----
# --- Configure ellmer first (API Key) ---
# Make sure your OPENAI_API_KEY is set in your environment

## --- Ragnar Setup ----
# Ensure the QUARTO_TIPS environment variable points to your store location
store_location <- Sys.getenv("QUARTO_TIPS") 

# Check if API key is available. Replace with another provider key if you're using another model
OPENAI_API_KEY <- Sys.getenv("OPENAI_API_KEY")
if (nchar(OPENAI_API_KEY) == 0) {
  warning("OPENAI_API_KEY environment variable not set. LLM calls will likely fail.", immediate. = TRUE)
}


# example if using another provider such as Google Gemini
# GOOGLE_API_KEY <- Sys.getenv("GOOGLE_API_KEY")
# if (nchar(GOOGLE_API_KEY) == 0) {
#   warning("GOOGLE_API_KEY environment variable not set. LLM calls will likely fail.", immediate. = TRUE)
# }


# Connect to the Ragnar store
store <- tryCatch({
  # Check if the store file exists before attempting to connect
  if (!file.exists(store_location)) {
    stop(paste("Ragnar store file not found at:", store_location))
  }
  ragnar_store_connect(store_location, read_only = TRUE)
}, error = function(e) {
  warning("Failed to connect to Ragnar store at: ", store_location, "\nError: ", e$message, immediate. = TRUE)
  NULL # Ensure store is NULL on failure
})

## --- Ellmer Setup ----
# Define the chat model with system prompt. Use another ellmer provider and model if you'd like, just make sure to set the API key.



# If you want to use OpenAI:
chat_model <- tryCatch({
  chat_openai(
    model = "gpt-4o-mini",
    system_prompt = "You answer questions about the Quarto technical publishing system based on the provided context from Sharon's Quarto tips. Use the provided context whenver possible! If the context can't answer the query, you can try to answer the question based on your own knowledge, but don't make anything up! If you use your own knowledge and not the context, make sure you say that you'e answered based on your knowledge and not the provided context. If the question doesn't seem relevant to Quarto or R or programming, say the question doesn't appear relevant and politely explain that you won't answer."
  )
}, error = function(e) {
  warning("Failed to initialize OpenAI chat model. Check API key and network. Error: ", e$message, immediate. = TRUE)
  NULL # Set model to NULL on failure
})


# If you want to use Google Gemini, for example
# chat_model <- tryCatch({
#   chat_gemini(
#     model = "gemini-2.0-flash",
#     system_prompt = "You answer questions about the Quarto technical publishing system based on the provided context from Sharon's Quarto tips. Use the provided context whenver possible! If the context can't answer the query, you can try to answer the question based on your own knowledge, but don't make anything up! If you use your own knowledge and not the context, make sure you say that you'e answered based on your knowledge and not the provided context."
#   )
# }, error = function(e) {
#   warning("Failed to initialize OpenAI chat model. Check API key and network. Error: ", e$message, immediate. = TRUE)
#   NULL # Set model to NULL on failure
# })


# --- 2. Helper Functions ----
# get_context: Retrieves context chunks from the Ragnar store
get_context <- function(the_query, the_store = store, the_number = 5) {
  # Check if the store object exists (initial connection check)
  if (is.null(the_store)) {
    warning("Ragnar store object is NULL in get_context.")
    # Return expected structure even on error
    return(list(context = "Error: Database connection is not available.",
                chunks = data.frame(text=character(0), origin=character(0), score=numeric(0), stringsAsFactors = FALSE)))
  }
  
  # Attempt to retrieve chunks using Vector Similarity Search
  embedding_near_chunks <- tryCatch({
    ragnar_retrieve_vss(the_store, the_query, top_k = the_number)
  }, error = function(e) {
    warning("Ragnar VSS retrieval failed: ", e$message)
    # Return an empty dataframe with expected columns on failure
    data.frame(text=character(0), origin=character(0), score=numeric(0), stringsAsFactors = FALSE)
  })
  
  # Handle cases where no chunks are retrieved or result is not a dataframe
  if (!inherits(embedding_near_chunks, "data.frame") || nrow(embedding_near_chunks) == 0) {
    # Return expected structure even when no context found
    return(list(context = "Could not find relevant context.",
                chunks = data.frame(text=character(0), origin=character(0), score=numeric(0), stringsAsFactors = FALSE)))
  }
  
  # Ensure 'text' column exists before pasting
  if (!"text" %in% names(embedding_near_chunks)) {
    warning("Retrieved chunks data frame is missing the 'text' column.")
    return(list(context = "Error: Retrieved data format is incorrect (missing 'text').",
                chunks = embedding_near_chunks)) # Return what was retrieved
  }
  
  # Combine the text of the chunks into a single context string
  context <- paste(embedding_near_chunks$text, collapse = "\n\n---\n\n")
  
  # Return the combined context and the raw chunks data frame
  my_list <- list(context = context, chunks = embedding_near_chunks)
  return(my_list)
}

# get_prompt_and_context: Creates the LLM prompt including retrieved context
get_prompt_and_context <- function(the_query, the_store = store, the_number = 5) {
  # Get context and the raw chunks data frame
  context_results <- get_context(the_query, the_store = the_store, the_number = the_number)
  
  my_context <- context_results$context
  my_chunks <- context_results$chunks # Get the chunks data frame
  
  # Create the prompt for the LLM
  my_prompt <- glue::glue(
    "Based *only* on the following context, please answer the question. \n",
    "Context:\n{my_context}\n\n",
    "---\n\n",
    "Question: {the_query}"
  )
  
  # Return the prompt and the chunks data frame
  my_prompt_results <- list(prompt = my_prompt, chunks = my_chunks)
  return(my_prompt_results)
}

# --- 3. Shiny Application UI ----

# Define sample questions for the cards
sample_q1_text <- "How do you render a Quarto document as a single HTML file?"
sample_q2_text <- "How do you add a div the Quarto way?"

## Use bslib::page_navbar for distinct top-level sections
ui <- bslib::page_navbar(
  title = "Quarto Tips Chatbot Search",
  theme = bslib::bs_theme(version = 5), # Use Bootstrap 5 theme
  
  ## --- Tab 1: Chatbot Interface ----
  bslib::nav_panel(
    title = "Chatbot Search",
    # Use layout_sidebar to place sidebar specifically for this tab
    bslib::layout_sidebar(
      # Define the sidebar where sources will be displayed
      sidebar = bslib::sidebar(
        width = "35%", # Slightly wider sidebar
        h4("Sources Searched for Responses"),
        # UI output placeholder for source details
        uiOutput("sources_output")
      ), # End sidebar definition
      
      # --- Main content area for Chatbot tab ---
      h1("Ask Questions About Sharon's Quarto Tips Collection Below:"),
      p("(These are helpful items she's collected from presentations, blogs, social media, documentation, and other sources. The chatbot will try to answer from its own knowledge if the answer isn't in the available tips.)"),
      
      # Heading for example questions
      h5("Example questions:", style = "text-align: center; margin-bottom: 5px; margin-top: 15px;"),
      
      # Container for clickable sample questions using actionButtons styled as cards
      div(class = "sample-question-container",
          div(class = "sample-question-card",
              actionButton("sample_q1", label = sample_q1_text, class = "sample-question-btn")),
          div(class = "sample-question-card",
              actionButton("sample_q2", label = sample_q2_text, class = "sample-question-btn"))
      ),
      
      # Chat UI element from shinychat package
      chat_ui("chat", placeholder = "Enter your question...")
      # --- End Main content area for Chatbot tab ---
    ) # End layout_sidebar
  ), # End nav_panel "Chatbot Search"
  
  ## --- Tab 2: Browse Tips Table ----
  bslib::nav_panel(
    title = "Browse All Tips",
    h2("Searchable Quarto Tips Table"),
    p("Use the search boxes below each column header or the global search box to filter the tips."),
    # reactable Output for the interactive table
    reactable::reactableOutput("tipsTable")
  ), # End nav_panel "Browse Tips"
  
  ## --- Tab 3: FAQ ----
  bslib::nav_panel(
    title = "FAQ",
    h2("FAQ"),
    p(HTML("<strong>Where do these Quarto tips come from?</strong> These are interesting tidbits Sharon came across during watching presentations; reading blogs, social media, and documentation; or looking up how to do something.")),
    p(HTML("<strong>Who made this app?</strong> Technically <a target='_blank' href='https://www.machlis.com'>Sharon Machlis</a>, but in large part this was coded with generative AI 😅 using Google's Gemini 2.5 pro large language model with help from Posit's Shiny Assistant and occasionally Claude Sonnet 3.7 and various ChatGPT models and custom GPTs.")),
    p(HTML("<strong>What LLM does this chatbot use?</strong> It currently uses OpenAI's gpt-4o-mini, which performed a bit better than Gemini Flash 2.0 and is less expensive than Anthropic's very capable models.")),
    p(HTML("<strong>What software does it use?</strong> This app uses the <a target='_blank' href='https://shiny.posit.co/r/getstarted/shiny-basics/lesson1/'>R Shiny Web framework</a> and a lot of cool R packages including shiny, <a target='_blank' href='https://posit-dev.github.io/shinychat/'>shinychat</a>, <a target='_blank' href='https://tidyverse.github.io/ragnar/'>ragnar</a>, <a target='_blank' href='https://ellmer.tidyverse.org/'>ellmer</a>, and <a target='_blank' href='https://glin.github.io/reactable/'>reactable</a>.")),
    p(HTML("<strong>Can I see the code behind this?</strong> I hope to put it on GitHub soon."))
  ), # End nav_panel "FAQ"
  
  # --- CSS Styling (applied globally) ----
  # Include CSS for styling the sample question cards and sidebar details
  tags$head(
    tags$style(HTML("
      /* Container for sample questions */
      .sample-question-container {
        display: flex;
        justify-content: center; /* Center cards horizontally */
        gap: 15px; /* Space between cards */
        margin-top: 5px;  /* Space above container (below heading) */
        margin-bottom: 10px; /* Reduced space below cards, closer to input */
        flex-wrap: wrap; /* Allow cards to wrap on smaller screens */
      }
      /* Styles for the clickable cards */
      .sample-question-card {
        background-color: #f8f9fa; /* Light grey background */
        border: 1px solid #dee2e6; /* Light border */
        border-radius: 0.375rem; /* Rounded corners */
        padding: 0.8rem 1rem; /* Padding inside card */
        text-align: center;
        color: #212529; /* Dark text */
        font-weight: normal; /* Regular font weight */
        min-height: 60px; /* Minimum height */
        width: auto; /* Adjust width based on content */
        max-width: 220px; /* Max width */
        display: inline-flex; /* Use flex to align text */
        align-items: center; /* Vertically center text */
        justify-content: center; /* Horizontally center text */
        white-space: normal; /* Allow text wrapping */
        line-height: 1.3; /* Adjust line height for wrapped text */
        cursor: pointer; /* Show pointer cursor on hover */
        transition: background-color 0.2s ease, box-shadow 0.2s ease; /* Smooth transitions */
      }
      .sample-question-card:hover {
        background-color: #e9ecef; /* Slightly darker on hover */
        box-shadow: 0 2px 4px rgba(0,0,0,0.1); /* Add shadow on hover */
      }

      /* Remove default button styling while keeping them clickable */
      .sample-question-btn {
        all: unset; /* Reset all button properties */
        width: 100%;
        height: 100%;
        display: flex;
        align-items: center;
        justify-content: center;
      }
      /* Style for the details/summary elements in the sidebar */
      #sources_output details summary {
        cursor: pointer;
        list-style-position: inside; /* Keeps triangle aligned */
        padding: 2px 0; /* Add slight padding */
        user-select: none;
        -webkit-user-select: none; /* Safari */
        -moz-user-select: none; /* Firefox */
        -ms-user-select: none; /* IE10+/Edge */
      }
      /* Styling for the blockquote containing the source context */
       #sources_output details blockquote {
        font-size: 0.9em;
        border-left: 3px solid #ccc;
        padding-left: 12px;
        margin-left: 5px;
        margin-top: 8px;
        color: #555;
        /* white-space: pre-wrap; */ /* Let contained elements like <pre> handle spacing */
       }
       /* Ensure pre/code within blockquote look okay (from markdown rendering) */
       #sources_output details blockquote pre {
         background-color: #f8f9fa; /* Light background for code blocks */
         border: 1px solid #dee2e6; /* Subtle border */
         padding: 10px;
         border-radius: 4px;
         white-space: pre-wrap; /* Wrap long lines in code */
         word-wrap: break-word; /* Break words if necessary */
         font-size: 0.95em; /* Slightly smaller font for code */
       }
       #sources_output details blockquote code:not(pre code) { /* Inline code */
         font-family: monospace, Consolas, 'Courier New', Courier;
         font-size: 0.95em;
         background-color: #e9ecef; /* Light background for inline code */
         padding: 0.2em 0.4em;
         border-radius: 3px;
       }
       /* Style source paragraph if needed */
        #sources_output details blockquote p em { /* Target the italicized source line */
          font-size: 0.9em;
          margin-top: 10px;
          color: #6c757d; /* Grey text */
          font-style: italic; /* Ensure it remains italic */
        }
        /* Style for the link within the source paragraph */
        #sources_output details blockquote p em a {
           color: #0d6efd; /* Standard link blue */
           text-decoration: underline;
        }
         #sources_output details blockquote p em a:hover {
           color: #0a58ca; /* Darker blue on hover */
         }


      /* --- reactable specific styles (optional adjustments) --- */
      /* Adjust filter input padding/margins if needed */
      .rt-th input[type='text'] {
         /* Example: margin-top: 2px; */
      }
      /* Ensure code column style overrides don't affect filter input */
       .rt-td pre, .rt-td code {
         margin: 0; /* Reset margins */
         padding: 0; /* Reset padding */
         background: none; /* Reset background */
         border: none; /* Reset border */
         font-size: inherit; /* Inherit font size from cell */
       }
      /* Style links in reactable table cells */
      .rt-td a {
        color: #0d6efd;
        text-decoration: underline;
      }
      .rt-td a:hover {
        color: #0a58ca;
      }
    ")) # End HTML()
  ) # End tags$head
) # End page_navbar

# --- 4. Shiny Application Server Logic ----
server <- function(input, output, session) {
  
  onStop(function() {
    if (!is.null(store) && !is.null(store@.con)) {
      DBI::dbDisconnect(store@.con)
    }
  })
  
  
  ## --- Chatbot Logic ----
  # Reactive values to store the last retrieved chunks data frame
  last_chunks <- reactiveVal(NULL) # Initialize as NULL
  
  # Function to process a query (used for both sample questions and user input)
  process_query <- function(query_text) {
    # 1. Get prompt and context (including the raw chunks)
    #    Check if store is available before proceeding
    if (is.null(store)) {
      chat_append("chat", "Assistant Error: Cannot connect to the tip database.")
      return() # Stop processing if store isn't available
    }
    prompt_results <- get_prompt_and_context(query_text, the_store = store)
    # Store the retrieved chunks data frame
    last_chunks(prompt_results$chunks)
    llm_prompt <- prompt_results$prompt
    
    # 2. Get streaming response from the LLM
    # Check if chat_model was initialized successfully
    if (is.null(chat_model)) {
      stream_result <- "Assistant Error: Chat model not available."
    } else {
      stream_result <- tryCatch({
        chat_model$stream(llm_prompt)
      }, error = function(e) {
        warning("Failed to create LLM stream: ", e$message)
        paste("Assistant Error: Could not get stream. Details:", e$message)
      })
    }
    
    
    # 3. Append assistant response to chat history
    chat_append("chat", stream_result)
  }
  
  ## Observers for sample question button clicks ----
  observeEvent(input$sample_q1, {
    process_query(sample_q1_text)
  })
  
  observeEvent(input$sample_q2, {
    process_query(sample_q2_text)
  })
  
  # Observer for user submitting text via the chat input
  observeEvent(input$chat_user_input, {
    query <- input$chat_user_input
    # Basic validation: ensure query is not NULL or just whitespace
    if (is.null(query) || nchar(trimws(query)) == 0) {
      return() # Do nothing if input is empty
    }
    # Process the user's query
    process_query(query)
    
  }) # End observeEvent for chat_user_input
  
  
  ## --- Render sources (for Chatbot Tab Sidebar) ----
  output$sources_output <- renderUI({
    chunks_df <- last_chunks() # Get the stored chunks data frame
    
    # Check if chunks_df is valid and has rows
    if (!is.null(chunks_df) && inherits(chunks_df, "data.frame") && nrow(chunks_df) > 0) {
      
      # --- Ensure required 'text' column exists ---
      if (!"text" %in% names(chunks_df)) {
        warning("Retrieved chunks data is missing the required 'text' column.")
        return(tags$p(tags$em("Error displaying sources: Retrieved data format is incorrect (missing 'text').")))
      }
      # --- End column check ---
      
      ## Create UI elements for each chunk using lapply ----
      chunk_elements <- lapply(1:nrow(chunks_df), function(i) {
        raw_text <- chunks_df$text[i]
        header_text <- "Context Excerpt" # Default fallback title
        details_md <- "" # Initialize details markdown string
        source_line <- "" # Initialize source line string
        
        # --- Start NEW Parsing Logic for Markdown-like format ---
        
        # Split the raw_text into lines
        lines <- stringr::str_split(raw_text, "\n", simplify = TRUE) |> as.vector()
        # Remove completely empty lines BUT keep lines with only whitespace (like inside code blocks)
        lines <- lines[nchar(stringr::str_trim(lines)) > 0 | grepl("^\\s+$", lines)]
        
        if (length(lines) == 0) {
          warning(paste("Chunk", i, "is empty or contains only whitespace."))
          return(NULL) # Skip this chunk if it's effectively empty
        }
        
        # 1. Find and Extract Header
        # Look for "Tip Header: ### Some Text"
        # Allow for optional "Source File:" line before "Tip Header:"
        header_line_indices <- stringr::str_which(lines, "^(Source File:.*?\n)?Tip Header:\\s*###\\s+")
        
        if (length(header_line_indices) > 0) {
          header_line_index <- header_line_indices[1] # Use the first match
          # Extract text after "### " on the identified line (or the line after Source File:)
          target_line <- lines[header_line_index]
          # If the match included Source File, the actual header is on the next line conceptually
          if (grepl("^Source File:", target_line)) {
            # This regex match might span two lines in the original string if \n was matched
            # Let's extract directly from the matched string part after ###
            header_text <- stringr::str_trim(stringr::str_extract(target_line, "(?<=###\\s).*"))
          } else {
            # Simpler case: Header is on this line
            header_text <- stringr::str_trim(stringr::str_extract(target_line, "(?<=###\\s).*"))
          }
          
          # If extraction failed (e.g., only "###" present), use a fallback
          if (is.na(header_text) || nchar(header_text) == 0) {
            header_text <- "Untitled Tip"
            warning(paste("Found 'Tip Header: ###' but no text after it in chunk:", i))
          }
        } else {
          warning(paste("Could not find 'Tip Header: ###' line in chunk:", i))
          # Fallback: Use the first non-empty line as header if possible
          first_non_empty_line_index <- which(nchar(stringr::str_trim(lines)) > 0)[1]
          if(!is.na(first_non_empty_line_index)) {
            header_text <- stringr::str_trim(lines[first_non_empty_line_index])
            # Avoid using 'Details:' or 'Source:' as a fallback header
            if (grepl("^(Details|Source):", header_text, ignore.case = TRUE)) {
              header_text <- "Context Chunk"
            }
          } else {
            header_text <- "Context Chunk" # Absolute fallback
          }
          # Store the index used for the header to avoid reusing it in details
          header_line_index <- first_non_empty_line_index
        }
        
        
        ## 2. Find and Extract Details and Source ----
        details_start_index <- stringr::str_which(lines, "^Details:")
        source_line_index   <- stringr::str_which(lines, "^Source:\\s*<?https?://") # Match source line, allow optional < >
        
        details_lines <- character(0) # Initialize as empty character vector
        
        # Determine the starting line for details search
        # It should be after the header line OR after the "Details:" line, whichever is later
        search_start_line <- 1
        if (length(header_line_indices) > 0 && !is.na(header_line_index)) { # Check if header_line_index is valid
          # If header was found, start searching after it
          search_start_line <- header_line_index + 1
          # Adjust if the header match itself spanned multiple lines (due to optional Source File)
          # This check might be complex; simpler to just start after the found index
        }
        if (length(details_start_index) > 0 && details_start_index[1] >= search_start_line) {
          # If "Details:" line exists and is after the header, start after "Details:"
          search_start_line <- details_start_index[1] + 1
        }
        
        
        # Determine the end line for details search
        # It should be before the "Source:" line (if found after start) or the end of lines
        search_end_line <- length(lines)
        if (length(source_line_index) > 0 && source_line_index[1] >= search_start_line) {
          search_end_line <- source_line_index[1] - 1
        }
        
        # Extract details lines if the range is valid
        if (search_start_line <= search_end_line && search_start_line <= length(lines)) {
          details_lines <- lines[search_start_line:search_end_line]
        } else if (search_start_line > length(lines) && length(details_start_index) == 0) {
          # Only trigger warning if "Details:" was explicitly missing
          warning(paste("Could not find 'Details:' line or content after header in chunk:", i))
        }
        
        
        # Combine details lines into a single Markdown string
        details_md <- paste(details_lines, collapse = "\n")
        
        # 3. Extract Source Line Text
        if (length(source_line_index) > 0) {
          source_line <- stringr::str_trim(lines[source_line_index[1]])
        } else {
          warning(paste("Could not find 'Source:' line in chunk:", i))
          source_line <- "" # Ensure it's an empty string if not found
        }
        
        # --- End NEW Parsing Logic ---
        
        ## --- Start NEW Source Link Generation ----
        source_html_output <- NULL # Initialize variable for the final source HTML
        if (nchar(source_line) > 0) {
          # Check if the line *already* contains a complete HTML <a> tag for the source
          # This is a basic check, might need refinement for complex cases
          if (grepl("^Source:\\s*<a\\s+href=", source_line, ignore.case = TRUE)) {
            # Assume it's already formatted correctly, pass it directly to HTML()
            source_html_output <- source_line
          } else {
            # Attempt to extract the URL from the source line
            # Pattern: Match "http(s)://" followed by non-whitespace chars, excluding trailing '>' if present
            extracted_url <- stringr::str_extract(source_line, "https?://[^\\s>]+")
            
            if (!is.na(extracted_url) && nchar(extracted_url) > 0) {
              # Create the clickable HTML link
              # Use the extracted URL as the link text for simplicity
              link_text <- extracted_url
              source_html_output <- paste0('Source: <a href="', extracted_url, '" target="_blank" rel="noopener noreferrer">', link_text, '</a>')
            } else {
              # Could not extract a valid URL, display the original line as plain text (fallback)
              warning(paste("Could not extract URL from source line:", source_line, "- displaying as text."))
              source_html_output <- source_line # Fallback to original text
            }
          }
        }
        # --- End NEW Source Link Generation ---
        
        
        ## 4. Create the HTML <details> element ----
        tags$details(
          style = "margin-bottom: 10px; border-bottom: 1px solid #eee; padding-bottom: 5px;",
          # Use the extracted header_text (already cleaned)
          tags$summary(tags$strong(header_text)),
          tags$blockquote(
            # Render the details markdown string using shiny::markdown
            # This handles ```code``` blocks correctly
            if (nchar(stringr::str_trim(details_md)) > 0) {
              shiny::markdown(details_md) # Use Shiny's markdown renderer
            } else {
              tags$p(tags$em("No details content found."))
            },
            # Add the source line - use HTML() to render the link if generated
            if (!is.null(source_html_output) && nchar(source_html_output) > 0) {
              tags$p(tags$em(HTML(source_html_output)), style="font-size: 0.9em; margin-top: 8px;")
            } else {
              NULL # Don't render anything if source_html_output is NULL or empty
            }
          ) # End blockquote
        ) # End details
      }) # End lapply
      
      # Remove any NULL elements that might result from empty chunks
      chunk_elements <- chunk_elements[!sapply(chunk_elements, is.null)]
      
      # Combine all chunk elements into a tagList
      tagList(
        h5("Details on searched text:"),
        tags$p(tags$em("Note: Generative AI can make mistakes. Click the triangle next to a header to expand/collapse the retrieved text excerpt."), style="font-size: 0.85em; color: #666;"),
        # Check if there are any valid elements before rendering
        if (length(chunk_elements) > 0) {
          chunk_elements
        } else {
          tags$p("No valid context excerpts could be displayed.")
        }
      )
      
    } else if (!is.null(chunks_df)){
      # Message if chunks_df exists but is empty
      p("No specific text context was retrieved for the last query.")
    } else {
      # Render nothing if no query has run yet (last_chunks is still NULL)
      NULL
    }
  }) # End renderUI for sources_output
  
  ## --- reactable Table Logic (for Browse Tips Tab) ----
  
  output$tipsTable <- reactable::renderReactable({
    # Ensure tips_df is available and has rows
    req(tips_df, nrow(tips_df) > 0)
    
    reactable::reactable(
      tips_df,
      width = "90%",
      style = list(margin = "0 auto"),
      searchable = TRUE,        # Enable global search
      filterable = TRUE,        # Enable per-column filtering
      striped = TRUE,           # Enable striped rows
      highlight = TRUE,         # Enable row highlighting on hover
      bordered = TRUE,          # Add borders
      compact = TRUE,           # Use compact layout
      defaultPageSize = 15,     # Set default rows per page
      columns = list(
        What = colDef(width = 250),
        Categories = colDef(width = 150),
        # --- MODIFIED Source Column ---
        Source = colDef(
          width = 150,
          html = TRUE, # Tell reactable to render HTML content in this column
          cell = function(value) {
            # Check if value is NA or empty string
            if (is.na(value) || nchar(trimws(value)) == 0) {
              return("") # Return empty string for empty cells
            }
            # Check if the value already contains an HTML anchor tag
            if (grepl("<a\\s+href=", value, ignore.case = TRUE)) {
              return(value) # Assume it's already formatted, return as is
            } else {
              # Attempt to extract the first URL from the value
              # Pattern: Match "http(s)://" followed by non-whitespace characters
              extracted_url <- stringr::str_extract(value, "https?://[^\\s]+")
              
              if (!is.na(extracted_url) && nchar(extracted_url) > 0) {
                # Create a clickable HTML link using the extracted URL
                # Use the URL itself as the link text
                return(paste0('<a href="', extracted_url, '" target="_blank" rel="noopener noreferrer">', extracted_url, '</a>'))
              } else {
                # If no URL is found, return the original value as plain text
                # Escape HTML special characters to prevent XSS issues
                return(htmltools::htmlEscape(value))
              }
            }
          }
        ),
        # --- END MODIFIED Source Column ---
        Notes = colDef(width = 150),
        Code = colDef(
          style = list(
            whiteSpace = "pre-wrap",
            fontFamily = "monospace, Consolas, 'Courier New', Courier",
            fontSize = "0.9em"
          ),
          width = 300
        )
      ),
      # Define custom theme for white/grey appearance
      theme = reactableTheme(
        color = "#212529",             # Default text color
        backgroundColor = "#ffffff",   # Default background
        borderColor = "#dee2e6",       # Border color
        stripedColor = "#f8f9fa",      # Striped row color
        highlightColor = "#f0f0f0",    # Hover color
        cellPadding = "8px 12px"       # Adjust cell padding
      )
    )
  }) # End renderReactable for tipsTable
  
} # End server function

# --- Run App ---
shinyApp(ui = ui, server = server)
