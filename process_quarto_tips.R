# Load necessary libraries
library(ragnar)
library(dplyr)
library(stringr)
library(glue)
library(DBI)


# --- Configuration ---

Sys.getenv("QUARTO_TIPS") # Use env var in .Renviron
# OR 
# Sys.setenv("QUARTO_TIPS" = "D:/Temp/QuartoTips.duckdb") # Set env var manually
store_location <- Sys.getenv("QUARTO_TIPS")

# 3. Choose your embedding model
embed_function <- \(x) ragnar::embed_openai(x, model = "text-embedding-3-small")

# Delete any existing store (optional, use with caution)
message("Deleting existing store (if any) at: ", store_location)
unlink(store_location, recursive = TRUE, force = TRUE)

# Create the ragnar vector store
message("Creating new vector store at: ", store_location)
store <- ragnar_store_create(
  location = store_location,
  embed = embed_function
)


# --- Processing the HTML File ---

# Load necessary libraries if not already loaded in your script
library(ragnar)
library(dplyr)
library(glue)


message("Ingesting HTML file, framing by H3 tags.")

# Process the HTML file
html_file <- "QuartoTips.html" 

# --- Check if HTML file exists ---
if (!file.exists(html_file)) {
  stop("HTML file not found at: ", html_file)
}

# 1. Read the HTML, using H3 tags to frame the content.
# 2. Chunk further if needed.
# 3. Add context while preserving essential metadata columns (origin, hash).
chunks <- html_file |>
  ragnar_read(frame_by_tags = c("h3")) |> # Frame by H3 tags
  dplyr::mutate(
    source_file = html_file,
    
  ) |>
  # Chunk further if the content within an H3 section is very long
  ragnar_chunk(boundaries = c("paragraph", "sentence")) |>
  
  # Add context - 
  dplyr::mutate(
    # Create the contextual text in a temporary column for clarity
    contextual_text_temp = dplyr::case_when(
      # Use .data$ prefix for robustness against masking, check if 'h3' exists
      !is.null(.data$h3) & !is.na(.data$h3) & nchar(trimws(.data$h3)) > 0 ~ glue::glue(r"---(
                                                                                       Source File: {source_file}
                                                                                       Tip Header: {.data$h3}
                                                                                       
                                                                                       Details:
                                                                                       {.data$text}
      )---"),
      # Fallback for content not under an H3 (like intro sections)
      TRUE ~ glue::glue(r"---(
                        Source File: {source_file}
                        
                        Content:
                        {.data$text}
      )---")
    )
  ) |>
  # >>> CHANGE: Overwrite 'text' column, keep others <<<
  # This replaces the old restrictive select() statement
  dplyr::mutate(text = contextual_text_temp) |>
  # Remove the temporary intermediate column
  dplyr::select(-contextual_text_temp)

# --- Store Chunks ---
message("Inserting ", nrow(chunks), " chunks into the store.")
ragnar_store_insert(store, chunks) 

# --- Save Chunks to a data frame also (Optional) ---
output_rds_path <- "quarto_tips_chunks_df.Rds"
message("Saving chunk data frame to: ", output_rds_path)
saveRDS(chunks, output_rds_path)

message("HTML processing and storing complete.")


# --- Optional: Verify Insertion (Example Query) ---
results <- ragnar_retrieve_vss(store, "How do I render my quarto file as HTML in just one file", top_k = 3)
print(results)

text_retrieved <- paste(results$text, collapse = "\n\n")
cat(text_retrieved)

# CLOSE CONNECTION TO DATABASE!!
# If a write connection exists, you can't connect to a Duckdb even read-only

if (!is.null(store) && !is.null(store@.con)) {
  DBI::dbDisconnect(store@.con)
  print("Database connection closed")
} 
