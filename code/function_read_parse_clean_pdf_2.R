# read_pdf.R
# Reads, parses, and cleans a PDF file into a tidy tibble.
#
# Each row = one word, with page number, x/y position, and cleaned text.
# NOTE: Using XML instead of directly to text.
#
# Usage:
#   source("read_pdf.R")
#   df <- read_pdf("data/2024_amazon_report.pdf")
#   df <- read_pdf("data/2024_amazon_report.pdf", pages = c(1, 3, 5))

library(pdftools)
library(tibble)
library(dplyr)

read_pdf <- function(path, pages = NULL) {
  
  # Validate file exists
  if (!file.exists(path)) {
    stop("File not found: ", path)
  }
  
  # Read all pages into a list of data frames (one per page)
  raw <- pdf_data(path)
  
  # Subset to requested pages if specified
  if (!is.null(pages)) {
    if (any(pages > length(raw))) {
      stop("Requested page(s) exceed total page count (", length(raw), ")")
    }
    raw <- raw[pages]
    page_numbers <- pages
  } else {
    page_numbers <- seq_along(raw)
  }
  
  # Combine all pages into a single tibble
  result <- mapply(function(page_df, page_num) {
    tibble(
      page    = page_num,
      x       = page_df$x,
      y       = page_df$y,
      width   = page_df$width,
      height  = page_df$height,
      text    = page_df$text
    )
  }, raw, page_numbers, SIMPLIFY = FALSE)
  
  result <- bind_rows(result)
  
  # Clean text column
  result <- result |>
    mutate(
      text = trimws(text),                        # strip leading/trailing whitespace
      text = gsub("\\s+", " ", text),             # collapse internal whitespace
      text = gsub("[^\x20-\x7E]", "", text)       # remove non-ASCII characters
    ) |>
    filter(nchar(text) > 0)                       # drop empty tokens
  
  result
}


# --- Helper: collapse a page (or the whole doc) into a plain string ----------

pdf_to_string <- function(path, pages = NULL, sep = " ") {
  df <- read_pdf(path, pages = pages)
  paste(df$text, collapse = sep)
}


# --- Helper: collapse to one string per page ---------------------------------

pdf_to_strings <- function(path, pages = NULL, sep = " ") {
  df <- read_pdf(path, pages = pages)
  df |>
    group_by(page) |>
    summarise(text = paste(text, collapse = sep), .groups = "drop")
}