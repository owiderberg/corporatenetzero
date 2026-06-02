# ============================================================
# read_pdf.R
#
# Read, parse, and clean a PDF file into a tidy tibble,
# with automatic detection and handling of multi-column layouts.
#
# Column detection works by analysing the x-positions of words
# on each page (from pdf_data()). A gap histogram is built across
# the page width; large empty horizontal bands indicate column
# separators. Each column is then read top-to-bottom independently
# and concatenated left-to-right, giving correct reading order.
#
# Requirements:
#   install.packages(c("tidyverse", "pdftools", "glue"))
# ============================================================

library(tidyverse)
library(pdftools)
library(glue)

# ------------------------------------------------------------
# 1. COLUMN DETECTION
# ------------------------------------------------------------

#' Detect column boundaries on a single page using word x-positions
#'
#' Divides the page width into `n_bins` horizontal slices and counts
#' how many words fall in each slice. Slices with zero (or very few)
#' words form "gaps" that indicate column separators.
#'
#' @param page_data   Data frame — one page from pdf_data(), with columns
#'                    x, y, width, height, text
#' @param n_bins      Number of horizontal bins to use (default 50)
#' @param gap_thresh  Fraction of max word density below which a bin is
#'                    considered a gap (default 0.05)
#' @param min_col_width_frac  Minimum column width as fraction of page
#'                    width; narrower regions are ignored (default 0.1)
#'
#' @return A numeric vector of x-positions marking the START of each
#'         column (always includes 0 as the first element)
detect_columns <- function(page_data,
                           n_bins             = 50L,
                           gap_thresh         = 0.05,
                           min_col_width_frac = 0.10) {
  
  words <- page_data |>
    filter(str_length(text) > 0)
  
  if (nrow(words) == 0L) return(0)
  
  page_width <- max(words$x + words$width, na.rm = TRUE)
  if (page_width <= 0) return(0)
  
  bin_width  <- page_width / n_bins
  max_density <- 0
  
  # Count words per horizontal bin
  density <- words |>
    mutate(bin = floor(x / bin_width)) |>
    count(bin) |>
    complete(bin = 0:(n_bins - 1L), fill = list(n = 0L))
  
  max_density <- max(density$n)
  threshold   <- max_density * gap_thresh
  
  # Label each bin as "gap" or "content"
  density <- density |>
    mutate(is_gap = n <= threshold)
  
  # Find transitions from gap -> content: these are column starts
  col_starts_bins <- density |>
    mutate(prev_gap = lag(is_gap, default = TRUE)) |>
    filter(!is_gap & prev_gap) |>
    pull(bin)
  
  col_starts_x <- col_starts_bins * bin_width
  
  # Filter out column starts that are too narrow (likely noise)
  min_col_width <- page_width * min_col_width_frac
  col_starts_x  <- col_starts_x[
    c(diff(c(col_starts_x, page_width))) >= min_col_width
  ]
  
  # Always start from 0
  sort(unique(c(0, col_starts_x)))
}

#' Assign each word to a column based on detected column boundaries
#'
#' @param page_data     Data frame from pdf_data() for one page
#' @param col_starts    Numeric vector of column start x-positions
#' @return page_data with an additional integer column `col_id`
assign_columns <- function(page_data, col_starts) {
  
  page_data |>
    mutate(
      col_id = map_int(x, function(wx) {
        # Column is the last start position <= word x
        idx <- which(col_starts <= wx)
        if (length(idx) == 0L) 1L else max(idx)
      })
    )
}

# ------------------------------------------------------------
# 2. COLUMN-AWARE TEXT EXTRACTION
# ------------------------------------------------------------

#' Extract reading-order text from one page, respecting columns
#'
#' Words are sorted by column first, then by y-position (top to
#' bottom) within each column, then by x-position (left to right)
#' within each line.
#'
#' @param page_data   Data frame — one page from pdf_data()
#' @param n_bins      Passed to detect_columns()
#' @param gap_thresh  Passed to detect_columns()
#' @param line_tol    Vertical tolerance (points) to group words into
#'                    the same line (default 4)
#'
#' @return A named list:
#'   - text       : character string with full page text
#'   - n_columns  : integer — number of columns detected
extract_page_text <- function(page_data,
                              n_bins   = 50L,
                              gap_thresh = 0.05,
                              line_tol   = 4L) {
  
  words <- page_data |>
    filter(str_length(text) > 0)
  
  if (nrow(words) == 0L) {
    return(list(text = "", n_columns = 0L))
  }
  
  col_starts <- detect_columns(words, n_bins, gap_thresh)
  n_cols     <- length(col_starts)
  
  words <- assign_columns(words, col_starts)
  
  # Round y to nearest `line_tol` points to group words into lines
  words <- words |>
    mutate(line_y = round(y / line_tol) * line_tol)
  
  # Sort: column -> line -> x position
  words <- words |>
    arrange(col_id, line_y, x)
  
  # Reconstruct text: join words into lines, lines into paragraphs
  text <- words |>
    group_by(col_id, line_y) |>
    summarise(line_text = str_c(text, collapse = " "), .groups = "drop") |>
    arrange(col_id, line_y) |>
    pull(line_text) |>
    str_c(collapse = "\n")
  
  list(text = text, n_columns = n_cols)
}

# ------------------------------------------------------------
# 3. PAGE-LEVEL HELPERS
# ------------------------------------------------------------

#' Strip header and footer lines from a page string
#'
#' @param page_text    Character string — raw text of one PDF page
#' @param header_lines Integer — lines to drop from the top
#' @param footer_lines Integer — lines to drop from the bottom
#' @return Character string with header/footer lines removed
strip_header_footer <- function(page_text, header_lines, footer_lines) {
  
  lines <- str_split(page_text, "\n")[[1]]
  n     <- length(lines)
  start <- min(header_lines + 1L, n)
  end   <- max(n - footer_lines, 1L)
  
  if (start > end) return("")
  lines[start:end] |> str_c(collapse = "\n")
}

#' Clean a page string
#'
#' Steps:
#'  1. Re-join words split by hyphenated line breaks
#'  2. Replace remaining newlines with spaces
#'  3. Remove non-printable control characters
#'  4. Collapse runs of whitespace to a single space
#'  5. Trim leading / trailing whitespace
#'
#' @param page_text Character string
#' @return Cleaned character string
clean_page <- function(page_text) {
  page_text |>
    str_replace_all("-\n",  "")  |>
    str_replace_all("\n",   " ") |>
    str_replace_all("[^\\x20-\\x7E\\xA0-\\xFF]", "") |>
    str_replace_all("\\s+", " ") |>
    str_trim()
}

# ------------------------------------------------------------
# 4. MAIN FUNCTION
# ------------------------------------------------------------

#' Read, parse, and clean a PDF into a tidy tibble
#'
#' Automatically detects the number of columns on each page and
#' extracts text in correct reading order (left column top-to-bottom,
#' then right column top-to-bottom, etc.).
#'
#' @param path             Path to the PDF file
#' @param header_lines     Lines to strip from the top of each page (default 1)
#' @param footer_lines     Lines to strip from the bottom of each page (default 1)
#' @param min_page_chars   Drop pages with fewer characters after cleaning (default 50)
#' @param n_bins           Bins used for column detection (default 50)
#' @param gap_thresh       Gap sensitivity for column detection (default 0.05);
#'                         increase toward 0.15 for noisier PDFs
#' @param line_tol         Point tolerance for grouping words into lines (default 4)
#'
#' @return A tibble with columns:
#'   - source      : filename (character)
#'   - page        : page number (integer)
#'   - n_columns   : number of columns detected on this page (integer)
#'   - n_chars     : character count of cleaned text (integer)
#'   - text        : cleaned page text in correct reading order (character)
#'
#' @examples
#'   # Basic usage — works for single- and multi-column PDFs
#'   pdf_tbl <- read_pdf("article.pdf")
#'
#'   # Check what column layouts were found
#'   pdf_tbl |> count(n_columns)
#'
#'   # Collapse all pages into one string for an LLM prompt
#'   full_text <- collapse_pdf(pdf_tbl)
#'
#'   # Stricter gap detection for dense newspaper columns
#'   pdf_tbl <- read_pdf("newspaper.pdf", gap_thresh = 0.02)
read_pdf <- function(path,
                     header_lines   = 1L,
                     footer_lines   = 1L,
                     min_page_chars = 50L,
                     n_bins         = 50L,
                     gap_thresh     = 0.02,
                     line_tol       = 4L) {
  
  if (!file.exists(path)) stop(glue("File not found: {path}"))
  
  ext <- tools::file_ext(path) |> tolower()
  if (ext != "pdf") stop(glue("Expected a .pdf file, got: .{ext}"))
  
  message(glue("Reading: {path}"))
  
  # pdf_data() returns a list of data frames — one per page —
  # with word-level x, y, width, height, and text columns.
  pages_data <- pdf_data(path)
  n_pages    <- length(pages_data)
  message(glue("  {n_pages} pages found"))
  
  result <- map(seq_len(n_pages), function(i) {
    
    page_result <- extract_page_text(
      pages_data[[i]],
      n_bins    = n_bins,
      gap_thresh = gap_thresh,
      line_tol  = line_tol
    )
    
    # Strip headers/footers then clean
    cleaned <- page_result$text |>
      strip_header_footer(header_lines, footer_lines) |>
      clean_page()
    
    tibble(
      source    = basename(path),
      page      = i,
      n_columns = page_result$n_columns,
      text      = cleaned,
      n_chars   = nchar(cleaned)
    )
  }) |>
    bind_rows() |>
    filter(n_chars >= min_page_chars) |>
    select(source, page, n_columns, n_chars, text)
  
  col_summary <- result |>
    count(n_columns) |>
    mutate(label = glue("{n_columns}-col: {n} pages")) |>
    pull(label) |>
    str_c(collapse = ", ")
  
  message(glue("  {nrow(result)} pages retained  |  {col_summary}"))
  result
}

# ------------------------------------------------------------
# 5. COLLAPSE HELPER
# ------------------------------------------------------------

#' Collapse a pdf tibble into a single string ready for an LLM prompt
#'
#' @param pdf_tbl   Tibble returned by read_pdf()
#' @param separator String inserted between pages (default: double newline)
#' @return A single character string
collapse_pdf <- function(pdf_tbl, separator = "\n\n") {
  str_c(pdf_tbl$text, collapse = separator)
}

# ------------------------------------------------------------
# 6. EXAMPLE USAGE
# ------------------------------------------------------------

pdf_tbl   <- read_pdf("data/2024_amazon_report.pdf")
full_text <- collapse_pdf(pdf_tbl)
cat(full_text)


# pdf_tbl <- read_pdf("article.pdf")
#
# # Inspect detected layouts
pdf_tbl |> count(n_columns)
#
# # Full text for prompting
# full_text <- collapse_pdf(pdf_tbl)
# cat(full_text)
#
# # Tuning tips:
# #  - Newspaper-style narrow columns -> lower gap_thresh (e.g. 0.02)
# #  - Wide margins causing false columns -> raise min_col_width_frac
# #    inside detect_columns() to e.g. 0.15
# #  - Subscripts grouping onto wrong lines -> raise line_tol (e.g. 6-8)
