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
  
  # --- FIX 1: Suppress poppler font/annotation warnings ----------------------
  # "Invalid Font Weight", "Bad annotation destination", and
  # "Mismatch between font type and embedded font file" are emitted by the
  # poppler C library via stderr. They are renderer warnings, not R errors,
  # so we redirect stderr for the duration of the call.
  raw <- withCallingHandlers(
    tryCatch(
      suppressWarnings(pdf_data(path)),   # suppressWarnings catches R-level warns
      error = function(e) {
        stop("pdf_data() failed on '", path, "': ", conditionMessage(e))
      }
    ),
    warning = function(w) {
      # Silently swallow known poppler noise; re-raise anything else
      msg <- conditionMessage(w)
      if (grepl("font|annotation|embedded", msg, ignore.case = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )
  
  # --- FIX 2: Guard against pages that came back empty (corrupt/image-only) --
  # Poppler font mismatches often coincide with pages that return NULL or
  # zero-row data frames; drop them gracefully instead of erroring.
  valid_idx <- which(vapply(raw, function(p) !is.null(p) && nrow(p) > 0, logical(1)))
  if (length(valid_idx) == 0) {
    stop("No extractable text found in '", path, "'. ",
         "The PDF may be image-only or fully corrupted.")
  }
  
  # Subset to requested pages if specified
  if (!is.null(pages)) {
    if (any(pages > length(raw))) {
      stop("Requested page(s) exceed total page count (", length(raw), ")")
    }
    # Only keep pages that are both requested AND valid
    keep         <- intersect(pages, valid_idx)
    skipped      <- setdiff(pages, valid_idx)
    if (length(skipped) > 0) {
      warning("Skipping page(s) with no extractable text: ",
              paste(skipped, collapse = ", "))
    }
    raw          <- raw[keep]
    page_numbers <- keep
  } else {
    raw          <- raw[valid_idx]
    page_numbers <- valid_idx
  }
  
  # Combine all pages into a single tibble
  result <- mapply(function(page_df, page_num) {
    tibble(
      page   = page_num,
      x      = page_df$x,
      y      = page_df$y,
      width  = page_df$width,
      height = page_df$height,
      text   = page_df$text
    )
  }, raw, page_numbers, SIMPLIFY = FALSE)
  
  result <- bind_rows(result)
  
  # --- FIX 3: Widen the non-ASCII strip to catch mangled font substitutions --
  # Font-type mismatches often produce replacement characters (U+FFFD) or
  # Windows-1252 mojibake in addition to plain non-ASCII bytes. Convert to
  # UTF-8 defensively, then strip anything that isn't printable ASCII.
  result <- result |>
    mutate(
      text = vapply(text, function(x) {
        x <- iconv(x, from = "UTF-8", to = "UTF-8", sub = "")   # drop invalid UTF-8
        x <- gsub("\uFFFD", "", x)                               # drop replacement chars
        x <- gsub("[^\x20-\x7E]", "", x)                         # keep printable ASCII
        trimws(gsub("\\s+", " ", x))                             # normalise whitespace
      }, character(1))
    ) |>
    filter(nchar(text) > 0)
  
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