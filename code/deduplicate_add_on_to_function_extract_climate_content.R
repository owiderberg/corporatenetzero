# deduplication code

# Deduplicate: if two matches are within 50 words of each other,
# keep the one with the longer / more specific keyword
result <- result |>
  arrange(char_position) |>
  mutate(
    word_gap = c(0L, diff(char_position)),   # char gap as proxy for word gap
    new_group = word_gap > 300L | row_number() == 1L,
    group_id  = cumsum(new_group)
  ) |>
  group_by(group_id) |>
  slice_max(order_by = nchar(keyword_matched), n = 1L, with_ties = FALSE) |>
  ungroup() |>
  mutate(match_id = row_number()) |>
  select(source, match_id, category, keyword_matched,
         char_position, before, after, context, context_words)

message(glue("  {nrow(result)} matches retained after deduplication"))
result
