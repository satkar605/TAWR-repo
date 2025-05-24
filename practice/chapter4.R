# Chapter 4 : Token Distribution and Regular Expressions
# This chapter explains how to use the positions of words in a vector to create distribution
# plots showing where words occur across a narrative. The grep function is introduced to 
# demonstrate how to use regular expressions for more nuanced pattern matching.

# 4.2 Start Up Code
text_v <- scan("data/text/melville.txt", what = "character", sep = "\n")
start_v <- which(text_v == "CHAPTER 1. Loomings.")
novel_lines_v <- text_v[start_v:length(text_v)]
novel_v <- paste(novel_lines_v, collapse = " ")
novel_lower_v <- tolower(novel_v)
moby_words_l <- strsplit(novel_lower_v, "\\W")
moby_word_v <- unlist(moby_words_l)
not_blanks_v <- which(moby_word_v != "")
moby_word_v <- moby_word_v[not_blanks_v]

# Section 4.4: Dispersion Plots

# We previously explored the raw and relative frequencies of words (global statistics),
# which reveal the overall usage patterns (e.g., central tendency) across the entire text.

# But now, we want to ask a different question:
# 👉 Where exactly do specific words occur in the novel?

# To analyze word positions, we treat the novel as a timeline by indexing each word.
# The seq() function generates a sequence of integers from 1 to the number of words in the novel.
?seq
seq(from = 1, to = 10)  # Example: simple sequence from 1 to 10

# Create a "novel time" vector — each number represents a word position
n_time_v <- seq(from = 1, to = length(moby_word_v))

# Locate all positions where the word "whale" appears in the novel
whales_v <- which(moby_word_v == "whale")

# Initialize a vector (same length as the novel) to record whale positions
# We use NA as a placeholder for "not a whale"
w_count_v <- rep(NA, times = length(n_time_v))

# Mark the positions of "whale" with 1
w_count_v[whales_v] <- 1

# Create a simple dispersion plot: shows where "whale" occurs across the book
plot(
  w_count_v,
  main = "Dispersion Plot of 'whale' in Moby Dick",
  xlab = "Novel Time",
  ylab = "whale",
  type = "h",        # Use vertical lines ("h" for histogram-style)
  ylim = c(0, 1),    # Y-axis range (0 to 1, since we're just marking presence)
  yaxt = 'n'         # Turn off default y-axis labels
)

# -------------------------
# Now repeat the process for "ahab"

# Find positions where the word "ahab" occurs
ahabs_v <- which(moby_word_v == "ahab")

# Initialize a new vector for "ahab" dispersion
a_count_v <- rep(NA, length(n_time_v))

# Mark the positions of "ahab" with 1
a_count_v[ahabs_v] <- 1

# Create a dispersion plot for "ahab"
plot(
  a_count_v,
  main = "Dispersion Plot of 'ahab' in Moby Dick",
  xlab = "Novel Time",
  ylab = "ahab",
  type = "h",
  ylim = c(0, 1),
  yaxt = 'n'
)

# Section 4.5 : Searching with grep

# So far, we found an interesting pattern:
# when the word "ahab" appears, "whale" often doesn't — reflecting the story’s theme
# (Ahab is searching for the whale, but they rarely appear together in text).

# 🟠 Limitations of using `which()`:
# - It looks for **exact matches** only.
# - It does not account for **plurals** (e.g., "whales"), **different forms** (e.g., "whaling"), or **synonyms**.
# - Therefore, it may **miss semantically related words**, which limits our analysis.

# 🛠️ To improve this, we introduce `grep()` and regular expressions (regex).
# `grep()` helps us find **patterns** in text, not just exact matches.
# Regular expressions are powerful ways to define these patterns (like wildcards, suffixes, etc.)

?grep   # Check the help documentation

# We define two regex-based patterns using `grep()` to capture more than exact matches.
# The `|` operator means "OR", so this pattern will match any listed variant.

# Whale-related words (including metaphorical terms like "leviathan")
whale_hits <- grep(
  "whale|whales|whale's|monster|leviathan",
  moby_word_v
)

# Ahab-related references (including possessives and titles)
ahab_hits <- grep(
  "ahab|ahabs|ahab's|captain",
  moby_word_v
)

# Compare with previous exact-match vectors (created using `which()`)
length(whales_v); length(whale_hits)   # whale variants will be more inclusive
length(ahabs_v); length(ahab_hits)     # same for ahab variants

# -----------------------------------------------------
# Dummy example: testing regex on a sample sentence
# -----------------------------------------------------

eg_v <- "this is a _test_ to see if we can keep ahab's and other words such as 
contractions like can't and ain't. it will also allow us to see some other oddities."

# Naive split: drops apostrophes and underscores
strsplit(eg_v, "\\W")

# Improved split: keeps apostrophes in contractions, removes punctuation
strsplit(eg_v, "[^A-Za-z0-9']")

# -----------------------------------------------------
# Reload and re-tokenize the full novel using better regex
# -----------------------------------------------------

text_v <- scan("data/text/melville.txt", what = "character", sep = "\n")
start_v <- which(text_v == "CHAPTER 1. Loomings.")
novel_lines_v <- text_v[start_v:length(text_v)]
novel_v <- paste(novel_lines_v, collapse = " ")
novel_lower_v <- tolower(novel_v)

# This regex retains alphabetic characters and apostrophes (for contractions)
moby_words_l <- strsplit(novel_lower_v, "[^A-Za-z0-9']")  
moby_word_v <- unlist(moby_words_l)
not_blanks_v <- which(moby_word_v != "")
moby_word_v <- moby_word_v[not_blanks_v]

# -----------------------------------------------------
# Repeat regex match with cleaned tokens
# -----------------------------------------------------

whale_hits_new <- grep(
  "whale|whales|whale's|monster|leviathan",
  moby_word_v
)
ahab_hits_new <- grep(
  "ahab|ahabs|ahab's|captain",
  moby_word_v
)

# Subset the matched words to inspect specific variants
whale_varients_v <- moby_word_v[whale_hits_new]
ahab_varients_v <- moby_word_v[ahab_hits_new]

# Frequency tables: how often each variant appears
sort(table(whale_varients_v), decreasing = TRUE)
sort(table(ahab_varients_v), decreasing = TRUE)

# Example insights: rare forms like "narwhale" and "whaleboats" now surface

# -----------------------------------------------------
# Comparing match strategies: exact vs regex vs anchored regex
# -----------------------------------------------------

length(which(whale_varients_v == "whale"))         # exact match
length(grep("whale", whale_varients_v))            # matches "whale", "whaleboats", etc.
length(grep("^whale$", whale_varients_v))          # regex match for the exact word "whale"

# Final thoughts:
# - `which()` is strict — only exact matches
# - `grep()` is flexible — uses patterns to find a broader set
# - The **tokenization** step (how you split the text) directly affects what matches

# This chapter ends with an encouragement to continue using dummy data and grep patterns
# to uncover subtle variations and language quirks in real-world texts

# Section 4.6 Practice

# 4.6.1 Dispersion Plot of Whale Variants

# Create a vector for plotting whale variant positions (regex-based matches)
w_variant_count_v <- rep(NA, length(moby_word_v))
w_variant_count_v[whale_hits_new] <- 1

# Plot dispersion for whale variants
plot(
  w_variant_count_v,
  main = "Dispersion Plot of Whale Variants",
  xlab = "Novel Time",
  ylab = "whale variants",
  type = "h",
  ylim = c(0, 1),
  yaxt = "n"
)

# Stacked dispersion plots for comparision
# Create a new graphic window with two rows, one column
par(mfrow = c(2, 1))  # Stack plots vertically

# -----------------------------------------------------
# Dispersion Plot 1: Exact Match for "whale"
# -----------------------------------------------------

# Create a vector for whale positions
w_count_v <- rep(NA, times = length(moby_word_v))
w_count_v[whales_v] <- 1

plot(
  w_count_v,
  main = "Dispersion Plot of Exact Match: 'whale'",
  xlab = "Novel Time",
  ylab = "whale",
  type = "h",
  ylim = c(0, 1),
  yaxt = "n"
)

# -----------------------------------------------------
# Dispersion Plot 2: Whale Variants via Regex
# -----------------------------------------------------

# Create a vector for regex-based whale variant positions
w_variant_count_v <- rep(NA, length(moby_word_v))
w_variant_count_v[whale_hits_new] <- 1

plot(
  w_variant_count_v,
  main = "Dispersion Plot of Whale Variants (Regex Match)",
  xlab = "Novel Time",
  ylab = "whale variants",
  type = "h",
  ylim = c(0, 1),
  yaxt = "n"
)

# Reset plotting layout back to default (1 plot)
par(mfrow = c(1, 1))

# Key Observations:

# The regex plot is denser, with fewer gaps
# The pattern is the same, but the regex plot gives us a more complete picture of
# whale-related presence + provides more detail as it capture not just literal mentions
# but plural forms "whales"; possessive forms "whale's" and even metaphorical "leviathian"

