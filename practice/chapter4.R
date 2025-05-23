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

