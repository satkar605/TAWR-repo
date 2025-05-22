# CHAPTER 3: Accessing and Comparing Word Frequency Data

##################################################
# START-UP CODE BLOCK
# This section prepares moby_word_v for analysis
##################################################

# Load the text file line-by-line
text_v <- scan("data/text/melville.txt", what = "character", sep = "\n")

# Identify where the main text starts (after front matter)
start_v <- which(text_v == "CHAPTER 1. Loomings.")

# Subset from the first chapter to the end
novel_lines_v <- text_v[start_v:length(text_v)]

# Combine the lines into a single string
novel_v <- paste(novel_lines_v, collapse = " ")

# Convert the entire text to lowercase
novel_lower_v <- tolower(novel_v)

# Split the text into individual words using non-word character boundaries
moby_words_l <- strsplit(novel_lower_v, "\\W")

# Flatten the list into a character vector
moby_word_v <- unlist(moby_words_l)

# Remove blank entries
not_blanks_v <- which(moby_word_v != "")
moby_word_v <- moby_word_v[not_blanks_v]

# Create a frequency table of all words
moby_freqs_t <- table(moby_word_v)

# Sort the table from most to least frequent
sorted_moby_freqs_t <- sort(moby_freqs_t, decreasing = TRUE)

##################################################
# END OF START-UP CODE BLOCK
# moby_word_v is now ready for analysis
##################################################

##################################################
# Section 3.3: Accessing Word Data
# Comparing frequencies of gendered pronouns
# Goal: Highlight character representation (he vs. she, him vs. her)
##################################################

# Retrieve frequency counts directly from the named table
sorted_moby_freqs_t["he"]    # Frequency of "he"
sorted_moby_freqs_t["she"]   # Frequency of "she"
sorted_moby_freqs_t["him"]   # Frequency of "him"
sorted_moby_freqs_t["her"]   # Frequency of "her"

# Conclusion: More references to male characters
# Example:
# - "he" occurs ~16.5x more than "she"
# - "him" occurs ~3.2x more than "her"
sorted_moby_freqs_t["him"] / sorted_moby_freqs_t["her"]
sorted_moby_freqs_t["he"] / sorted_moby_freqs_t["she"]

##################################################
# Named Indexing vs. Positional Indexing
##################################################

# Positional indexing on a vector (accessing by number)
moby_word_v[4:6]  # returns the 4th to 6th word in the vector

# Named indexing on a table (accessing by word name)
sorted_moby_freqs_t["the"]   # returns frequency count of "the"

# This will return NA — why?
moby_word_v["the"]

#  Explanation:
# moby_word_v is a **regular character vector**, not a named vector or table
# So using moby_word_v["the"] tries to find a value at the **position named "the"** (which doesn’t exist)
# Use named indexing only on named objects like `table()` output

##################################################
# Relative Frequencies vs. Raw Counts
##################################################

# Always consider the length of the full text when interpreting raw counts
# This allows you to normalize across texts of different lengths

length(moby_word_v)          # Total number of words in the text
sum(sorted_moby_freqs_t)     # Total word count (should match above)
##################################################
# Section 3.4: Recycling and Relative Frequencies
# Purpose:
# - Convert raw word counts into relative frequencies (percentages)
# - Demonstrate R’s vector recycling behavior
# - Plot the top 10 words by relative frequency
##################################################

# Calculate the total number of words (all word occurrences)
moby_length_v <- sum(sorted_moby_freqs_t)

# Convert raw word counts into relative frequencies (% of full text)
# R *recycles* the scalar total word count across the entire vector
sorted_moby_rel_freqs_t <- 100 * (sorted_moby_freqs_t / moby_length_v)

# Demonstration: vector recycling in R with simple numbers
num_vector_v <- c(1, 2, 3, 4, 5)
num_vector_v * 10
# This shows that R multiplies each element by the scalar 10 (recycling the scalar)

# Check the relative frequency of a specific word (e.g., "the")
sorted_moby_rel_freqs_t["the"]
# ➤ Interpretation: "the" occurs approximately 6.6 times per 100 words in Moby Dick

##################################################
# Plotting the Top 10 Words by Relative Frequency
##################################################

# Create a line plot with points for the top 10 words
plot(
  sorted_moby_rel_freqs_t[1:10],   # relative frequencies of top 10 words
  type = "b",                      # "b" = both points and lines
  xlab = "Top Ten Words in Moby Dick",
  ylab = "Percentage (%) of Full Text",
  xaxt = "n"                       # suppress automatic x-axis labels
)

# Add custom x-axis labels using the actual word names
axis(
  side = 1,                                 # bottom axis
  at = 1:10,                                # position of ticks
  labels = names(sorted_moby_rel_freqs_t[1:10])  # word labels
)
