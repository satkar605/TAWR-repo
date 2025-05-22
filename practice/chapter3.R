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

# Section 3.5 Practice

# Practice Problem 1 :
# load the Austen text line-by-line
austen_text_v <- scan("data/text/austen.txt", what = "character", sep = "\n")
head(austen_text_v)
# find the start of the actual novel text
austen_start_v <- which(austen_text_v == "CHAPTER 1")
# Keep only the novel's main content
austen_lines_v <- austen_text_v[austen_start_v:length(austen_text_v)]

# clean and tokenize the text : same as we did for Moby Dick
# collapse lines to a single string
austen_v <- paste(austen_lines_v, collapse = " ")
#convert to lowercase
austen_lower_v <- tolower(austen_v)
# split into words using non-word characters
austen_words_l <- strsplit(austen_lower_v, "\\W")
# flatten list into vector
austen_word_v <- unlist(austen_words_l)
# remove blanks
not_blanks_austen_v <- which(austen_word_v != "")
austen_word_v <- austen_word_v[not_blanks_austen_v]

# create frequency table
austen_freqs_t <- table(austen_word_v)
# sort in descending order
sorted_austen_freqs_t <- sort(austen_freqs_t, decreasing = TRUE)

# calculate relative frequencies
austen_length_v <- sum(sorted_austen_freqs_t)
sorted_sense_rel_freqs_t <- 100 * (sorted_austen_freqs_t / austen_length_v)

# plot top 10 words in sense and sensibility
plot(
  sorted_sense_rel_freqs_t[1:10],
  type = "b",
  xlab = "Top Ten Words in Sense and Sensibility",
  ylab = "Percentage (%) of Full Text",
  xaxt = "n"
)
# add custom x-axis labels (actual word names)
axis(
    side = 1,
    at = 1:10,
    labels = names(sorted_sense_rel_freqs_t[1:10])
)

##################################################
# Side-by-Side Plot: Top 10 Words in Both Novels
##################################################

# Set up a 1-row, 2-column plot area
par(mfrow = c(1, 2))  # 1 row, 2 columns

# --- Plot for Moby Dick ---
plot(
  sorted_moby_rel_freqs_t[1:10],
  type = "b",
  xlab = "Top 10 Words in Moby Dick",
  ylab = "Percentage (%) of Full Text",
  xaxt = "n",
  main = "Moby Dick"
)
axis(
  side = 1,
  at = 1:10,
  labels = names(sorted_moby_rel_freqs_t[1:10])
)

# --- Plot for Sense and Sensibility ---
plot(
  sorted_sense_rel_freqs_t[1:10],
  type = "b",
  xlab = "Top 10 Words in Sense & Sensibility",
  ylab = "Percentage (%) of Full Text",
  xaxt = "n",
  main = "Sense and Sensibility"
)
axis(
  side = 1,
  at = 1:10,
  labels = names(sorted_sense_rel_freqs_t[1:10])
)

# Reset plotting area to default (optional)
par(mfrow = c(1, 1))

# Comparitive Analysis : Both novels follow Zipf's law, but Moby Dick uses a more 
# concentrated set of high-frequency words, with "his" suggesting a male-centered
# narrative. Sense and Sensibility shows a more balanced distribution, with "her"
# indicating a female-centric focus. All in all, there are eight common words which
# are commonly repeated  (top 10) in both the novels.

# Section 3.5 Practice Problem 2
# get the names of the top 10 words in each novel
top10_moby_words <- names(sorted_moby_rel_freqs_t[1:10])
top10_sense_words <- names(sorted_sense_rel_freqs_t[1:10])

# combine both sets of word names into a single vector
combined_top_words <- c(top10_moby_words, top10_sense_words)
# use unique() to find distinct word types
unique_top_words <- unique(combined_top_words)
unique_top_words
length(unique_top_words)
# Result : This will return 12 words, since 8 are shared and 2 from each are unique.

# Section 3.5 Practice Problem 3
# The %in% operator checks if elements on the left exist in the list on the right.
# It returns TRUE or FALSE for each element — basically: "is x found in y?"

# Find words from Moby Dick's top 10 are also in Austen's top 10
shared_words <- top10_moby_words[top10_moby_words %in% top10_sense_words]
shared_words

# Section 3.4 Practice Problem 4
# Find words in Austen's top 10 that are NOT in Moby Dick's top 10
unique_sense_words <- top10_sense_words[!top10_sense_words %in% top10_moby_words]
unique_sense_words

# Total unique word types across both top ten lists: 12
# ➤ 8 words are shared between Moby Dick and Sense and Sensibility:
#    "the", "of", "and", "a", "to", "in", "it", "i"
# ➤ 2 words are unique to Moby Dick: "his", "that"
# ➤ 2 words are unique to Sense and Sensibility: "her", "was"

# These differences highlight the narrative focus:
# - "his" in Moby Dick vs. "her" in Austen reflects gendered character emphasis
# - "was" in Austen's list may suggest a more descriptive or reflective storytelling style
