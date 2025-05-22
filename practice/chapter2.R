# Chapter 2 : First Foray into Text Analysis with R
# Goals : Learn to load, tokenize, and search a text.
# Concepts : Word Frequencies and Lexical Makeup

# Section 2.1 : Loading the First Text File

# Get current working directory (helpful for file path issues)
getwd()

# Load the local version of Moby Dick from the "data/text" folder
# Each line in the text becomes one element in the character vector
text_v <- scan("data/text/melville.txt", what = "character", sep = "\n")

# (Optional) Load Moby Dick directly from Project Gutenberg
# Again, we're reading it line by line using the newline separator
gutentext_v <- scan(
  "http://www.gutenberg.org/cache/epub/2701/pg2701.txt",
  what = "character",
  sep = "\n"
)

# By using sep = "\n", we're telling R to treat each line of the text as one element
# This helps us isolate sections like metadata vs. main content

# Section 2.3 : Separate Content from Metadata

# Find the index where the actual story begins (i.e. the first chapter)
# We're assuming the story starts exactly with "CHAPTER 1. Loomings."
start_v <- which(text_v == "CHAPTER 1. Loomings.")
start_v  # Should return 3 or similar depending on your version

# Check how many total lines exist in the full text (metadata + story)
length(text_v)

# Separate metadata (lines before the story starts)
# Subtract 1 from start_v to exclude the chapter line from metadata
metadata_v <- text_v[1:(start_v - 1)]

# Extract the main story content from start_v to the end of the file
novel_lines_v <- text_v[start_v:length(text_v)]

# Check how many lines were removed as metadata
length(text_v) - length(novel_lines_v)

# Collapse the vector of lines into a single character string
# paste() glues them together, using a space (" ") to join the lines
# This is useful when we want the entire novel as one long string
novel_v <- paste(novel_lines_v, collapse = " ")

# This should return 1, meaning we now have one long text string
length(novel_v)

# View the first portion of the novel (expect: "CHAPTER 1. Loomings. Call me Ishmael...")
novel_v[1]

# Section 2.4 : Reprocessing the Content
# Now that the novel is stored as one long string (novel_v), we can start reprocessing it

# Step 1: Convert all characters to lowercase using tolower()
# This ensures that "Whale" and "whale" are treated the same in later analysis
novel_lower_v <- tolower(novel_v)

# Step 2: Split the lowercase text into individual words
# strsplit() is used to break up a string based on a pattern
# "\\W" is a regular expression that matches any non-word character (e.g., space, punctuation)
# This helps us isolate actual words and ignore punctuation or special characters
moby_word_l <- strsplit(novel_lower_v, "\\W")

# At this point, R returns a **list** rather than a vector
# Why a list? Because strsplit is designed to handle multiple strings at once
# Even though we're splitting just one string (the full novel), the result is still wrapped in a list
# Lists are flexible containers in R—each element can hold different types of data
# Think of it like a file drawer that can contain various types of files (numbers, text, even other lists)

# Step 3: Understand what type of object you're working with
class(novel_lower_v)  # Should return "character"
class(moby_word_l)    # Should return "list"

# Step 4: Get a deeper look into the structure of the list using str()
# str() reveals how many elements are inside, and previews the first few items
# This helps confirm that the list contains a vector of words (e.g., "chapter", "1", "loomings", etc.)
str(moby_word_l)

# Tip: You can learn more about strsplit by typing ?strsplit in your console
# This will open the help page and show usage examples, arguments, and return types
?strsplit

# Using the unlist function:
# The strsplit() function earlier returned a list (moby_word_l) with one element:
# a vector of all the words from the novel. We now "flatten" this list into a plain vector.
moby_word_v <- unlist(moby_word_l)

# Confirm the structure before and after unlisting
str(moby_word_l)   # Should show a list with 1 element: a character vector of ~253,992 items

# Step: Identify non-blank entries
# Some elements are empty strings (""), usually caused by splitting on punctuation or multiple spaces
# We only want actual words, so we find the indices where the string is not blank
not_blanks_v <- which(moby_word_v != "")
not_blanks_v  # Returns the positions of all non-empty strings

# Clean the word vector: keep only the valid (non-blank) words
# We overwrite moby_word_v with a cleaner version, removing all empty strings
moby_word_v <- moby_word_v[not_blanks_v]

# Preview the cleaned-up vector
moby_word_v         # Shows the entire vector (huge)
moby_word_v[1:10]   # Shows the first 10 words
moby_word_v[99986]  # Spot-check a single word deep into the vector
moby_word_v[4:6]    # View a short range

# You can also use a variable to store your index positions
mypositions_v <- c(4, 5, 6)
moby_word_v[mypositions_v]  # Same as above
moby_word_v[c(4, 5, 6)]      # Direct inline indexing (same result)

# Search for a specific word — in this case: "whale"
# which() returns all the positions where "whale" appears in the text
which(moby_word_v == "whale")


# Section 2.5: Beginning Some Analysis

# We now have all the words from Moby Dick in a clean character vector (moby_word_v),
# in the same order as they appear in the novel.
# This structure is helpful for both **chronological reading** and **text analysis**.

# Step 1: Count how many times the word "whale" appears in the novel
# which() gives positions where the word equals "whale"
# length() counts how many such positions exist
length(moby_word_v[which(moby_word_v == "whale")])

# Step 2: Count the total number of words in the novel
length(moby_word_v)

# Step 3: Save both values into variables for reuse
whale_hits_v <- length(moby_word_v[which(moby_word_v == "whale")])  # total number of "whale"
total_words_v <- length(moby_word_v)                                # total number of words

# Step 4: Calculate the proportion of "whale" occurrences
# This gives us an idea of how "central" this word is in the novel
whale_hits_v / total_words_v

# Step 5: Count the number of **unique words** in the novel
# unique() finds all the distinct word types; length() counts how many there are
length(unique(moby_word_v))

# Step 6: Analyze word frequencies using table()
# table() creates a frequency count for every unique word in the vector
moby_freqs_t <- table(moby_word_v)

# Preview the first few words and their frequencies (not sorted yet)
moby_freqs_t[1:10]

# Step 7: Sort the frequency table in decreasing order
# This helps us identify the most common words — a key step to check Zipf’s Law
sorted_moby_freqs_t <- sort(moby_freqs_t, decreasing = TRUE)

# View the top most frequent words in the text
head(sorted_moby_freqs_t)

# ———————————————————————————————
# Quick Note: What is Zipf’s Law?

# Zipf’s Law is a principle in linguistics that suggests:
# The most frequent word in a language/text appears about twice as often as the 2nd most frequent,
# three times as often as the 3rd most frequent, and so on.
# That means when you sort word frequencies, they should follow a **long-tailed, predictable pattern**.

# In Moby Dick, you’ll likely see function words like “the,” “and,” “of” dominate the top of the list,
# which is normal and consistent with Zipf’s Law.

