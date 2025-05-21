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
