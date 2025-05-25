# Chapter 5 : Token Distribution Analysis
# ----------------------------------------

# Section 5.1 : Cleaning the Workspace
# -------------------------------------
# Best Practice: Clear the R environment before starting a new task.
# Alternatively, the broom button in the Console or Environment tab works fine too.
rm(list = ls())
ls() # Confirm it's empty
# Expected output: character(0)

# Section 5.2 : Start-up Code for Chapter 5
# ------------------------------------------
# Load the full text line-by-line
text_v <- scan("data/text/melville.txt", what = "character", sep = "\n") 

# Identify where the actual novel starts
start_v <- which(text_v == "CHAPTER 1. Loomings.")

# Store all lines from the start of the novel to the end
novel_lines_v <- text_v[start_v:length(text_v)]

# Section 5.3 : Identifying Chapter Breaks with grep
# ---------------------------------------------------
# In Moby Dick, every chapter begins with the word "CHAPTER"
# followed by a space and one or more digits (chapter index)

# Goal: Work with a dummy line to check the regex
dummy_line <- "CHAPTER 17. The Ramadan."
# Does the pattern match?
grepl("^CHAPTER \\d+", dummy_line) # grepl() returns TRUE or FALSE; perfect for quick checks

# Locate chapter header lines using regex
chap_positions_v <- grep("^CHAPTER \\d+", novel_lines_v)

# Preview the matched lines
novel_lines_v[chap_positions_v]
head(novel_lines_v[chap_positions_v])

# The challenge: How do we know where the novel ends after the last chapter?
# Use tail() to inspect the final lines
tail(novel_lines_v) # This shows that the last line of the book is "THE END"

# See current start positions of all chapters
chap_positions_v

# Get the index of the last line in the text i.e. "THE END"
last_position_v <- length(novel_lines_v)

# Create updated chapter positions vector with end marker
chap_positions_v <- c(chap_positions_v, last_position_v)

# Section 5.4 : The for Loop and if Conditional
# ---------------------------------------------
# The challenge ahead: Figure out how to process the text (using for loops)

chap_positions_v[1] # Returns the position for CHAPTER 1
chap_positions_v[2] # Returns the position for CHAPTER 2

# Loop over all chapter positions
# This prints the actual line number in novel_lines_v where each chapter begins
for(i in 1:length(chap_positions_v)) {
  print(chap_positions_v[i])
}

# This loop gives more readable output, showing chapter number and position
for(i in 1:length(chap_positions_v)) {
  print(paste("Chapter", i, "begins at position", chap_positions_v[i]))
}

# Full loop with if condition to extract chapter text and compute word frequencies
# Create two empty lists to store chapter-level frequency data
chapter_raws_l <- list()  # Raw word counts 
chapter_freqs_l <- list() # Relative word frequencies (percent)

# Iterate over each chapter, excluding the last dummy "THE END" marker
# Automates chapter-wise text processing into two organized lists
for(i in 1:length(chap_positions_v)) {
  if(i != length(chap_positions_v)) {
    
    # Get the chapter title (e.g., "CHAPTER 1. Loomings.")
    chapter_title <- novel_lines_v[chap_positions_v[i]]
    
    # Define the text range: from line after current chapter start...
    start <- chap_positions_v[i] + 1
    
    # ...to the line before the next chapter start
    end <- chap_positions_v[i + 1] - 1
    
    # Extract lines of text for the chapter
    chapter_lines_v <- novel_lines_v[start:end]
    
    # Collapse all lines into a single string and convert to lowercase
    chapter_words_v <- tolower(paste(chapter_lines_v, collapse = " "))
    
    # Split the string into words using non-word character pattern
    chapter_words_l <- strsplit(chapter_words_v, "\\W")
    
    # Convert the list to a flat vector
    chapter_word_v <- unlist(chapter_words_l)
    
    # Remove empty strings that might occur due to punctuation or spacing
    chapter_word_v <- chapter_word_v[which(chapter_word_v != "")]
    
    # Compute raw word frequency table
    chapter_freqs_t <- table(chapter_word_v)
    
    # Save the raw frequency table in the list
    chapter_raws_l[[chapter_title]] <- chapter_freqs_t
    
    # Convert to relative frequencies (percent) and save
    chapter_freqs_t_rel <- 100 * (chapter_freqs_t / sum(chapter_freqs_t))
    chapter_freqs_l[[chapter_title]] <- chapter_freqs_t_rel
  }
}

# Section 5.6 : Accessing and Processing List Items
# --------------------------------------------------

# Vector Recycling: In R, when operations involve vectors of different lengths, 
# R automatically "recycles" the shorter vector to match the longer one.
# In our case, this could help if chapters had different word sets.

# rbind : Used to bind rows of data together
# For rbind to work, the rows must have the same number of columns
x <- c(1,2,3,4,5)
y <- c(6,7,8,9,10)
rbind(x, y)

y <- c(6,7,8,9,10,11)
rbind(x, y)

# More recycling: Multiplying with shorter vectors
x <- c(1,2,3,4,5,6)
y <- 2
x * y # Every item in vector x gets multiplied by 2

x <- c(1,2,3,4,5,6)
y <- c(2,3)
x * y # R recycles the shorter vector 'y' to match the length of 'x'

# lapply : A function in the "apply" family designed specifically to work with lists
?apply
?lapply

# Similar to a for loop, lapply is a function for iterating over elements in a list
x <- list(a = 1:10, b = 2:25, b = 100:1090)
# A list containing the mean for each object in the list 'x':
lapply(x, mean)

# Accessing the first item in the list and filtering to the word type 'whale'
chapter_freqs_l[[1]]["whale"]

# Instead of repeating that line for every chapter, use lapply
lapply(chapter_freqs_l, '[', 'whale')

# By adding '[' as the function argument to lapply, we tell it to apply
# bracketed sub-setting to each item in the list
whale_l <- lapply(chapter_freqs_l, '[', 'whale')

# do.call: Used when we want to apply a function (e.g., rbind) to every list item
?do.call

x <- list(1:3, 4:6, 7:9)
# Convert this list into a matrix
do.call(rbind, x)

# Now we can create a matrix where each chapter is a row and the
# column holds relative frequency values for the word 'whale'
whales_m <- do.call(rbind, whale_l)
head(whales_m)

# Creating another matrix for the word 'ahab'
ahab_l <- lapply(chapter_freqs_l, '[', 'ahab')
ahabs_m <- do.call(rbind, ahab_l)
head(ahabs_m)
tail(ahabs_m)

# cbind : Binds matrices column-wise
?cbind
x <- c(1,2,3,4,5,6)
y <- c(2,4,5,6,7,8)
z <- c(24,23,34,32,12,10)
test_m <- cbind(x,y,z)
test_m
# to access hte value held in the second row and third column in the test matrix
test_m[2,3]
test_m[2,] #show all values in the second row
test_m[,1] #show all values in the first column
test_m[,"y"] # access the column by its name

# extract each column as a vector (whale and ahab frequencies)
whales_v <- whales_m[,1]
ahabs_v <- ahabs_m[,1]
# These are numeric vectors where each value corresponds to one chapter

# Bind the vectors together as columns in a new matrix
whales_ahabs_m <- cbind(whales_v, ahabs_v)
dim(whales_ahabs_m) #check dimension
colnames(whales_ahabs_m) <- c("whale", "ahab")

# Base R plot : compare 'whale' and 'ahab" by chapter
barplot(whales_ahabs_m, beside=TRUE) 

# Enhanced plot (using ggplot2)
library(tidyverse)
library(patchwork)

# Get chapter indices
chapter_index <- 1:nrow(whales_ahabs_m)

# Create a clean data frame with chapter index
whales_ahabs_df <- whales_ahabs_m %>%
  as.data.frame() %>%
  mutate(chapter = chapter_index)


# Plot 1: 'whale'
plot_whale <- ggplot(whales_ahabs_df, aes(x = chapter, y = whale)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Relative Frequency of 'whale' by Chapter",
    x = "Chapter Index",
    y = "Relative Frequency (%)"
  ) +
  theme_minimal()

# Plot 2: 'ahab'
plot_ahab <- ggplot(whales_ahabs_df, aes(x = chapter, y = ahab)) +
  geom_col(fill = "firebrick") +
  labs(
    title = "Relative Frequency of 'ahab' by Chapter",
    x = "Chapter Index",
    y = "Relative Frequency (%)"
  ) +
  theme_minimal()

# Combine vertically
plot_whale / plot_ahab



