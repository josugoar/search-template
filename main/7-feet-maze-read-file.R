# Clear environment and console
rm(list=ls())
cat("\014")
graphics.off()
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# File name
file <- "../data/feet-maze-1b.txt"
# Read all data
full_data   <- read.csv(file, header = FALSE)
# Read first line
first_line  <- unlist(read.csv(file, header = FALSE, nrows = 1, sep = ";"))
# Convert first line in a vector with two values: [1] rows & [2] columns
size <- unlist(first_line)
# Read maze data to a data.frame
maze_data   <- read.csv(file, header = FALSE, skip = 1, nrows = size[1], sep = ";")
# Read start position X, Y into a vector
start_position <- unlist(read.csv(file, header = FALSE, skip = size[1] + 1, nrows = 1, sep = ","))
# Transform 0-N coordinates to 1-N
start_position <- start_position + 1
# Read end position X, Y into a vector
end_position <- unlist(read.csv(file, header = FALSE, skip = size[1] + 2, nrows = 1, sep = ","))
# Transform 0-N coordinates to 1-N
end_position <- end_position + 1
# Left walls
left_walls <- unlist(read.csv(file, header = FALSE, skip = size[1] + 3, nrows = 1, sep = ";"))