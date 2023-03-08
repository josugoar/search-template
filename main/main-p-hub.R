# Clear environment and console
rm(list=ls())
cat("\014")
graphics.off()
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Import the libraries needed to display the results
library(kableExtra)
library(magrittr)

# Include algorithm functions
source("../algorithms/blind/expand-node.R")
source("../algorithms/informed/hill-climbing-search.R")

# Include functions for data analysis and result plot
source("../algorithms/results-analysis/analyze-results.R")

# Include the problem
source("../problem/p-hub-problem.R")

# Executes hill climbing search and return the results
execute.hill.climbing <- function(filename, p) {
  # Initialize problem
  problem <- initialize.problem(p = p, filename = filename)
  # Execute hill climbing
  return(hill.climbing.search(problem = problem))
}

# Execute Hill Climbing several times and analyze results
test.hill.climbing <- function(file, p, times) {
  # Execute hill climbing 'n' times
  results <- vector(mode = "list", length = times)
  
  for (i in 1:times) {
    results[[i]] <- execute.hill.climbing(filename = file, p = p)
  }
  
  # Initialize a problem instance for the analysis
  problem <- initialize.problem(filename = file, p = p)
  
  # Analyze results
  results_df <- local.analyze.results(results, problem)
  
  print(paste0("Best evaluation: ", round(min(results_df$Evaluation), 2), 
               " - Mean: ", round(mean(results_df$Evaluation), 2), 
               " - SD: ", round(sd(results_df$Evaluation), 2)), quote = FALSE)
  print(paste0("Best runtime: ", round(min(results_df$Runtime), 2), 
               " - Mean: ", round(mean(results_df$Runtime), 2), 
               " - SD: ", round(sd(results_df$Runtime), 2)), quote = FALSE)
  
  # Print results in an HTML Table
  kable_material(kbl(results_df, caption = paste(problem$name, "hill.climbing", sep = " - ")),
                 c("striped", "hover", "condensed", "responsive"))
}

# Clear console
cat("\014")
graphics.off()

file        <- "../data/p-hub/AP40.txt"
p           <- 4
times       <- 5
test.hill.climbing(file, p, times)

file        <- "../data/p-hub/AP100.txt"
p           <- 3
times       <- 5
test.hill.climbing(file, p, times)
