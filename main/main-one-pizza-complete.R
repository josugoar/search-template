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
source("../algorithms/informed/local-beam-search.R")
source("../algorithms/informed/random-restart-hill-climbing.R")
source("../algorithms/informed/stochastic-hill-climbing.R")
source("../algorithms/informed/stochastic-random-restart.R")

# Include functions for data analysis and result plot
source("../algorithms/results-analysis/analyze-results.R")

# Include the problem
source("../problem/one-pizza-complete.R")

beams <- NULL
restarts <- NULL

# Executes hill climbing search and return the results
execute.hill.climbing <- function(filename) {
  # Initialize problem
  problem <- initialize.problem(filename = filename)
  # Execute hill climbing
  return(hill.climbing.search(problem = problem))
}

# Executes hill climbing search and return the results
execute.local.beam.search <- function(filename) {
  # Initialize problem
  problem <- initialize.problem(filename = filename)
  # Execute local beam search
  return(local.beam.search(problem = problem, beams))
}

# Executes random restart hill climbing search and return the results
execute.random.restart.hill.climbing <- function(filename) {
  # Execute random restart hill climbing
  return(random.restart.hill.climbing(file = filename, restarts))
}

# Executes stochastic hill climbing search and return the results
execute.stochastic.hill.climbing <- function(filename) {
  # Initialize problem
  problem <- initialize.problem(filename = filename)
  # Execute stochastic hill climbing
  return(stochastic.hill.climbing(problem = problem, restarts))
}

# Executes stochastic random restart search and return the results
execute.stochastic.random.restart <- function(filename) {
  # Execute stochastic random restart
  return(stochastic.random.restart(file = filename, restarts))
}

# Execute several times and analyze results
test <- function(file, times, execute) {
  # Execute 'n' times
  results <- vector(mode = "list", length = times)
  
  for (i in 1:times) {
    results[[i]] <- execute(filename = file)
  }
  
  # Initialize a problem instance for the analysis
  problem <- initialize.problem(filename = file)
  
  # Analyze results
  results_df <- local.analyze.results(results, problem)
  
  print(paste0("Best evaluation: ", round(min(results_df$Evaluation), 2), 
               " - Mean: ", round(mean(results_df$Evaluation), 2), 
               " - SD: ", round(sd(results_df$Evaluation), 2)), quote = FALSE)
  print(paste0("Best runtime: ", round(min(results_df$Runtime), 2), 
               " - Mean: ", round(mean(results_df$Runtime), 2), 
               " - SD: ", round(sd(results_df$Runtime), 2)), quote = FALSE)
  
  # Print results in an HTML Table
  kable_material(kbl(results_df, caption = paste(problem$name, sep = " - ")),
                 c("striped", "hover", "condensed", "responsive"))
}

# Clear console
cat("\014")
graphics.off()

file        <- "../data/one-pizza/a_an_example.in.txt"
times       <- 10
test(file, times, execute.hill.climbing)
beams <- 3
test(file, times, execute.local.beam.search)
beams <- 5
test(file, times, execute.local.beam.search)
beams <- 10
test(file, times, execute.local.beam.search)
restarts <- 5
test(file, times, execute.random.restart.hill.climbing)
test(file, times, execute.stochastic.hill.climbing)
test(file, times, execute.stochastic.random.restart)
restarts <- 15
test(file, times, execute.random.restart.hill.climbing)
test(file, times, execute.stochastic.hill.climbing)
test(file, times, execute.stochastic.random.restart)
restarts <- 30
test(file, times, execute.random.restart.hill.climbing)
test(file, times, execute.stochastic.hill.climbing)
test(file, times, execute.stochastic.random.restart)
