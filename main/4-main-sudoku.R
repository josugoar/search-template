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
source("../algorithms/blind/breadth-first-search.R")
source("../algorithms/blind/depth-first-search.R")
source("../algorithms/blind/depth-limited-search.R")
source("../algorithms/blind/iterative-deepening-search.R")

# Include functions for data analysis and result plot
source("../algorithms/results-analysis/analyze-results.R")

# Let's try with the Sudoku
source("../problem/Sudoku.R")
problem <- initialize.problem("../data/sudoku-1.txt") # Easy sudoku - Only 7 blank numbers

bfs_ts <- breadth.first.search(problem, max_iterations = 2500, count_print = 1000)   
bfs_gs <- breadth.first.search(problem, max_iterations = 2500, count_print = 1000, graph_search = TRUE)
dfs_ts <- depth.first.search(problem, max_iterations = 2500, count_print = 1000)
dfs_gs <- depth.first.search(problem, max_iterations = 2500, count_print = 1000, graph_search = TRUE)
dls6_ts <- depth.limited.search(problem, depth_limit = 6, max_iterations = 2500, count_print = 1000)
dls6_gs <- depth.limited.search(problem, depth_limit = 6, max_iterations = 2500, count_print = 1000, graph_search = TRUE)
dls10_ts <- depth.limited.search(problem, depth_limit = 49, max_iterations = 2500, count_print = 1000)
dls10_gs <- depth.limited.search(problem, depth_limit = 49, max_iterations = 2500, count_print = 1000, graph_search = TRUE)
ids_ts <- iterative.deepening.search(problem, max_iterations = 2500, count_print = 1000)
ids_gs <- iterative.deepening.search(problem, max_iterations = 2500, count_print = 1000, graph_search = TRUE)

# Analyze the result of all the executions
results <- analyze.results(list(bfs_ts, bfs_gs, 
                                dfs_ts, dfs_gs, 
                                dls6_ts, dls6_gs, 
                                dls10_ts, dls10_gs, 
                                ids_ts, ids_gs), problem)
# Print results in an HTML Table
kable_material(kbl(results, caption = "Sudoku Easy"), c("striped", "hover"))

problem <- initialize.problem("../data/sudoku-2.txt") # Hard sudoku

bfs_ts <- breadth.first.search(problem, max_iterations = 2500, count_print = 1000)   
bfs_gs <- breadth.first.search(problem, max_iterations = 2500, count_print = 1000, graph_search = TRUE)
dfs_ts <- depth.first.search(problem, max_iterations = 2500, count_print = 1000)
dfs_gs <- depth.first.search(problem, max_iterations = 2500, count_print = 1000, graph_search = TRUE)
dls_ts <- depth.limited.search(problem, depth_limit = 49, max_iterations = 2500, count_print = 1000)
dls_gs <- depth.limited.search(problem, depth_limit = 49, max_iterations = 2500, count_print = 1000, graph_search = TRUE)

# Analyze the result of all the executions
results <- analyze.results(list(bfs_ts, bfs_gs, 
                                dfs_ts, dfs_gs, 
                                dls_ts, dls_gs), problem)
# Print results in an HTML Table
kable_material(kbl(results, caption = "Sudoku Hard"), c("striped", "hover", "condensed", "responsive"))
