# Clear environment and console
rm(list=ls())
cat("\014")
graphics.off()
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Import the libraries needed to display the results
library(kableExtra)
library(magrittr)

# Here we have the set of functions that define a problem
# (Let's take a look to the functions)
source("../problem/river-crossing-puzzle.R")

# Include algorithm functions
source("../algorithms/blind/expand-node.R")
source("../algorithms/blind/breadth-first-search.R")
source("../algorithms/blind/depth-first-search.R")
source("../algorithms/blind/depth-limited-search.R")
source("../algorithms/blind/iterative-deepening-search.R")

# Include functions for data analysis and result plot
source("../algorithms/results-analysis/analyze-results.R")

# Execution of the algorithm
problem <- initialize.problem()
bfs_1 <- breadth.first.search(problem)

# All the algorithms have several parameters. Test max_iterations, count_print and trace
bfs_2 <- breadth.first.search(problem, max_iterations = 50, count_print = 10, trace = TRUE)

# Let's see the difference between distinct algorithms
bfs_ts <- breadth.first.search(problem, max_iterations = 100, count_print = 50)   
bfs_gs <- breadth.first.search(problem, max_iterations = 100, count_print = 50, graph_search = TRUE)
dfs_ts <- depth.first.search(problem, max_iterations = 100, count_print = 50)
dfs_gs <- depth.first.search(problem, max_iterations = 100, count_print = 50, graph_search = TRUE)
dls_ts <- depth.limited.search(problem, max_iterations = 100, count_print = 50, depth_limit = 3)
dls_gs <- depth.limited.search(problem, max_iterations = 100, count_print = 50, depth_limit = 3, graph_search = TRUE)
ids_ts <- iterative.deepening.search(problem, max_iterations = 100, count_print = 50)
ids_gs <- iterative.deepening.search(problem, max_iterations = 100, count_print = 50, graph_search = TRUE)

# Analyze the result of all the executions
results <- analyze.results(list(bfs_ts, bfs_gs, 
                                dfs_ts, dfs_gs, 
                                dls_ts, dls_gs, 
                                ids_ts, ids_gs), problem)
# Print results in an HTML Table
kable_material(kbl(results, caption = "River Crossing"), c("striped", "hover", "condensed", "responsive"))
