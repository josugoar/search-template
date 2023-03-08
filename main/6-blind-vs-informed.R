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
# Informed algorithm
source("../algorithms/informed/greedy-best-first-search.R")

# Include functions for data analysis and result plot
source("../algorithms/results-analysis/analyze-results.R")

# 8-Puzzle Problem
source("../problem/8Puzzle.R")

# Let's check different states
state0 <- initialize.problem(3, 3, c(0, 1, 2, 3, 4, 5, 6, 7, 8))$state_initial
to.string(state0)
state1 <- initialize.problem(3, 3, c(1, 2, 5, 3, 0, 4, 6, 7, 8))$state_initial
to.string(state1)
state2 <- initialize.problem(3, 3, c(1, 2, 5, 3, 4, 8, 0, 6, 7))$state_initial
to.string(state2)
state3 <- initialize.problem(3, 3, c(1, 2, 3, 0, 4, 5, 6, 7, 8))$state_initial
to.string(state3)

problem <- initialize.problem(3, 3)
# Defining an heuristic... Number of misplaced tiles
state1 != problem$state_final
sum(state1 != problem$state_final)

# Put it inside a function
misplaced <- function(state, problem) {
  return(sum(state != problem$state_final))
}

misplaced(state0, problem)
misplaced(state1, problem)
misplaced(state2, problem)
misplaced(state3, problem)

# Comparison of the method versus Iterative Deepening Search on a instance of the problem
problem <- initialize.problem(3, 3, c(1, 2, 5, 3, 4, 8, 0, 6, 7))

bfs_gs  <- breadth.first.search(problem, max_iterations = 2000, count_print = 1000,graph_search = TRUE)
dfs_gs  <- depth.first.search(problem, max_iterations = 2000, count_print = 1000, graph_search = TRUE)
dls_gs  <- depth.limited.search(problem, depth_limit = 6, max_iterations = 2000, count_print = 1000, graph_search = TRUE)
ids_gs  <- iterative.deepening.search(problem, max_iterations = 2000, count_print = 1000, graph_search = TRUE)
gbfs_gs <- greedy.best.first.search(problem, max_iterations = 2000, count_print = 1000, graph_search = TRUE)

# Analyze the result of all the executions
results <- analyze.results(list(bfs_gs, dfs_gs, dls_gs, ids_gs, gbfs_gs), problem)
# Print results in an HTML Table
kable_material(kbl(results, caption = "Blind vs. Informed Search"), c("striped", "hover", "condensed", "responsive"))
