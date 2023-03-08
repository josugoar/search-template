# This function must return a list with the information needed to solve the problem.
initialize.problem <- function(file) {
  problem <- list()
  
  # Compulsory attributes
  problem$name             <- paste0("Sudoku - [", file, "]")
  problem$state_initial    <- read.csv(file, header = FALSE)
  # There is an action for each individual cell
  problem$actions_possible <- data.frame(value = 1:9) 
  # In this problem final_state is unknown
  
  return(problem)
}

# Analyzes if an action can be applied in a state.
is.applicable <- function (state, action, problem) {
  value <- action

  where_put <- which(state == 0, arr.ind = TRUE)[1, ]
  where_is  <- which(state == value, arr.ind = TRUE)
  
  app_row <- any(where_is[, 1] == where_put[1])
  app_col <- any(where_is[, 2] == where_put[2])
  
  square <- floor((where_put - 0.01) / 3)
  square <- (square * 3) + 1
  square <- state[square[1] : (square[1] + 2), square[2] : (square[2] + 2)]
  app_squ <- any(square == value)
  
  return(!app_row & !app_col & !app_squ)
}

# Returns the state resulting on applying the action over the state.
effect <- function (state, action, problem) {
  result <- state
  
  where_put <- which(state == 0, arr.ind = TRUE)[1, ]
  result[where_put[1], where_put[2]] <- action
  
  return(result)
}

# Analyzes if a state is final or not
is.final.state <- function (state, final_state, problem) {
  return(length(which(state == 0)) == 0)
}

# Transforms a state into a string
to.string <- function (state, problem) {
    print(state)
}

# Returns the cost of applying an action over a state
get.cost <- function (action, state, problem) {
  return(1)
}

# Heuristic function used by Informed Search Algorithms
get.evaluation <- function(state, problem) {
	return(1)
}