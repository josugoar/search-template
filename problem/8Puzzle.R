# This function must return a list with the information needed to solve the problem.
initialize.problem <- function(rows = 3, columns = 3, perm = sample(0 : (rows * columns - 1))) {
  problem <- list()

  # Compulsory attributes
  problem$name              <- paste0("8-Puzzle (", rows, "x", columns, ") - [", paste0(perm, collapse = "-"), "]")
  problem$state_initial     <- matrix(perm, nrow = rows, byrow = TRUE)
  problem$state_final       <- matrix(0:(rows * columns - 1), nrow = rows, byrow = TRUE)
  # There are 4 actions that move blank space: UP, DOWN, LEFT, RIGHT
  problem$actions_possible  <- data.frame(direction = c("Up", "Down", "Left", "Right"), stringsAsFactors = FALSE)
  
  # Additional attributes
  problem$rows              <- rows
  problem$columns           <- columns
  
  return(problem)
}

# Analyzes if an action can be applied in a state.
# There is an IF for each action.
is.applicable <- function (state, action, problem) {
  # Get the location of "blank" in the matrix
  where <- which(state == 0, arr.ind = TRUE)
  row <- where[1]
  col <- where[2]
  
  if (action == "Up") {
    return(row != 1)
  }
  
  if (action == "Down") {
    return(row != problem$rows)
  }
  
  if (action == "Left") {
    return(col != 1)
  }
  
  if (action == "Right") {
    return(col != problem$columns)
  }
  
  return(FALSE)
}

# Returns the state resulting on applying the action over the state.
# There is an IF for each action.
effect = function (state, action, problem) {
  # Get the location of "blank" in the matrix
  where <- which(state == 0, arr.ind = TRUE)
  row <- where[1]
  col <- where[2]
  result <- state  
  
  if (action == "Up") {
    result[row-1, col] <- state[row, col]
    result[row, col]   <- state[row-1, col]
    return(result)
  }
  
  if (action == "Down") {
    result[row+1, col] <- state[row, col]
    result[row, col]   <- state[row+1, col]
    return(result)
  }
  
  if (action == "Left") {
    result[row, col-1] <- state[row, col]
    result[row, col]   <- state[row, col-1]
    return(result)
  }
  
  if (action == "Right") {
    result[row, col+1] <- state[row, col]
    result[row, col]   <- state[row, col+1]
    return(result)
  }
}

# Analyzes if a state is final or not
is.final.state <- function (state, final_state, problem) {
  return(sum(state == final_state) == (nrow(state) * ncol(state)))
}

# Transforms a state into a string
to.string <- function (state, problem) {
  for (i in 1:nrow(state)) {
    print(state[i, ])
  }
}

# Returns the cost of applying an action over a state
get.cost = function (action, state, problem) {
  return(1)
}

# Heuristic function used by Informed Search Algorithms
get.evaluation = function(state, problem) {
  return(sum(state != problem$state_final))
}