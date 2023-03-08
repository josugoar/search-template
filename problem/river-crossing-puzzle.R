# This function must return a list with the information needed to solve the problem.
initialize.problem <- function() {
  problem <- list()
  
  # Compulsory attributes
  problem$name <- paste0("River crossing puzzle")
  problem$state_initial    <- data.frame(farmer = TRUE, wolf = TRUE, goat = TRUE, cabbage = TRUE)
  # There is an action for the movement of each element from one side to the other
  problem$actions_possible <- data.frame(action = c("farmer", "wolf", "goat", "cabbage"), stringsAsFactors = FALSE)
  # In this problem final_state is unknown
  
  return(problem)
}

# Analyzes if an action can be applied in a state.
# There is an IF for each action.
is.applicable <- function (state, action, problem) {
  if (action == "farmer") {
    return (state$wolf != state$goat) && (state$goat != state$cabbage)
  }
  
  if (action == "wolf") {
    return (state$goat != state$cabbage) && (state$farmer == state$wolf)
  }
  
  if (action == "goat") {
    return (state$farmer == state$goat)
  }
  
  if (action == "cabbage") {
    return (state$goat != state$cabbage) && (state$farmer == state$cabbage)
  }
}

# Returns the state resulting on applying the action over the state.
# There is an IF for each action.
effect <- function (state, action, problem) {
  result <- state
  result$farmer <- !result$farmer
  
  if (action == "wolf") {
    result$wolf <- !result$wolf
    return(result)
  }
  
  if (action == "goat") {
    result$goat <- !result$goat
    return(result)
  }
  
  if (action == "cabbage") {
    result$cabbage <- !result$cabbage
    return(result)
  }
  
  return(result)
}

# Analyzes if a state is final or not
is.final.state <- function (state, final_state, problem) {
  return(all(state == FALSE))
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