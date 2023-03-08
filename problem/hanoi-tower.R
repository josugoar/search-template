#Import needed library
library("gtools")

# This function must return a list with the information needed to solve the problem.
# (Depending on the problem, it should receive or not parameters)
initialize.problem <- function(rods = 3, disks = 4, random_actions = FALSE) {
  problem <- list() # Default value is an empty list.

  # This attributes are compulsory
  problem$name             <- paste0("Hanoi tower (r=", rods, ", d=", disks, ")")
  problem$state_initial    <- rep(1, disks)
  problem$state_final      <- rep(rods, disks)
  
  # Create a vector with the permutations "origin", "destination" concatenated with an arrow '->'
  actions <- apply(permutations(rods, 2, repeats.allowed = FALSE), 1, paste0, collapse="->")
  
  if (random_actions) {
    actions <- sample(actions)
  }
  
  #Create a dataframe of strings with a column. Each row is an action
  problem$actions_possible <- data.frame(action = actions, stringsAsFactors = FALSE)
  problem$rods  <- rods
  problem$disks <- disks
  
  return(problem)
}

# Analyzes if an action can be applied in the received state.
is.applicable <- function (state, action, problem) {
  # Get origin rod: the number before the arrow '->'
  origin <- as.numeric(sub("\\->.*", "", action))
  # Get destination rod: the number after the arrow
  dest   <- as.numeric(sub(".*\\->", "", action))
  
  # If there is no disk at origin rod, return false
  if (!any(state == origin)) {
    return(FALSE)
  # If there is no disk at destination rod, return true
  } else if (!any(state == dest)) {
    return(TRUE)
  # Check the size of the disks
  } else {
    # Obtain the size of the discs at the top of origin and dest rods
    size_orig <- tail(which(state == origin), n = 1)
    size_dest <- tail(which(state == dest), n = 1)
    
    return(size_dest < size_orig)
  }
}

# Returns the state resulting on applying the action over the state
effect <- function (state, action, problem) {
  result <- state
  
  # Get origin rod: the number before the arrow '->'
  origin <- as.numeric(sub("\\->.*", "", action))
  # Get destination rod: the number after the arrow
  dest   <- as.numeric(sub(".*\\->", "", action))
  
  # Modify the rod of the top most disk of origin rod
  result[tail(which(result == origin), n = 1)] <- dest
  
  return(result)
}

# Analyzes if a state is final or not
is.final.state <- function (state, final_state, problem) {
  return(all(state == final_state))
}

# Transforms a state into a string
to.string = function (state, problem) {
  return(paste(state, collapse=","))
}

# Returns the cost of applying an action over a state
get.cost <- function (action, state, problem) {
  return(1) # Default value is 1.
}

# Heuristic function used by Informed Search Algorithms
get.evaluation <- function(state, problem) {
  return(problem$disks - length(which(state == problem$rods)))
}