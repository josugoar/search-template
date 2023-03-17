# =======================================================================
# Group Name:
# Students:
# =======================================================================
# You must implement the different functions according to your problem.
# You cannot modify the function headers because they are used by the 
# search algorithms. If you modify any headers the algorithms may not work.
# =======================================================================

# This function must return a list with the information needed to solve the problem.
# (Depending on the problem, it should receive or not parameters)
initialize.problem <- function(filename, satisfied = 0.3) {
  lines <- readLines(filename)
  customers <- as.numeric(lines[1])
  likes <- list()
  dislikes <- list()
  for (i in 2:(2 * customers + 1)) {
    line <- unlist(strsplit(lines[i], " "))
    n <- as.numeric(line[1])
    if (n == 0) {
      ingredients <- c()
    } else {
      ingredients <- line[2:(n + 1)]
    }
    if (i %% 2 == 0) {
      likes <- append(likes, list(ingredients))
    } else {
      dislikes <- append(dislikes, list(ingredients))
    }
  }
  actions <- unique(unlist(likes))
  
  problem <- list() # Default value is an empty list.
  
  # This attributes are compulsory
  problem$name              <- paste0("One-pizza [filename = ", filename, " - Clients = ", customers, " - Diff. ingredients = ", length(actions), " - Satisfied = ", satisfied * 100,"%]")
  problem$state_initial     <- c()
  problem$state_final       <- NULL
  problem$actions_possible  <- data.frame(action = actions, stringsAsFactors = FALSE)
  
  # You can add additional attributes
  problem$satisfied  <- satisfied
  problem$customers  <- customers
  problem$likes      <- likes
  problem$dislikes   <- dislikes
  
  return(problem)
}

# Analyzes if an action can be applied in the received state.
is.applicable <- function (state, action, problem) {
  result <- FALSE # Default value is FALSE.
  
  result <- !(action %in% state)
  
  return(result)
}

# Returns the state resulting on applying the action over the state
effect <- function (state, action, problem) {
  result <- state # Default value is the current state.
  
  result <- sort(append(state, action))
  
  return(result)
}

# Analyzes if a state is final or not
is.final.state <- function (state, final_satate, problem) {
  result <- FALSE # Default value is FALSE.
  
  result <- get.satisfied(state, problem) >= problem$satisfied
  
  return(result)
}

# Transforms a state into a string
to.string = function (state, problem) {
  
  return(paste0(state, collapse = ", "))
}

# Returns the cost of applying an action over a state
get.cost <- function (action, state, problem) {
  
  return(1) # Default value is 1.
}

# Heuristic function used by Informed Search Algorithms
get.evaluation <- function(state, problem) {
  
  return(1 - min(get.satisfied(state, problem), problem$satisfied) / problem$satisfied)
  
	return(1) # Default value is 1.
}

get.satisfied <- function (state, problem) {
  return(sum(sapply(1:problem$c, function(i) all(problem$l[[i]] %in% state) & !any(state %in% problem$d[[i]]))) / problem$c)
}