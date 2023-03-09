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
initialize.problem <- function(filename) {
  lines <- readLines(filename)
  c <- as.numeric(lines[1])
  l <- list()
  d <- list()
  for (i in 2:(2 * c + 1)) {
    line <- unlist(strsplit(lines[i], " "))
    n <- as.numeric(line[1]) + 1
    if (n == 1) {
      ingredients <- list()
    } else {
      ingredients <- line[2:n]
    }
    if (i %% 2 == 0) {
      l <- append(l, list(ingredients))
    } else {
      d <- append(d, list(ingredients))
    }
  }

  problem <- list() # Default value is an empty list.
  
  # This attributes are compulsory
  problem$name              <- paste0("One Pizza - [", filename, "]")
  problem$state_initial     <- list()
  problem$state_final       <- NULL
  problem$actions_possible  <- data.frame(action = unique(unlist(l)), stringsAsFactors = FALSE)
  
  # You can add additional attributes
  problem$c  <- c
  problem$l  <- l
  problem$d  <- d
  
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
  
  result <- append(state, action)
  
  return(result)
}

# Analyzes if a state is final or not
is.final.state <- function (state, final_satate, problem) {
  result <- FALSE # Default value is FALSE.

  result <- (sum(sapply(1:problem$c, function(i) all(problem$l[[i]] %in% state) & !any(state %in% problem$d[[i]]))) / problem$c) >= 0.3
    
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
    
	return(1) # Default value is 1.
}