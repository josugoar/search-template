# This function must return a list with the information needed to solve the problem.
# (Depending on the problem, it should receive or not parameters)
initialize.problem <- function(filename, p) {
  problem <- list() # Default value is an empty list.
  #P = number of hubs
  problem$p         <- p
  problem$name      <- paste0("p-Hub - [", filename, " - p=", problem$p, "]")
  #Size is the number of airports
  problem$size      <- as.numeric(unlist(read.csv(filename, header=FALSE, nrows=1)))
  #Distances data.frame
  problem$distances <- read.csv(filename, header=FALSE, skip=problem$size+2, dec=".", sep=" ")

  # Initial state is generated at random
  problem$state_initial <- sample(c(1:problem$size), problem$p)
  # There is no final state
  problem$state_final   <- NULL
  # Action is "Change one of the hubs". There are as many actions as the number of hubs. 
  problem$actions_possible <- data.frame(action = c(1:problem$p), stringsAsFactors = FALSE)

  return(problem)
}

# Analyzes if an action can be applied in the received state.
is.applicable <- function (state, action, problem) {
  # Any action is applicable
  return(TRUE)
}

# Returns the state resulting on applying the action over the state
effect <- function (state, action, problem) {
  result <- state # Default value is the current state.
  
  # Modifies the hub of the position defined by the action. 
  # The new airport is chosen randomly from all airports except current hubs.  
  result[action] <- sample(setdiff(c(1:problem$size), state), 1)
  
  return(result)
}

# Analyzes if a state is final or not
is.final.state <- function (state, final_satate, problem) {
  # Final state is unknown
  return(TRUE)
}

# Transforms a state into a string
to.string = function (state, problem) {
  return(paste0(state, collapse = ", "))
}

# Returns the cost of applying an action over a state
get.cost <- function (action, state, problem) {
  return(1) # Default value is 1.
}

# Heuristic function used by Informed Search Algorithms.
# Distance between all airports, taking into account the distribution of hubs
get.evaluation <- function(state, problem) {
  total_distance <- 0
  i <- 1
  
  # Create a list of airports closest to each hub.
  airports_lists <- split.airports(state, problem)
    
  # The distance between each pair of airports is calculated.
  while (i <= problem$size) {
    j <- i+1
    
    while (j <= problem$size) {
      hub_i <- get.hub(i, airports_lists)
      hub_j <- get.hub(j, airports_lists)
      
      # The distance is the sum of the distance between each secondary airport and its hub.
      # The hub is the first element of each airports list.
      total_distance <- total_distance + 
        problem$distances[i, airports_lists[[hub_i]][1]] +
        problem$distances[j, airports_lists[[hub_j]][1]]
      # If the hubs are different, the distance between the hubs is also added.
      if (hub_i != hub_j) {
        total_distance <- total_distance + 
          problem$distances[airports_lists[[hub_i]][1], airports_lists[[hub_j]][1]]
      }
      
      j <- j+1
    }
    
    i <- i+1
  }
  
	return(total_distance)
}

# Gets the ordinal hub number with which an airport is associated.
get.hub <- function(airport, airports_lists) {
  for (i in 1:length(airports_lists)) {
    if (airport %in% airports_lists[[i]]) {
      return(i)
    }
  }
}

# Split the airports into a list for each hub of the state. 
# In each list are the closest airports to each hub. 
# The hub itself is the first item in each list.
split.airports <- function(state, problem) {
  # Create lists for secondary airports
  airports_lists <- vector(mode = "list", length = problem$p)
  # Insert hub airports into each list
  for (i in 1:problem$p) {
    airports_lists[[i]] <- c(airports_lists[[i]], state[i])
  }
  
  # For each secondary airport
  for (i in 1:problem$size) {
    # If airport is not a hub
    if (!(i %in% state)) {
      # Obtain distances from the airport 'i' to each hub
      distance_to_hub <- lapply(state, FUN = function(x, y) { return(problem$distances[x,y]) }, y=i)
      # Select nearest hub
      hub <- which.min(distance_to_hub)
      # Add secondary airport to the appropriate list
      airports_lists[[hub]] <- c(airports_lists[[hub]], i)
    }
  }
  
  return(airports_lists)
}
