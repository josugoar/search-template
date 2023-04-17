# This function must return a list with the information needed to solve the problem.
initialize.problem <- function(filename, max_customers = 100) {
  problem <- list() # Default value is an empty list.
  
  #Read only first line
  lines <- readLines(filename, n=1)
  
  #Number of clients
  problem$customers <- as.numeric(lines[1])
  
  #Limit the number of clients to max_customers
  if (problem$customers > max_customers) {
    lines <- readLines(filename, n=(max_customers*2)+1)
    problem$customers <- max_customers
  } else {
    lines <- readLines(filename)
  }
  
  #Remove first element of lines
  lines <- lines[-1]
  
  #Remove the number and blank from each line
  lines <- substr(lines, start = 3, stop = nchar(lines))
  #Split ingredients 
  lines <- strsplit(lines, " ")
  
  #Get like ingredients (odd lines)
  problem$like <- lines[seq(1, length(lines), by = 2)]

  #Get different ingredients
  problem$ingredients <- unique(unlist(problem$like))

  #Action is "Modify ingredient value". There are as many actions as ingredients.
  problem$actions_possible <- data.frame(action = c(1:length(problem$ingredients)), stringsAsFactors = FALSE)

  #Get dislike ingredients (even lines)
  problem$dislike <- lines[seq(2, length(lines), by = 2)]

  #The state is a vector (0 | 1) whose size is equal to the number of different ingredients.
  #The value of each position indicates whether the ingredient is present in the one pizza or not.
  problem$state_initial <- problem$state_initial <- sample(c(0,1), size = length(problem$ingredients), replace = TRUE, prob = c(0.5, 0.5))

  #Final state is unknown
  problem$state_final   <- NULL

  problem$name  <- paste0("One-pizza [filename = ", filename, 
                          " - Clients = ", length(problem$like), 
                          " - Diff. ingredients = ", nrow(problem$actions_possible), "]")
  return(problem)
}

# Analyzes if an action can be applied in the received state.
is.applicable <- function (state, action, problem) {
  #All actions all always applicable
  return(TRUE)
}

# Returns the state resulting on applying the action over the state
effect <- function (state, action, problem) {
  result <- state
  
  #The value of the position specified by the action is changed (1 to 0 & 0 to 1)
  result[action] <- abs(result[action]-1)
  
  return(result)
}

# Analyzes if a state is final or not
is.final.state <- function (state, final_satate, problem) {
  #Any state is final
  return(TRUE)
}

# Transforms a state into a string
to.string = function (state, problem) {
  #An array with the ingredients that are present in the pizza is created.
  present_ingredients <- problem$ingredients[state == 1]
  
  return(paste0(present_ingredients, collapse = ", "))
}

# Returns the cost of applying an action over a state
get.cost <- function (action, state, problem) {
  #An array with the ingredients that are present in the pizza is created.
  present_ingredients <- problem$ingredients[state == 1]
  #Obtain satisfied clients
  satisfied <- satisfied.clients(present_ingredients, problem)
  
  return(satisfied) # Default value is 1.
}

# Heuristic function used by Informed Search Algorithms.
get.evaluation <- function(state, problem) {
  #An array with the ingredients that are present in the pizza is created.
  present_ingredients <- problem$ingredients[state == 1]
  #Obtain unsatisfied clients
  unsatisfied <- unsatisfied.clients(present_ingredients, problem)
  
  return(unsatisfied)
}

# Counts satisfied customers
satisfied.clients <- function(ingredients, problem) {
  satisfied <- 0;
  
  for (i in 1:length(problem$like)) {
    #All liked and none disliked ingredients are in the state.
    if (all(problem$like[[i]] %in% ingredients) && 
        (length(problem$dislike[[i]]) == 0 || !all(problem$dislike[[i]] %in% ingredients))) {
      satisfied <- satisfied + 1;
    }
  }
  
  return(satisfied)
}

# Counts unsatisfied customers
unsatisfied.clients <- function(ingredients, problem) {
  unsatisfied <- 0;
  
  for (i in 1:length(problem$like)) {
    # Not all liked ingredients or at least one disliked ingredient is in the state.
    if (!all(problem$like[[i]] %in% ingredients) || any(problem$dislike[[i]] %in% ingredients)) {
      unsatisfied <- unsatisfied + 1;
    }
  }
  
  return(unsatisfied)
}
