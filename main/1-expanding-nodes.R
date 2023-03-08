# Clear environment and console
rm(list=ls())
cat("\014")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#---------------------------------------------------------------------------
# The problem "Wolf, goat, and cabbage"
# https://en.wikipedia.org/wiki/River_crossing_puzzle
# A farmer must transport a Wolf, a goat and cabbage from one side of a river 
# to another using a boat which can only hold one item in addition to the farmer, 
# subject to the constraints that the wolf cannot be left alone with the goat, 
# and the goat cannot be left alone with the cabbage

#---------------------------------------------------------------------------
# State: the state will be defined as a 4-elements (boolean) vector,
# representing each one of them the position of the elements:
# 1st position: location of the farmer  (TRUE: left / FALSE: right)
# 2nd position: location of the wolf    (TRUE: left / FALSE: right)
# 3rd position: location of the goat    (TRUE: left / FALSE: right)
# 4th position: location of the cabbage (TRUE: left / FALSE: right)

#A data.frame is like a table. We can define columns (with name) and 
#add row (with values for each column)
state_initial <- data.frame(farmer = TRUE, wolf = TRUE, goat = TRUE, cabbage = TRUE)
print(state_initial)

# Create several states
state1 <- data.frame(farmer = TRUE, wolf = TRUE, goat = TRUE, cabbage = TRUE)
state2 <- data.frame(farmer = FALSE, wolf = TRUE, goat = FALSE, cabbage = TRUE)
state3 <- data.frame(farmer = FALSE, wolf = FALSE, goat = FALSE, cabbage = FALSE)

# The final state test can be done by checking that all the elements are FALSE
(!state1$farmer && !state1$wolf && !state1$goat && !state1$cabbage)
(!state2$farmer && !state2$wolf && !state2$goat && !state2$cabbage)
(!state3$farmer && !state3$wolf && !state3$goat && !state3$cabbage)

# Or, in a simpler way, adding the elements in a vector and comparing with 0
sum(as.numeric(state1)) == 0
sum(as.numeric(state2)) == 0
sum(as.numeric(state3)) == 0

# Or, checking if all of the elements of the data.frame are equal to false
all(state1 == FALSE)
all(state2 == FALSE)
all(state3 == FALSE)

#---------------------------------------------------------------------------
# Actions: actions with their conditions and effects
# 4 different actions can be done
# 1: "farmer"  -> changes the side of the farmer alone
# 2: "wolf"    -> changes the side of the wolf and the farmer
# 3: "goat"    -> changes the side of the goat and the farmer
# 4: "cabbage" -> changes the side of the cabbage and the farmer

# 1: moving the farmer can be done if no restrictions are violated 
# 2-4: moving another element can be done if no restrictions are violated and 
# the element is in the same side of the river than the farmer

# We can check if moving the farmer can be done as:
(state1$wolf != state1$goat) && (state1$goat != state1$cabbage)
(state2$wolf != state2$goat) && (state2$goat != state2$cabbage)

# We can check if moving the goat can be done as:
(state1$goat == state1$farmer)
(state2$goat == state2$farmer)

# And moving the cabbage (wolf and goat cannot be in the same place)
(state1$wolf != state1$goat) && (state1$cabbage == state1$farmer)
(state2$wolf != state2$goat) && (state2$cabbage == state2$farmer)

# The effect of applying an action (goat, for instance) over a state is a new state 
# inverting the corresponding positions:
new_state <- state2
new_state$farmer <- !new_state$farmer #(!state2$farmer)
new_state$goat <- !new_state$goat #(!state2$goat)
new_state

#---------------------------------------------------------------------------
rm(list=ls())
cat("\014")

# Putting all toghether
# Defining a problem and its initialization
problem <- list()
problem$state_initial <- data.frame(farmer = TRUE, wolf = TRUE, goat = TRUE, cabbage = TRUE)
problem$actions_possible <- data.frame(action = c("farmer","wolf","goat","cabbage"), stringsAsFactors = FALSE)

# This function must return a list with the information needed to solve the problem.
initialize.problem <- function() {
  problem <- list()
  
  problem$name             <- paste0("River crossing puzzle")
  problem$state_initial    <- data.frame(farmer = TRUE, wolf = TRUE, goat = TRUE, cabbage = TRUE)
  problem$actions_possible <- data.frame(action = c("farmer", "wolf", "goat", "cabbage"), stringsAsFactors = FALSE)
  
  return(problem)
}

# Analyzes if an action can be applied in a state. There is a conditional for each action.
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
effect <- function (state, action) {
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
is.final.state <- function (state, final_state) {
  return(all(state == FALSE))
}

# Solving a problem
# I start from an initial state of a problem
problem <- initialize.problem()
state   <- problem$state_initial
actions <- problem$actions_possible

# Define a frontier list with nodes pending of "expansion"
frontier <- list(state)

# For the first state in the frontier, 
# I have to extract it and check if it is a final state.
# if not, I have to check which of the actions are applicable
current_state <- frontier[[1]]
frontier[[1]] <- NULL
current_state
is.final.state(current_state)
# Create a data.frame with all the applicable actions over the current.state
applicable_actions <- sapply(actions$action, function (x) is.applicable(current_state, x, problem))
applicable_actions

# Expand current.state: Apply all applicable actions and generate new states
res <- lapply(actions$action[applicable_actions], function(x) effect(current_state, x))
res
# Add successors nodes to the frontier list
frontier <- append(frontier, res)
frontier
# AND REPEAT THE PROCEDURE... UNTIL FINAL STATE FOUND!!!