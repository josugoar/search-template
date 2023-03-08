expand.node = function(current_node, actions_possible, problem) {
  successors <- list()
  
  for (i in 1 : nrow(actions_possible)) {
    action <- actions_possible[i, ]
    
    if (is.applicable(current_node$state, action, problem)) {
      successor               <- list()
      successor$parent        <- current_node
      successor$state         <- effect(current_node$state, action, problem)    
      successor$actions       <- rbind(current_node$actions, action)
      successor$depth         <- current_node$depth + 1
      successor$cost          <- current_node$cost + get.cost(action, current_node$state, problem)
      successor$evaluation    <- get.evaluation(successor$state, problem)
      
      successors <- append(successors, list(successor))
    }
  }
  
  return(successors)
}

local.expand.node = function(current_node, actions_possible, problem) {
  successors <- list()
  
  for (i in 1 : nrow(actions_possible)) {
    action <- actions_possible[i, ]
    
    if (is.applicable(current_node$state, action, problem)) {
      successor               <- list()
      successor$parent        <- current_node
      successor$state         <- effect(current_node$state, action, problem)    
      successor$actions       <- rbind(current_node$actions, action)
      successor$depth         <- current_node$depth + 1
      successor$cost          <- get.cost(action, current_node$state, problem)
      successor$evaluation    <- get.evaluation(successor$state, problem)
      
      successors <- append(successors, list(successor))
    }
  }
  
  return(successors)
}