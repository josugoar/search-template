random.restart.hill.climbing.search = function(problem,
                                               max_iterations = 50, 
                                               count_print = 10, 
                                               trace = FALSE,
                                               restarts = 0) {
  
  name_method      <- paste0("Random Restart Hill Climbing Search")
  state_initial    <- problem$state_initial
  actions_possible <- problem$actions_possible
  
  # Get Start time
  start_time       <- Sys.time()
  
  result <- NULL
  restart <- 0
  
  while (restart <= restarts) {
    state_initial    <- problem$state_initial

    while (sample(c(TRUE, FALSE), 1)) {
      sucessor_nodes <- local.expand.node(state_initial, actions_possible, problem)
      state_initial <- sample(successor_nodes, 1)
    }
    
    hill_climb_result <- hill.climbing.search(problem,
                                              max_iterations = max_iterations,
                                              count_print = count_print,
                                              trace = trace)
    
    if (!is.null(result) && hill_climb_result$evaluation <= result$state_final$evaluation) {
      result <- hill_climb_result
    }
    
    restart <- restart + 1
  }
  
  # Get runtime
  end_time <- Sys.time()
  
  result$name    <- name_method
  result$runtime <- end_time - start_time
  
  return(result)
}
