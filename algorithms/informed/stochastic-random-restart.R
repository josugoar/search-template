stochastic.random.restart = function(file,
                                     restarts,
                                     max_iterations = 50, 
                                     count_print = 10, 
                                     trace = FALSE) {
  
  name_method <- paste0("Stochastic Random Restart Hill Climbing Search")
  
  # Get Start time
  start_time  <- Sys.time()
  
  state_final <- NULL
  report      <- NULL
  
  count <- 0
  
  while (count <= restarts) {
    problem <- initialize.problem(file)
    result <- stochastic.hill.climbing(problem,
                                       max_iterations = max_iterations,
                                       count_print = count_print,
                                       trace = trace)
    
    if (is.null(state_final) || result$state_final$evaluation <= state_final$evaluation) {
      state_final <- result$state_final
      report      <- result$report
    }
    
    count <- count + 1
  }
  
  # Get runtime
  end_time <- Sys.time()
  
  result <- list()
  result$name        <- name_method
  result$runtime     <- end_time - start_time
  result$state_final <- state_final
  result$report      <- report
  
  return(result)
}
