iterative.deepening.search = function(problem,
                                      max_iterations = 1000, 
                                      count_print = 100, 
                                      trace = FALSE, 
                                      graph_search = FALSE,
                                      max_depth = 20) {
  # Get Start time
  start_time       <- Sys.time()
  
  limit <- 0
  
  while (limit <= max_depth) {
    cat(paste0("Iteration: ", limit, " - "))
    
    depth_limit_result <- depth.limited.search(problem,
                                               max_iterations = max_iterations,
                                               count_print = count_print, 
                                               trace = trace, 
                                               graph_search = graph_search,
                                               depth_limit = limit)

    if (all(is.na(depth_limit_result$state_final))) {
      limit <- limit + 1
    } else {
      break
    }
  }
  
  # Get runtime
  end_time <- Sys.time()
  
  result <- list()
  result$name        <- paste0("Iterative Deeping Search - It:", limit, ifelse(graph_search, " + GS", ""))
  result$runtime     <- end_time - start_time
  result$state_final <- depth_limit_result$state_final
  result$report      <- depth_limit_result$report
  
  return(result)
}