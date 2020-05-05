#' @title Freds_MarkovChain_Steadystate
#'
#' @param initial_state       1 X n matrix indicating initial position
#' @param transition_matrix   n X n matrix 
#' @param tolerance           error bound to stop iterations, currently set to 
#'                              10 ^ -3 
#' @param max_iterations      maximum number of iterations to search for steady
#'                              state, currently set to 15
#'
#' @return A steady state if one exists within constraints
#'
Freds_MarkovChain_SteadyState <- function(initial_state, transition_matrix, 
                                         tolerance = -3,
                                    max_iterations = 15) {

  ##### Loop through the max iterations minus one ####
  for (iteration in 1:(max_iterations-1)) {
    
    ##### Difference between the states ####
    previous_state <- Freds_MarkovChain(initial_state, transition_matrix, iteration)
    
    current_state <- Freds_MarkovChain(initial_state, transition_matrix, iteration+1) 
    
    # Create boolean vector
    diff <- abs(current_state - previous_state) < 10 ^ tolerance
    
    ##### Stop Condiditions ####
    # If Steady State reached, return it 
    if (sum(diff) == length(diff)) {
      return(current_state)
      
      # If we haven't reached the Steady State by the max iterations, 
      # throw error saying so
    } else if (iteration == max_iterations-1) {
      stop("Steady State not reached in ", max_iterations, " iterations.")
    }
  }
  
}