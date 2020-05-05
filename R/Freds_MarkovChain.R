#' @title Freds_MarkovChain
#'
#' @param intial_state 1 X n matrix
#' @param transition_matrix a square n X n matrix 
#' @param iterations 
#'
#' @return 1 X n matrix indicating the current state
#'
#' @examples
Freds_MarkovChain <- function(initial_state, transition_matrix, iterations){
  
  ##### Need expm Package ####
  # For the matrix exponential operator %^%
  library(expm)
  
  ##### Matrix Exponentiate Transition Matrix ####
  transition_matrix <- transition_matrix %^% iterations
    
  ##### Do Math ####
  # Current state = initial state * transition matrix ^ iteration
  current_state <- initial_state %*% transition_matrix
  
  ##### Return state after iterations #### 
  return(current_state)
}