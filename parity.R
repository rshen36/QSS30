parity <- function(n) {
  
  B <- matrix(0,n,n)
  V <- matrix(0,n,1)
  W <- matrix(0,n,1)
  
  for (i in 1:n) {
    
    V[i] <- 1
    W[i] <- (-1) ** i
    
    for (j in 1:i) {
      B[i,j] <- choose(i-1,j-1)
    }
  }
  
  print(B)
  S <- matrix(0,n,n)
  S <- B %% 2
  print(S)
  output <- list(B,S,V,W)
  return(output)
  
}

