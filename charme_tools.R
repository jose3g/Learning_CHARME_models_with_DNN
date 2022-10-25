sample.model.architecture <- function(units.vector, delta){
  L <-length(units.vector)
  Parameters <- list()
  for (l in 1:(L-1)){
    A <- matrix(runif(units.vector[l+1]*units.vector[l], min = -delta, max = delta), nrow = units.vector[l+1])
    b <- matrix(rnorm(units.vector[l+1]), ncol = 1) 
    Z <- cbind(A,b)
    Parameters[[l]] <- Z
  }
  W1 <- Parameters[[1]][,-ncol(Parameters[[1]])]
  for (l in 2:(L-1)){
    W1 <- Parameters[[l]][,-ncol(Parameters[[l]])]%*%W1
  }
  W2 <- norm(Parameters[[2]][,-ncol(Parameters[[2]])], type = 'f')
  for (l in 3:(L-1)){
    W2 <- W2*norm(Parameters[[l]][, -ncol(Parameters[[l]])], type = '2')
  }
  W2 <- W2*sum(apply(Parameters[[1]][,-ncol(Parameters[[1]])], 2, function(x){sqrt(sum(x^2))}))
  return(list(Parameters, W2)) 
}

# Here, units.vector = c($N_0$, $N_1$, ..., $N_L$), where $N_0=$dimension_input, 
# $N_L=$dimension_output and  delta is such that the rang of the values of W are in [-delta, delta].

# Example:
Architecture <- sample.model.architecture(c(5,8,10,10,1), 0.3)
Architecture[[2]]

#-------------------------------------------------------------------------------

model_function <- function(weights_biases, activation.function, x0){
  x <- matrix(c(x0,1), ncol=1)
  L <- length(weights_biases)
  for (l in 1:(L-1)){
    x <- activation.function(weights_biases[[l]]%*%x)
    x <- rbind(x,1)
  }
  x <- weights_biases[[L]]%*%x
  return(x)
}

#-------------------------------------------------------------------------------

Switching <- function(n, prob){
  if (sum(prob)<1) stop("the second argument is not a measure of probability: the sum of the coordinates must be equal to 1")
  U <- runif(n)
  R <- rep(0, n)
  for (k in 1:(length(prob)-1)){
    R <- R + (k+1)*((U>sum(prob[1:k]))&(U<=sum(prob[1:(k+1)])))  
  }
  R <- R + (U<=prob[1])
  return(R)
}

#-------------------------------------------------------------------------------

#Index_Switching function gives the indexes i such that switching.observations[i]=x.

Index_Switching <- function(switching.observations, x){
  I <- (1:length(switching.observations))[switching.observations==x]
  return(I)
}

#Here, switching.observations is a vector of observations and x is any value 
# within the coordinate range of switching.observations. 

#-------------------------------------------------------------------------------

#T_KG function transforms a list in Keras format to sample.model.architecture format. 

T_KG <- function(list.k){
  L <- length(list.k)
  #print(L)
  list.G <- list()
  if (is.matrix(list.k[[(L-1)]])==T){
    for (l in 1:(L/2)){
      list.G[[l]] <- cbind(t(list.k[[(2*l-1)]]), as.matrix(list.k[[2*l]], ncol=1)) 
    }
  }
  else{
    for (l in 1:(L/2-1)){
      list.G[[l]] <- cbind(t(list.k[[(2*l-1)]]), as.matrix(list.k[[2*l]], ncol=1)) 
    }
    list.G[[(L/2)]] <- matrix(c(t(list.k[[(L-1)]]), list.k[[L]]), nrow = 1)
  }
  return(list.G)
}

#Here, list.k is the list in Keras format. 