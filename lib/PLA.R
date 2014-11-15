PLA = function(DF, y, max.iter = 10000){
  X = as.matrix(cbind(1, DF[,])
  w = c(0,0,0)
  counter = 0
  # Iterate to update the weights until converge.
  N=dim(X)[1]
  while(sum(sign(X %*% w) == y) != N) {
    idx = sample(which(sign(X %*% w) != y),1)
    w = w + y[idx] * X[idx,]
    counter = counter + 1
    # Break the loop if it fails to converge.
    if(counter >= max.iter) break()
  }
  # Return the weights as well as the number of iterations.
  return(list(w = w, counter = counter))
}
