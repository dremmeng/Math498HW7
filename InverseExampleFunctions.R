fitWBS<- function( z, W, nKnots,sGrid){
  sKnots<- seq( min(sGrid),max( sGrid), length.out=nKnots)
  B<- naturalSplineBasis(sGrid,sKnots)
  X<- W%*%B
  fit<- lm(z~ X -1)
  fHat<- B%*%fit$coefficients  
  return( fHat)
}

fitWSS<- function( z, W, lambda,sGrid){
  
  BigB<- naturalSplineBasis(sGrid,sGrid)
  R<- naturalCubicSplineR(sGrid)
  X2<- W%*%BigB
  cHat<- solve(t(X2)%*%X2 + lambda*R)%*%t(X2)%*%z
  # effective degrees of freedom
  #ALambda <- X2%*%solve(t(X2)%*%X2 + lambda*R)%*%t(X2) 
  # effdf <-  sum(diag( ALambda)) 
  fHat<- BigB%*%cHat
  return( fHat)
}