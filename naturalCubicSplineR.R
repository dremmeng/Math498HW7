naturalCubicSplineR<- function( sKnots){
  PhiKnots<- naturalSplineBasis( sKnots,sKnots,
                             degree=3,
                             derivative=2)
  K<- length( sKnots)
  Y<- PhiKnots[1:(K-1),]
  DELTA<- (PhiKnots[2:K,] - PhiKnots[1:(K-1),])
  h<- diff( sKnots)
  
  R<- (t(Y)%*%(h*Y) + 
         (t(DELTA)%*%(h*Y) + t(Y)%*%(h*DELTA))/2 +
         t(DELTA)%*%(h*DELTA)/3)
  #sGrid<- seq( min(sKnots), max( sKnots),1e-4)
  #look2<- naturalSplineBasis( sGrid,sKnots,
  #                            degree=3,
  #                           derivative=2)
  #RTest<- (t(look2)%*%look2)*(sGrid[2]- sGrid[1])
  #return(list( R= R, RTest= RTest))
  return( R)
  
}