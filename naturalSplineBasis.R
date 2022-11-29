naturalSplineBasis <- function(sGrid,
                               sKnots,
                               degree = 3,
                               derivative = 0) {
  boundaryKnots<- c( min(sKnots),max(sKnots))
  sKnots0<- c( rep( boundaryKnots[1],degree),sort(sKnots),
               rep( boundaryKnots[2],degree) )
  testRight<- sGrid < min(sKnots) 
  testLeft <- sGrid > max(sKnots)             
  if( any(testRight |testLeft) )
  {stop("some points for evaluation outside knot range.")}
               
  basis <- splineDesign(sKnots0, sGrid,
                        ord= degree+1, outer.ok=TRUE,
                        derivs=derivative)
  # set up constraints to enforce natural BCs.
  const <- splineDesign(sKnots0, boundaryKnots, ord = degree+1,
                        derivs = c(2,2)) 
  qr.const <- qr(t(const))
  QBasis<- t(qr.qty( qr.const, t(basis) ))
  basis <- QBasis[,-(1:2)]
  basis
  
  return( basis )
  
}