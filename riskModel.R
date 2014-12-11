makeDSECov <- function(rsk, assets) {
  if (any(duplicated(assets))) stop('Duplicate security IDs found.')
  
  r_ref <- match(assets, row.names(rsk$RSK))
  
  BETA <- rsk$RSK[r_ref,"Beta"]
  SRISK <- rsk$RSK[r_ref,"SRISK%"]^2
  model.name <- 
  factor.names <- 
  EXP <- rsk$RSK[r_ref,factor.names]
  
  BETA[is.na(BETA)] <- 0
  SRISK[is.na(SRISK)] <- 0
  EXP[is.na(EXP)] <- 0

  if (length(assets) == 1) {
    names(BETA) <- assets
    names(SRISK) <- assets
    EXP <- v2rm(EXP)
    rownames(EXP) <- assets
  }
    
  stopifnot(all(dimnames(EXP)[[2]] == dimnames(rsk$COV)[[2]]))  # verify factor alignment
  
  return(list(D=SRISK, S=rsk$COV, EXP=EXP, BETA=BETA))
}

buildQMatrix <- function(DSE) {
  # Generate full asset covariance matrix
  
  model <- 
  groups <- 
  map <- 

  attach(map)
  
  FCF <- list()
  FCF$FACTOR <- with(DSE, EXP %*% S %*% t(EXP))
  if (length(DSE$D)==1) {    
    FCF$STYLE <- with(DSE, v2rm(EXP[,STYLE]) %*% S[STYLE,STYLE] %*% v2cm(EXP[,STYLE]))
    FCF$INDUSTRY <- with(DSE, v2rm(EXP[,INDUSTRY]) %*% S[INDUSTRY,INDUSTRY] %*% v2cm(EXP[,INDUSTRY]))
    FCF$Q <- with(DSE, EXP %*% S %*% t(EXP) + D)
  } else {
    FCF$STYLE <- with(DSE, EXP[,STYLE] %*% S[STYLE,STYLE] %*% t(EXP[,STYLE]))
    FCF$INDUSTRY <- with(DSE, EXP[,INDUSTRY] %*% S[INDUSTRY,INDUSTRY] %*% t(EXP[,INDUSTRY]))
    FCF$Q <- with(DSE, EXP %*% S %*% t(EXP) + diag(D))
  }
  
  detach(map)

  return(FCF)
}

tsRiskDecomp <- function(hld, rsk, model) {
  # Change to multiPortRiskDecomp
  
  group <- dcast(hld, assets ~ Timestamp, value.var='SEMV', fun.aggregate=sum)
  group <- subset(group, assets %in% rownames(rsk$RSK))
  group <- dfNanToZero(group)
  
  DSE <- makeDSECov(rsk, group$assets)
  FCF <- buildQMatrix(DSE)
  
  SEMV <- as.matrix(group[,-1])
  
  total <- diag(t(SEMV) %*% FCF$Q %*% SEMV)^.5 / 100
  style <- diag(t(SEMV) %*% FCF$STYLE %*% SEMV)^.5 / 100
  
  data.frame(TotalVol=total,StyleVol=style)
}
