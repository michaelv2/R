makeDSECov <- function(rsk, assets) {
  ## -----------------------------------------------------------------------
  ## USAGE: Function to build factor exposure matrix and specific risk vector
  ##        for the supplied risk model.
  ## INPUT: Risk model data, risk model name, security ID vector.
  ## OUTPUT: List containing specific risk vector, exposure matrix, and
  ##         vector of predicted betas.
  ## 
  ## NOTE: Expects non-duplicate entries in input ID vector
  ## -----------------------------------------------------------------------
  
  if (any(duplicated(assets))) stop('Duplicate security IDs found.')
  
  r_ref <- match(assets, row.names(rsk$RSK))
  
  BETA <- rsk$RSK[r_ref,"Beta"]
  SRISK <- rsk$RSK[r_ref,"SRISK%"]^2
  model.name <- unique(sapply(strsplit(dimnames(rsk$COV)[[1]], '_'), '[', 1))
  factor.names <- dimnames(rsk$RSK)[[2]][grepl(paste(model.name,"_",sep=""),dimnames(rsk$RSK)[[2]])]
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
  
  model <- unique(sapply(strsplit(dimnames(DSE$EXP)[[2]],'_'), '[', 1))
  groups <- getFactorGroups(model)
  map <- getFactorMapping(groups, DSE)

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
