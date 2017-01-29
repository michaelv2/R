makeDSECov <- function(rsk, secid, ignoreLSR=FALSE) {
  ## -----------------------------------------------------------------------
  ## USAGE: Function to build factor exposure matrix and specific risk vector
  ##        for the given security list.
  ## INPUT: Risk model data from loadRM(), security ID vector.
  ## OUTPUT: List containing specific risk matrix, covariance, exposure matrix
  ##         and vector of predicted betas.
  ##
  ## NOTE: Expects non-duplicate entries in input ID vector
  ##
  ## -----------------------------------------------------------------------
  library(Matrix)

  if (any(duplicated(secid))) stop('Duplicate security IDs found.')
  
  r_ref <- match(secid, row.names(rsk$RSK))
  factors <- dimnames(rsk$COV)[[1]]
  stopifnot(all(factors %in% colnames(rsk$RSK)))

  BETA <- rsk$RSK[r_ref,"pbeta"]
  SRISK <- rsk$RSK[r_ref,"specific_risk"]^2
  EXP <- rsk$RSK[r_ref,factors]
  LSR <- rsk$LSR[r_ref,]

  BETA[is.na(BETA)] <- 0
  SRISK[is.na(SRISK)] <- 0
  EXP[is.na(EXP)] <- 0
  
  if (length(secid) == 1) {
    names(BETA) <- secid
    names(SRISK) <- secid
    EXP <- v2rm(EXP)
    rownames(EXP) <- secid
  } else {
    SRISK <- Matrix(diag(SRISK), sparse=TRUE)
    rownames(SRISK) <- colnames(SRISK) <- secid
    if (!ignoreLSR) {
      # Update linked assets
      LSR$root_risk <- LSR$root_risk^2
      LSR$term <- LSR$root_risk * LSR$elasticity
      LSR$secid <- rownames(LSR)
      LSR <- LSR[!is.na(LSR$elasticity) & LSR$root_id != LSR$secid & LSR$root_id %in% secid,]
      if (nrow(LSR)>0) {
        cat(paste(nrow(LSR),'rows in LSR\n'))
        for (r in 1:nrow(LSR)) {
          # Fill off-diagonal terms
          # NOTE: doesn't handle cross-linked names (e.g. two names with same root)
          i1 <- match(LSR$secid[r], rownames(SRISK))
          i2 <- match(LSR$root_id[r], colnames(SRISK))
          SRISK[i1,i2] <- SRISK[i2,i1] <- LSR$term[r]
        }
      }
    }
  }
  
  stopifnot(all(colnames(EXP) == dimnames(rsk$COV)[[2]]))  # verify factor alignment
  
  dse <- list(D=SRISK, S=rsk$COV, EXP=EXP, BETA=BETA, LSR=LSR)
  attr(dse, 'model') <- attr(rsk, 'model')
  return(dse)
}

buildQMatrix <- function(DSE) {
  # Generate full asset covariance matrix
  
  f_groups <- getFactorGroups(DSE)
  attach(f_groups)
  
  FCF <- list()
  FCF$FACTOR <- with(DSE, EXP %*% S %*% t(EXP))
  if (length(DSE$BETA)==1) {
    FCF$MARKET <- with(DSE, v2rm(EXP[,MARKET]) %*% S[MARKET,MARKET,drop=F] %*% v2cm(EXP[,MARKET]))
    FCF$STYLE <- with(DSE, v2rm(EXP[,STYLE]) %*% S[STYLE,STYLE] %*% v2cm(EXP[,STYLE]))
    FCF$INDUSTRY <- with(DSE, v2rm(EXP[,INDUSTRY]) %*% S[INDUSTRY,INDUSTRY] %*% v2cm(EXP[,INDUSTRY]))
    FCF$COUNTRY <- with(DSE, v2rm(EXP[,COUNTRY]) %*% S[COUNTRY,COUNTRY] %*% v2cm(EXP[,COUNTRY]))
    FCF$CURRENCY <- with(DSE, v2rm(EXP[,CURRENCY]) %*% S[CURRENCY,CURRENCY] %*% v2cm(EXP[,CURRENCY]))
    FCF$Q <- with(DSE, EXP %*% S %*% t(EXP) + D)
  } else {
    FCF$MARKET <- with(DSE, EXP[,MARKET,drop=F] %*% S[MARKET,MARKET,drop=F] %*% t(EXP[,MARKET,drop=F]))
    FCF$STYLE <- with(DSE, EXP[,STYLE] %*% S[STYLE,STYLE] %*% t(EXP[,STYLE]))
    FCF$INDUSTRY <- with(DSE, EXP[,INDUSTRY] %*% S[INDUSTRY,INDUSTRY] %*% t(EXP[,INDUSTRY]))
    FCF$COUNTRY <- with(DSE, EXP[,COUNTRY] %*% S[COUNTRY,COUNTRY] %*% t(EXP[,COUNTRY]))
    FCF$CURRENCY <- with(DSE, EXP[,CURRENCY] %*% S[CURRENCY,CURRENCY] %*% t(EXP[,CURRENCY]))
    FCF$Q <- with(DSE, EXP %*% S %*% t(EXP) + D)
  }
  
  detach(f_groups)
  
  return(FCF)
}

tsRiskDecomp <- function(hld, rsk, model) {
  # Change to multiPortRiskDecomp
  
  group <- dcast(hld, assets ~ Timestamp, value.var='Value', fun.aggregate=sum)
  group <- subset(group, assets %in% rownames(rsk$RSK))
  group <- dfNanToZero(group)
  
  DSE <- makeDSECov(rsk, group$assets)
  FCF <- buildQMatrix(DSE)
  
  Value <- as.matrix(group[,-1])
  
  total <- diag(t(Value) %*% FCF$Q %*% Value)^.5 / 100
  style <- diag(t(Value) %*% FCF$STYLE %*% Value)^.5 / 100
  
  data.frame(TotalVol=total,StyleVol=style)
}
