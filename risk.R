makeDSECov <- function(rsk, secid, ignoreLSR=FALSE) {
  ## -----------------------------------------------------------------------
  ## USAGE: Function to build factor exposure matrix and specific risk vector
  ##        for the given security list.
  ## INPUT: rsk = risk model data, a list containing
  ##           (1) RSK: data matrix containing factor loadings, 
  ##                    along with pbeta, total_risk, specific_risk (in annual % units)
  ##           (2) COV: factor variance / covariance matrix (in annual % units)
  ##        secid = vector of security identifiers
  ##        
  ## OUTPUT: List containing specific risk matrix, covariance, exposure matrix
  ##         and vector of predicted betas.
  ##
  ## NOTE: Expects non-duplicate entries in input ID vector
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
  ## -----------------------------------------------------------------------
  ## USAGE: Generate full asset covariance matrix
  ## INPUT: DSE object containing specific risk matrix, covariance matrix,
  ##        factor loadings, predicted betas and linked specific risk
  ## OUTPUT: List with asset level covariance matrix for each factor group
  ## -----------------------------------------------------------------------
  
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
  
riskDecompCalc <- function(WEIGHT, DSE, method='Northfield') {
  ## -----------------------------------------------------------------------
  ## USAGE: Variance decomposition for a portfolio
  ## INPUT: WEIGHT = vector of portfolio weights
  ##        DSE    = DSE risk model object
  ##        method = method for allocation of covariances between factors
  ##           Barra: don't allocate any covariance to factors
  ##           Northfield: allocate 50% of covariance to each factor pair
  ## OUTPUT: List with total variance for each factor group
  ## -----------------------------------------------------------------------
  FCF <- buildQMatrix(DSE)
  
  fEXP <- t(WEIGHT) %*% DSE$EXP
  
  if (method=='Barra') {
    # variance = W' * (X*COV*X' + SS) * W  ("Barra")
    tot_var <- as.matrix(t(WEIGHT) %*% FCF$Q %*% WEIGHT)
    mkt_var <- t(WEIGHT) %*% FCF$MARKET %*% WEIGHT
    factor_var <- t(WEIGHT) %*% FCF$FACTOR %*% WEIGHT
    style_var <- t(WEIGHT) %*% FCF$STYLE %*% WEIGHT
    industry_var <- t(WEIGHT) %*% FCF$INDUSTRY %*% WEIGHT
    country_var <- t(WEIGHT) %*% FCF$COUNTRY %*% WEIGHT
    currency_var <- t(WEIGHT) %*% FCF$CURRENCY %*% WEIGHT
    idio_var <- t(WEIGHT) %*% DSE$D %*% WEIGHT
  } else {
    # variance = W'X * (COV*X'*W) + W'*SS*W  ("Northfield")
    VAR <- DSE$S %*% t(DSE$EXP) %*% WEIGHT
    SS <- t(WEIGHT) %*% DSE$D %*% WEIGHT
    
    fmap <- getFactorGroups(DSE)
    attach(fmap)
    factor_var <- fEXP %*% VAR
    tot_var <- as.matrix(factor_var + SS)
    mkt_var <- t(fEXP[MARKET]) %*% VAR[MARKET]
    style_var <- t(fEXP[STYLE]) %*% VAR[STYLE]
    industry_var <- t(fEXP[INDUSTRY]) %*% VAR[INDUSTRY]
    country_var <- t(fEXP[COUNTRY]) %*% VAR[COUNTRY]
    currency_var <- t(fEXP[CURRENCY]) %*% VAR[CURRENCY]
    idio_var <- SS
    detach(fmap)
  }
  
  data.frame(total=tot_var[1,1], 
                   factor=factor_var[1,1], 
                   market=mkt_var[1,1], 
                   style=style_var[1,1], 
                   industry=industry_var[1,1],
                   country=country_var[1,1],
                   currency=currency_var[1,1],
                   specific=idio_var[1,1])
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
