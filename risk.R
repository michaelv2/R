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
