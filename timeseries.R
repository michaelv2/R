rle2 <- function(x) {
  # Modified version of base::rle that includes indexes
  if (!is.vector(x) && !is.list(x)) 
    stop("'x' must be an atomic vector")
  n <- length(x)
  if (n == 0L) 
    return(structure(list(lengths = integer(), values = x), 
                     class = "rle"))
  y <- x[-1L] != x[-n]
  i <- c(which(y | is.na(y)), n)
  data.frame(lengths = diff(c(0L, i)), values = x[i], index = i)
}

getRuns <- function(x, decreasing=F) {
  # Function to calculate runs of equal values for every element in a vector
  if (decreasing) {
    x <- rev(x)
  }
  runs <- list()
  done <- FALSE
  while (!done) {
    y <- rle2(x)
    y <- subset(y, values!=0)
    if (nrow(y)>0) {
      runs[[length(runs)+1]] <- y
      x[y[,'index']] <- 0
    } else {
      done <- TRUE
    }
  }
  runs <- do.call(rbind, runs)
  if (decreasing) {
    runs <- runs[order(runs$index, decreasing=T),]
    runs$index <- rev(runs$index)
  }
  runs <- runs[order(runs$index),]
  return(runs)
}

get.bday <- function(sdt, edt, region='US') {
  # Return business days for given date range
  # Expects YYYYMMDD for date strings
  library(timeDate)
  
  if (class(sdt) != 'Date') {
    sdt <- toDate(sdt)
    edt <- toDate(edt)
  }
  dtes <- seq(sdt, edt, by=1)
  years <- as.numeric(unique(strftime(dtes,'%Y')))
  dtes <- as.timeDate(dtes)
  
  if (region=='US') {
    dtes <- dtes[isBizday(dtes, holidays=holidayNYSE(years), wday=1:5)]
  } else if (region=='LN') {
    dtes <- dtes[isBizday(dtes, holidays=holidayLONDON(), wday=1:5)]
  }
  
  return(strftime(dtes, '%Y%m%d'))
}

toDate <- function(datestr) {
  as.Date(as.character(datestr),'%Y%m%d')
}
