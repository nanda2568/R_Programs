corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  
  (fileCnt <- data.frame(id = 0, nobs = 0))
  (fileCnt <- complete(directory, 1:332))
  sulfateV <- numeric()
  nitrateV <- numeric()
  #nobsCol <- as.vector(fileCnt["nobs"])
  #idCol <- as.vector(fileCnt["id"])
  #print(fileCnt$nobs[2])
  for(i in 1:fileCnt$nobs){
    if (fileCnt$nobs[i] > threshold){
      #print(fileCnt$nobs[i])
      fiName <- formatC(fileCnt$id[i], digits = 3, width = 3, flag = "0")
      fileName <- paste(fiName, "csv", sep=".")
      fullFileName <- paste(directory, fileName, sep="/")
      pollData <- read.csv(fullFileName, header = TRUE)[, c("sulfate", "nitrate")]
      # Loop through the pollutant column and count all non-NA values
      for(j in 1: nrow(pollData)){
        if ((is.na(pollData$sulfate[j]) == FALSE) && (is.na(pollData$nitrate[j]) == FALSE)){        
          sulfateV <- c(sulfateV, pollData$sulfate[j])
          nitrateV <- c(nitrateV, pollData$nitrate[j])
        }
      }
      
    }
  }
  cor(sulfateV,nitrateV)
  # suppresses row.names and quotes all entries
  
  ## Usage and example results:
  ## source("corr.R")
  ## source("complete.R")
  ## cr <- corr("specdata", 150)
  ## head(cr)
  
  ## [1] -0.01896 -0.14051 -0.04390 -0.06816 -0.12351 -0.07589
  
  ## summary(cr)
  
  ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  ## -0.2110 -0.0500  0.0946  0.1250  0.2680  0.7630
  
  ## cr <- corr("specdata", 400)
  ## head(cr)
  
  ## [1] -0.01896 -0.04390 -0.06816 -0.07589  0.76313 -0.15783
  
  ## summary(cr)
  
  ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  ## -0.1760 -0.0311  0.1000  0.1400  0.2680  0.7630
  
  ## cr <- corr("specdata", 5000)
  ## summary(cr)
  
  ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  ## 
  
  ## length(cr)
  
  ## [1] 0
  
  ## cr <- corr("specdata")
  ## summary(cr)
  
  ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  ## -1.0000 -0.0528  0.1070  0.1370  0.2780  1.0000
  
  ## length(cr)
  
  ## [1] 323
  
}