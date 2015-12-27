## Function to calculate mean of air pollutants:
## pollutantmean <- function(directory, pollutant, id = 1:332) {

## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'pollutant' is a character vector of length 1 indicating
## the name of the pollutant for which we will calculate the
## mean; either "sulfate" or "nitrate".

## 'id' is an integer vector indicating the monitor ID numbers
## to be used

## Return the mean of the pollutant across all monitors list
## in the 'id' vector (ignoring NA values)
## NOTE: Do not round the result!
## NOTE: Accompanying input csv files are in specdata.zip

## Usage and examples:
## source("pollutantmean.R")
## pollutantmean("specdata", "sulfate", 1:10)
## [1] 4.064
## pollutantmean("specdata", "nitrate", 70:72)
## [1] 1.706
## pollutantmean("specdata", "nitrate", 23)
## [1] 1.281

pollutantmean <- function(directory, pollutant, id = 1:332){

  meanValue <- 0                  # variable to hold the mean of pollutants
  count <- 0                      # used in calculating mean
  totPollutant <- 0               # used in calculating mean

  for (i in seq_along(id)){       # for all monitor ids (csv files) passed as an argument
    fiName <- formatC(id[i], digits = 3, width = 3, flag = "0")
    fileName <- paste(fiName, "csv", sep=".")
    fullFileName <- paste(directory, fileName, sep="/")
    pollutantData <- read.csv(fullFileName)[,pollutant]
    
    # Loop through the pollutant column and add up all non-NA values
    for(j in seq_along(pollutantData)){
      if (is.na(pollutantData[j]) == FALSE){
         count <- count + 1
         totPollutant <- totPollutant + as.numeric(pollutantData[j])
      }
    }
  }
  
  meanValue <- totPollutant / count   # calculate mean from total and count of recorded values
  return(meanValue)                   # return mean to display on the console
}