best <- function(state, outcome) {
  ## Read outcome data
  
  df <- read.csv("data\\outcome-of-care-measures.csv", colClasses = "character")
  outcome <- tolower(outcome)
  
  ## Check that state and outcome are valid
  if (sum(df$State == state) == 0) {
    stop("invalid state")
  }
  
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop('invalid outcome')
  }  
  ## Return hospital name in that state with lowest 30-day death
  if (outcome == "heart attack"){
    outcomeData = data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
  }
  if (outcome == "heart failure"){
    outcomeData = data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
  }
  if (outcome == "pneumonia"){
    outcomeData = data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
  }
  sdata <- data.frame(data$State, data$Hospital.Name, as.numeric(outcomeData)) #must use as.numeric on `outcomeData` to prevent transferiing this into class `factor`
  z <- subset(sdata, data$State == state)      # Gather rows with the desired state
  ## rate
  
  hname <- c()
  for (i in 1:length(z)){
    zz <- z[i] == min((z[,3]), na.rm = TRUE) #compate the oucome data to the minimum
  }
  
  result <- data.frame(z[,2], zz)
  fresult <- subset(result, result[,2] == TRUE)
  afresult <- fresult[order(fresult$z...2.),] #Alphabetize results data frame
  
  print(afresult[1,1])

}