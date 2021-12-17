
# find best hospital

best <- function(state, outcome) {
  ## note: the code validates the state and outcome input at begining
  
  # Check that if state are valid
  
  if (!any(df$State == state)) {
    stop("invalid state")
  }
  
  # Check if the outcome is valid
  
  ## predefine the related colname
  
  outcome_col_names = c(
    "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
    "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
    "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  )
  
  ## Using swtich to translate the outcome to specific column name
  ## for those not in the switch cases, it is invalid.
  outcome <- switch(EXPR=outcome,
                    "heart attack" = outcome_col_names[1], 
                    "heart failure"= outcome_col_names[2], 
                    "pneumonia" = outcome_col_names[3],
                    NA
                    )
  
  if ( is.na(outcome )) {
       stop("invalid outcome")
  }
  
  ## Read outcome data from csv and load as data.frame
  
  df <- read.csv("data\\outcome-of-care-measures.csv")
  
  ## select the rows with state name and desired outcome type
  interim_df <- df[df$State==state,c('State',"Hospital.Name",outcome)]
  
  # convert the outcome result as numeric and drop NA
  interim_df[,outcome]<- as.numeric(interim_df[,outcome])
  interim_df<- interim_df[!is.na(interim_df[outcome]),]
  
  # defined the sort order
  sorted_cols_name <- c(outcome, "Hospital.Name")
  
  # sort
  # best means lowest number.
  
  interim_df <- interim_df[ do.call("order", interim_df[sorted_cols_name]),]
  
  # return the best hospital
  interim_df[1,"Hospital.Name"]
}

## testing
print(" Test best function 1 pass =: ",best("TX", "heart attack") ==  "CYPRESS FAIRBANKS MEDICAL CENTER" )

best("TX", "heart failure") ==  "FORT DUNCAN MEDICAL CENTER"

best("MD", "heart attack") ==  "JOHNS HOPKINS HOSPITAL, THE"