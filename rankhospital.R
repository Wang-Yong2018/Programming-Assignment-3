rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## note: the code validates the state and outcome input at begining
  
  # Check that if state are valid
  
  if (!any(df$State == state)) {
    stop("invalid state")
  }
  
  # Check if the outcome is valid
  
  ## Predefine the related colname
  
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
  na_string = 'Not Available'
  df <- read.csv("data\\outcome-of-care-measures.csv",na.strings=na_string)
  
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
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
  
  # Pre-define best_hospital as NA. 
  best_hospital <- NA
  
  ## create the rank_vector
  rank_vec <- rank(interim_df[[outcome]],na.last=FALSE, ties.method = "first")
  
  
  if ( num  == 'best') {
    best_hospital <- interim_df[rank_vec[rank_vec==min(rank_vec)],'Hospital.Name']
    
  }
  
  if (num == 'worst') {
    best_hospital<- interim_df[rank_vec[rank_vec==max(rank_vec)],'Hospital.Name']
  }
  
  if (num %in% rank_vec ) {
    best_hospital<- interim_df[rank_vec[rank_vec==num],'Hospital.Name']
  } 
  
  best_hospital
  
}


# testing for rankhospital 
# print(rankhospital("TX", "heart failure", 4))
# print(rankhospital("MD", "heart attack", "worst"))
# print(rankhospital("MN", "heart attack", 5000))