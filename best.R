# this is a fumction find the hospital with the best outcomes in the state
# function takes two arguments
#   state -  two charcater abreviation for the state where hospitals are being ranked
#   outcome - nome of the outcom under consideration (i.e. "heart attack", "heart failure" ect.)
# function returns the name of the hospital that has the best 30 day mortality or the specified outcome
#   return value is a character vector
# hospitals that hevr no data for the outcome are excluded from the analysis
# Ties are broken by aphabetical listing for hospital names.

best <- function(state, outcome) {
  ## Read the outcome data
  data_file <- "outcome-of-care-measures.csv"
  outcome_data <- read.csv(data_file)
  #print("Past Loading Data")
  ## Check to make sure the state and outcome provided to the function are valid
  # get outcome data in the same format as the rows
  # split the outcome into tow words and capitalize the first letter of each word\\
  capFirst <- function (x) {
    result <- paste(toupper(substr(x, start=1, stop=1)), substr(x, start=2, stop=nchar(x)), sep="")
    result
  }

  temp_name <- unlist(strsplit(outcome, " ", fixed = TRUE))
  if (length(temp_name)==2) {
    temp_name[1] <- capFirst(temp_name[1])
    temp_name[2] <- capFirst(temp_name[2])
    #assemble an approriaite name
    outcome_col_name <- paste(temp_name[1],temp_name[2], sep=".")
  } else if (length(temp_name)==1) {
    outcome_col_name <- capFirst(temp_name)
  }

  #print(state)


  #print(outcome_col_name)
  #generating a list of all states that exist in the data
  all_states <- levels(outcome_data[,"State"])
  all_cols <- names(outcome_data)
  #print(all_cols)
  a <- grepl(outcome_col_name, all_cols)
  outcome_cols <- all_cols[a]
  #print(outcome_cols)
  #print("****************************************************")
  full_outcome_col_name <- paste("Hospital.30.Day.Death..Mortality..Rates.from",outcome_col_name, sep = ".")
  print(full_outcome_col_name)
  #b <- grepl("Hospital.30.Day.Death..Mortality.Rates.from", outcome_cols)
  #print("****************************************************")
  #print(b)
  #print(outcome_cols[b])
  #print("****************************************************")

  # Optional Check to make sure the input is two capital letters
  #Check to make sure the input can be found in the list
  if (match(state,all_states,nomatch = -1)==-1) {
    stop("invalid state")
  } else if (length(grep(outcome_col_name, all_cols))==0) {
    stop("invalid outcome")
  }

  mortality <- "Hospital.30.Day.Death..Mortality..Rates.from."
  mortality_col_name <- paste(mortality, outcome_col_name, sep="")
  #print(mortality_col_name)

  ## return the hospital name with the best 30 day death rate for the outcome
  #subset the input data by state
  print(state)
  state_data <- outcome_data[outcome_data$State==state, c(mortality_col_name, "Hospital.Name")]
  #print(state_data[,1])
  #state_data[,1] <- as.numeric(state_data[,1])
  #print(state_data[,1])
  result <- state_data[order(state_data[,1],state_data[,2]),c(1,2)]

  #toString(result[1])

  result
}


