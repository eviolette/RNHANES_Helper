
# simple function to check packages. Takes list of packages, installs them if necessary, then requires them
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# import required packages
packages <- c("NHANES", "RNHANES", "dplyr", "tidyverse")
check.packages(packages)

# get list of files and variables from RNHANES
files <- nhanes_data_files()
variables <- nhanes_variables()

# simple outlier removal function based on interquartile range
remove.outliers.df <- function(df, colrange) {
  for (i in colrange) {
    qnt <- quantile(df[,i], probs=c(.25, .75), na.rm = T)
    H <- 1.5 * IQR(df[,i], na.rm = T)
    df <- df[df[,i] >= (qnt[1] - H),]
    df <- df[df[,i] <= (qnt[2] + H),]
  }
  return (df)
}

# function: list of variables, time period, list of names for the list of variables, option to remove NAs, 
#           option to remove outliers -> dataframe of variables from NHANES

#           vector of strings, string, vector of strings, boolean, vector of booleans -> dataframe
nhanes.merg.data <- function(var.list, 
                             period = "all", 
                             name.list = var.list, 
                             na.rm = FALSE, 
                             rm.outliers.list = rep(FALSE, length(var.list))) {
  
  # Retrieve list of all possible period configurations
  
  all.periods <- unique(nhanes_search(variables, "")$cycle)
  
  if (!(period %in% all.periods) & period != "all") stop("Invalid time period specified")
  
  period.list <- list()
  
  # fill list of overlapping time periods where all of the variables were recorded
  # i.e. all of the variables were recorded in the 2003-2004 survey, so the period list would include "2003-2004"
  if (period != "all") {
    period.list[1] <- period
  } else {
    temp.list <- list()
    for (i in 1:length(var.list)) {
      period.list[[i]] <- unique(nhanes_search(variables, var.list[i])$cycle)
    }
    period.list <- Reduce(intersect, period.list)
  }
  
  # simple checking
  if (length(name.list) != length(var.list)) stop("Length of names and variables do not match")
  if (length(rm.outliers.list) != length(var.list)) stop("Length of outlier removal list and variables list do not match")
  
  final.df <- NULL
  
  if (length(period.list) == 0) stop("No period found that contains all variables")
  
  
  # iterate through the time periods where all of the variables were recorded
  for (i in 1:length(period.list)) {
    
    period <- period.list[i] # current period we're looking at
    
    # Retrieve list of datasets associated with each of the respective target variables
    
    file.list <- list()
    
    for (i in 1:length(var.list)) {
      file.list[i] <- list(nhanes_search(variables, var.list[i], cycle == period)$data_file_name)
    }
    
    
    # Retrieve list of data frames from the files found above
    
    df.list <- list()
    
    for (i in 1:length(file.list)) {
      df.list[i] <- NULL
      for (j in 1:length(file.list[[i]])) {
        if (length(file.list[[i]][j]) == 0) {
          stop(paste("No data file found for variable", var.list[i])) # error: no file found for the variable
        }
        tryCatch({
          df.list[[i]] <- (rbind(df.list[i], nhanes_load_data(file.list[[i]][j], year = period))) %>%
            select(var.list[i], "SEQN") # load the dataframe from the file, select the target variable and patient ID (SEQN)
          if (rm.outliers.list[i]) {
            df.list[[i]] <- remove.outliers.df(df.list[[i]], c(1)) # if removal of outliers specified for this var, do so
          }
        }, error=function(e){
          print(paste("Problem loading data file", file.list[[i]][j])) # skips if a data file isn't loaded properly
        })
      }
    }
    
    merg <- NULL
    
    if (length(df.list) != 0) {
      merg <- reduce(df.list, full_join, by = "SEQN"); # reduce the list of dataframes into a single dataframe, by ID (SEQN)
      
      
      if (na.rm) merg <- na.omit(merg) # if removal of NAs specified, do so
      
      ID <- merg$SEQN
      
      merg$SEQN <- NULL
      
      colnames(merg) <- name.list # rename columns based on function input
      
      merg <- cbind(merg, ID, period)
    }
    
    
    
    final.df <- rbind(final.df, merg) # combine this final dataframe with the previous time period(s)' final dataframe.
  }
  
  return(final.df) # return the final single dataframe
  
  
}
