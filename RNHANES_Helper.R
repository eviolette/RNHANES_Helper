
source("check_packages.R")
# Usage example
packages<-c("NHANES", "RNHANES", "dplyr", "tidyverse")
check.packages(packages)

files <- nhanes_data_files()
variables <- nhanes_variables()

remove.outliers.df <- function(df, colrange) {
  for (i in colrange) {
    qnt <- quantile(df[,i], probs=c(.25, .75), na.rm = T)
    H <- 1.5 * IQR(df[,i], na.rm = T)
    df <- df[df[,i] >= (qnt[1] - H),]
    df <- df[df[,i] <= (qnt[2] + H),]
  }
  return (df)
}

nhanes.merg.data <- function(var.list, 
                             period = "all", 
                             name.list = var.list, 
                             na.rm = FALSE, 
                             rm.outliers.list = rep(FALSE, length(var.list))) {
  
  # Retrieve list of all possible period configurations
  
  all.periods <- unique(nhanes_search(variables, "")$cycle)
  
  if (!(period %in% all.periods) & period != "all") stop("Invalid time period specified")
  
  period.list <- list()
  
  if (period != "all") {
    period.list[1] <- period
  } else {
    temp.list <- list()
    for (i in 1:length(var.list)) {
      period.list[[i]] <- unique(nhanes_search(variables, var.list[i])$cycle)
    }
    period.list <- Reduce(intersect, period.list)
  }
  
  
  if (length(name.list) != length(var.list)) stop("Length of names and variables do not match")
  if (length(rm.outliers.list) != length(var.list)) stop("Length of outlier removal list and variables list do not match")
  
  final.df <- NULL
  
  if (length(period.list) == 0) stop("No period found that contains all variables")
  
  
  for (i in 1:length(period.list)) {
    
    period <- period.list[i]
    
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
          stop(paste("No data file found for variable", var.list[i]))
        }
        tryCatch({
          df.list[[i]] <- (rbind(df.list[i], nhanes_load_data(file.list[[i]][j], year = period))) %>%
            select(var.list[i], "SEQN")
          if (rm.outliers.list[i]) {
            df.list[[i]] <- remove.outliers.df(df.list[[i]], c(1,2))
          }
        }, error=function(e){
          print(paste("Problem loading data file", file.list[[i]][j])) # throws error if a data file isn't found
        })
      }
    }
    return (df.list)
    
    merg <- reduce(df.list, full_join, by = "SEQN");
    
    
    if (na.rm) merg <- na.omit(merg)
    
    ID <- merg$SEQN
    
    merg$SEQN <- NULL
    
    colnames(merg) <- name.list
    
    merg <- cbind(merg, ID, period)
    
    final.df <- rbind(final.df, merg)
  }
  
  return(final.df)
  
  
}
