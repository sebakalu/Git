#Function to find runs of a certain value and return the highest consecutive run in each row of a dataframe, returns Inf if that value is not in the row
run_count <- function(df, value){
  result_list <- list()
  for(i in 1:nrow(df)){
    temp <- rle(as.character(df[i,]))
    result_list[[length(result_list)+1]] <- suppressWarnings(max(temp$lengths[temp$values == as.character(list(value))]))
  }
  return(as.data.frame(do.call(rbind, result_list)))
  
}