find_matching_str <- function(str_to_check, database, method = "osa", no_cores = 2, db = "nmnh"){
  
  #remove these chars
  this_str <- gsub("[?!*]", "", as.character(str_to_check))
  
  if (this_str == "" || this_str == "-" || this_str == "NA" || is.na(this_str)){
    cat("Empty string, returning NAs.")
    results <- as.data.frame(cbind(NA, NA))
    names(results) <- c("match", "score")
    return(results)
  }else{
    
    if (method == "jw"){
      #Jaro-Winkler distance
      str_matches <- stringdist::stringdist(this_str, database[,1], nthread = no_cores, method = method, p = 0.1)
    }else{
      str_matches <- stringdist::stringdist(this_str, database[,1], nthread = no_cores, method = method)
    }
    
    #Add string to scores
    results <- cbind(database, str_matches)
    
    if (db == "nmnh"){
      names(results) <- c("match", "ID", "score")
    }else if (db == "gbif"){
      names(results) <- c("match", "no_records", "ROWID", "score")
    }
    
    return(results)
  }
}



find_matching_str2 <- function(str_to_check, database, method = "osa", no_cores = 2){
  
  #remove these chars
  this_str <- gsub("[?!*]", "", as.character(str_to_check))
  
  if (this_str == "" || this_str == "-" || this_str == "NA" || is.na(this_str)){
    cat("Empty string, returning NAs.")
    results <- as.data.frame(cbind(NA, NA))
    names(results) <- c("match", "score")
    return(results)
  }else{
    
    if (method == "jw"){
      #Jaro-Winkler distance
      str_matches <- stringdist::stringdist(this_str, database[,1], nthread = no_cores, method = method, p = 0.1)
    }else{
      str_matches <- stringdist::stringdist(this_str, database[,1], nthread = no_cores, method = method)
    }
    
    #Add string to scores
    results <- cbind(database, str_matches)
    
    names(results) <- c("match", "score")
    
    return(results)
  }
}
