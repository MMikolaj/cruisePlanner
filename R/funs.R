## helps to clean data (coordinates) from the goolge sheet 

clean_data <- Vectorize(function(x) {
  
  x <- as.numeric(x) ## converst all values in column into numerics
  
  if (is.null(x)) {
    
    return(NA)
    
  } else if (x<=180 & x>=(-180) ) {## iv value is in a range of -180 : 180 degrees then keeps it
    
    return(x)
    
  } else { ## else it was converted to POSIX and needs to be translated back into date and from it the coordinate can be extracted
    
    d <- as.Date.POSIXct(x)
    
    Y <- format(d, format="%Y") ## the decimals are stored as "year"
    M <- format(d, format="%m") ## degreees are stored as "month"
    
    dd <- paste0(M, ".", Y)
    dd <- as.double(dd)
    return(dd)
  }
  
})


## extract names from a column where names are separated by /

extract_names <- function(x) {
  
  x <- strsplit(x, "/")
  x <- unlist(x)
  x <- gsub("^\\s|\\s$", "", x)
  x <- unique(x)
  x <- sort(x)
  
  return(x)
}
