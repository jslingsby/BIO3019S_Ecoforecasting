##' Download MODIS NDVI data
##' @param URL  web address where data is located
##'
## 1) How I really did it. For the prac we'll use option 2 because it's faster
## library("MODISTools") #Call R library MODISTools that allows us to download MODIS satellite data directly into R
##
## ndvi <- mt_subset(product = "MOD13Q1",
##                         lat = -34.100875,
##                         lon = 18.449375,
##                         band = "250m_16_days_NDVI",
##                         start = "2000-01-01",
##                         end = "2021-10-01",
##                         progress = FALSE)
##

## 2) How we'll do it for the prac: Read the data from a .csv file in my github repository for the course notes
##
download.NDVI <- function(URL) {
  
  # Wrap function in an if/else loop that checks if the URL is valid
  if (length(URL) == 1 & is.character(URL) & substr(URL,1,4)=="http") {
    
    # Read in data
    modat <- read.csv(URL)
    
    # Convert Digital Numbers (more efficient for data storage) to NDVI
    modat$NDVI <- modat$value*0.0001
    
    # Convert calendar_date to class "Date"
    modat$calendar_date <- as.Date(as.character(modat$calendar_date))
    
    # Return the data
    return(modat)
    
  } else {
    
    # If the URL is not valid return...
    print(paste("download.NDVI: Input URL not provided correctly",URL))
  }
}