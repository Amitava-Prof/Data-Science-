library(stringr)
library(ggplot2)

# Read file
uber <- read.csv("Uber Request Data.csv")
View(uber)

#Data Understanding - Total Records - 6745
length(unique(uber$Request.id))
#Request.id has unique entries
pick_up_city <- subset(uber, uber$Pickup.point == "City")
pick_up_airport <- subset(uber, uber$Pickup.point == "Airport")
#No empty Pickup.point, Possible values - City, Airport
# Driver.id is NA if the Status is 'No cars available'
status <- factor(uber$Status)
status_completed <- subset(uber, uber$Status == "Trip Completed")
status_cancelled <- subset(uber, uber$Status == "Cancelled")
status_not_available <- subset(uber, uber$Status == "No Cars Available")
# Status - no NA value, 3 levels - Trip Completed, Cancelled, No Cars Available
request_time <- subset(uber, is.na(uber$Request.timestamp))
# Request.timestamp - No NA value, mix of dateformat dd/mm/yyyy hh:mm:ss or dd-mm-yyyy hh:mm:ss sometimes no ss
# data is of July, 2016 from 11th to 15th
#Drop.timestamp - NA when the status is No cars Available or Cancelled, different date format


# Data Cleaning
#function to format the date
datefunction <- function(strdatetime){
  if (is.na(strdatetime) == TRUE)
    return(strdatetime)
  strdatetime <- gsub("[/]", "-",strdatetime)
  datetm <- strsplit(strdatetime, " ")
  strdate <- datetm[[1]][1]
  strtime <- datetm[[1]][2]
  ldate <- strsplit(strdate, "-")
  ltime <- strsplit(strtime,":")
  
  final_date = ""
  for (i in 1:3)
  {temp = as.character(ldate[[1]][i])
  temp = str_pad(temp, 2, pad = "0")
  if(i>1)
    final_date = paste(final_date, temp, sep = "-")
  else
    final_date = temp
  }
  
  final_time = ""
  for (i in 1:3)
  {temp = as.character(ltime[[1]][i])
  temp = str_pad(temp, 2, pad = "0")
  if(is.na(temp))
    final_time = paste(final_time, "00", sep = ":")
  else  if(i>1)
    final_time = paste(final_time, temp, sep = ":")
  else
    final_time = temp
  }
  final_datetime = paste(final_date,final_time)      
  #final_datetime <- strptime(final_datetime, format = "%d-%m-%Y %H:%M:%S")
  return(final_datetime)
}
  
# function to define the time of the day based on the hour
  timeofdayfunction <- function(hour){
    if( hour>= "04" & hour < "06" )
      return("Early Morning")
    else if( hour>="06" & hour< "12")
      return("Morning")
    else if (hour>="12" & hour < "14")
      return("Noon")
    else if (hour>="12" & hour < "14")
      return("Noon")
    else if(hour>="14" & hour < "16")
      return("Afternoon")
    else if (hour>="16" & hour <"20")
      return("Evening")
    else if (hour>="20" & hour <"24")
      return("Night")
    else if (hour>="00" & hour < "04")
      return("Midnight")
    else
      return(NA)
  }  
  
  Is_Airport_Cancelled <- function(x,y)
  {
    if(x == 'Airport' & y == 'Cancelled')
      return(1)
    else
      return(0)
  }
  Is_City_Cancelled <- function(x,y)
  {
    if(x == 'City' & y == 'Cancelled')
      return(1)
    else
      return(0)
  }
  Is_Airport_NA <- function(x,y)
  {
    if(x == 'Airport' & y == 'No Cars Available')
      return(1)
    else
      return(0)
  }
  Is_City_NA <- function(x,y)
  {
    if(x == 'City' & y == 'No Cars Available')
      return(1)
    else
      return(0)
  }
  
#copy uber dataset to uber_date  
uber_date <- uber
# call datefunction to changethe representation of Request,timestamp and Drop.timestamp to same format
uber_date$Request.timestamp = sapply(uber_date$Request.timestamp, function(x) datefunction(x))
uber_date$Drop.timestamp = sapply(uber_date$Drop.timestamp , function(x) datefunction(x))
# convert Request.timestamp and Drop.timestamp to timestamp format
uber_date$Request.timestamp <- strptime(uber_date$Request.timestamp, format = "%d-%m-%Y %H:%M:%S")
uber_date$Drop.timestamp <- strptime(uber_date$Drop.timestamp, format = "%d-%m-%Y %H:%M:%S")

# Derive new variable which will help in analysis
uber_date$Request_day <- as.numeric(format(uber_date$Request.timestamp,"%d"))
uber_date$Request_hour = as.numeric(format(uber_date$Request.timestamp, "%H"))
uber_date$trip_duration =  uber_date$Drop.timestamp - uber_date$Request.timestamp

# Extra column for plotting
uber_date$Demand = 1
uber_date$Supply = sapply(uber_date$Status, function(x) if (x == "Trip Completed") return(1) else return(0))
uber_date$Is_Cancelled = sapply(uber_date$Status, function(x) if (x == "Cancelled") return(1) else return(0))
uber_date$Is_Completed = sapply(uber_date$Status, function(x) if (x == "Trip Completed") return(1) else return(0))
uber_date$Is_Not_available = sapply(uber_date$Status, function(x) if (x == "No Cars Available") return(1) else return(0))
uber_date$Is_Gap = sapply(uber_date$Supply, function(x) if (x == 1) return(0) else return(1))
uber_date$Is_Airport_Pick_up_Cancelled = mapply(Is_Airport_Cancelled, uber_date$Pickup.point, uber_date$Status)
uber_date$Is_City_Pick_up_Cancelled = mapply(Is_City_Cancelled, uber_date$Pickup.point, uber_date$Status)
uber_date$Is_Airport_Pick_up_NA = mapply(Is_Airport_NA, uber_date$Pickup.point, uber_date$Status)
uber_date$Is_City_Pick_up_NA = mapply(Is_City_NA, uber_date$Pickup.point, uber_date$Status)

# Plot Graph
# 1.	Visually identify the most pressing problems for Uber
par(mfrow=c(2,2))
uber_date_city <- subset(uber_date, uber_date$Pickup.point == 'City')
# PLot for City
ggplot(uber_date_city, aes(x=Request_hour)) +  geom_bar(aes(fill=Status))
uber_date_Airport <- subset(uber_date, uber_date$Pickup.point == 'Airport')
# Plot for Airport
ggplot(uber_date_Airport, aes(x=Request_hour)) +  geom_bar(aes(fill=Status))

# Supply and Demand plotting
uber_new_df <- data.frame(hour = numeric(0), demand = numeric(0), supply = numeric(0), gap = numeric(0), cancelled = numeric(0), no_cab = numeric(0));
for (i in 0:23)
{
  newrow = data.frame(hour = i, demand = nrow(subset(uber_date,uber_date$Request_hour==i)), supply = nrow(subset(uber_date,uber_date$Request_hour == i & uber_date$Status == 'Trip Completed')), gap = 0,
                      cancelled = nrow(subset(uber_date,uber_date$Request_hour == i & uber_date$Status == 'Cancelled')), no_cab = nrow(subset(uber_date,uber_date$Request_hour == i & uber_date$Status == 'No Cars Available')))
  uber_new_df <- rbind(uber_new_df, newrow, stringsAsFactors=FALSE)
}
uber_new_df$gap = uber_new_df$demand - uber_new_df$supply

ggplot(uber_new_df, aes(x=hour)) +   geom_line(aes(y=demand, color="demand")) +   geom_line(aes(y=supply,color="supply")) 
ggplot(uber_new_df, aes(x=hour)) +   geom_line(aes(y=gap, color="gap"))
ggplot(uber_new_df, aes(x=hour, fill=hour)) +  geom_bar(aes(y=gap),stat="identity") 

# Analisis based on the driver id
uber_driver <- data.frame()
for(i in unique(uber_date$Driver.id[!is.na(uber_date$Driver.id)]))
{
  for (j in unique(uber_date$Request_day))
  {
    newrow = data.frame(id = i, trip_date = j, trip_city = nrow(subset(uber_date,uber_date$Driver.id==i & uber_date$Request_day == j & uber_date$Pickup.point == 'City')),
                        trip_airport = nrow(subset(uber_date,uber_date$Driver.id==i & uber_date$Request_day == j & uber_date$Pickup.point == 'Airport')),
                        cancel_city = nrow(subset(uber_date,uber_date$Driver.id==i & uber_date$Request_day == j & uber_date$Pickup.point == 'City' & uber_date$Status == "Cancelled")),
                        cancel_airport = nrow(subset(uber_date,uber_date$Driver.id==i & uber_date$Request_day == j & uber_date$Pickup.point == 'Airport' & uber_date$Status == "Cancelled")))
    uber_driver <- rbind(uber_driver, newrow, stringsAsFactors=FALSE)
  }
}

# All drivers are having more or equal trips than cancellation
uber_driver_not_working <- subset(uber_driver, uber_driver$trip_city + uber_driver$trip_airport < uber_driver$cancel_city + uber_driver$cancel_airport)


# Analysis on Trip duration
Airport_median = median(uber_date$trip_duration[!is.na(uber_date$trip_duration) & uber_date$Pickup.point == 'Airport'])
City_median = median(uber_date$trip_duration[!is.na(uber_date$trip_duration) & uber_date$Pickup.point == 'City'])
# Finding - No significant observation

# Writing the csv's to use it in the plotting of Tableau
write.csv(uber_driver,"uber_driver.csv")
write.csv(uber_date,"uber_datetime.csv")