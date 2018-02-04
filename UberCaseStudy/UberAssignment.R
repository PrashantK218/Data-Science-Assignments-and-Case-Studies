rm(list=ls())
####################################### Uber Assignment #####################################################

library(stringr)
library(ggplot2)
# Getting the Data
UberData <- read.csv("Uber Request Data.csv",stringsAsFactors = FALSE)
str(UberData)

###################################### Checking Data quality issues ########################################

#1. Identifying duplicate Request IDs 
sum(duplicated(UberData$Request.id)) # returns 0. There are no duplicate values

#2. Identifying missing datas

################################### Number of missing datas in each column ##############################################
sum(is.na(UberData))                    # Total 6564 of missing data
sum(is.na(UberData$Request.id))         #0
sum(is.na(UberData$Pickup.point))       #0
sum(is.na(UberData$Driver.id))          #2650
sum(is.na(UberData$Status))             #0
sum(is.na(UberData$Request.timestamp))  #0
sum(is.na(UberData$Drop.timestamp))     #3914

# From the above codes, we conclude that most of the blank values are from Driver.id and Drop.timestamp.
# Taking a look at the data it seems like these values are missing when the staus is 'cancelled' or 'no cars available'.
# We are going to leave these blank fields as it is.

#3. Identifying non-standardized datas

# a) As R is case sensitive, we need to have Pickup point and status all in lower cases for our ease.
# b) Having a look at the data frame, we can see that the Request.timestamp and Drop.timestamp are not in standard date format which R recognizes.
# c) For some dates, seconds value is missing.

##################################### Data Cleaning & Preparation #################################################

#1. Converting all the values of  Pickup.point and status column to lower cases.
# As we are going to treat them as factors and not just characters, we are also going to convert them as factors.

UberData$Pickup.point <- factor(tolower(UberData$Pickup.point))
summary(UberData$Pickup.point)

UberData$Status <- factor(tolower(UberData$Status))
summary(UberData$Status)

#2. Converting the Request.timestamp and Drop.timestamp into correct date-time format.
# Replacing "/" with "-" to  have a common format.

UberData$Request.timestamp <-  str_replace_all(UberData$Request.timestamp,pattern = '/','-')
UberData$Drop.timestamp <- str_replace_all(UberData$Drop.timestamp,pattern = '/','-')

# Replacing the missing "Seconds" value for each time with "00".

UberData$Request.timestamp <- as.POSIXct(UberData$Request.timestamp, format = "%d-%m-%Y %H:%M")
UberData$Drop.timestamp <- as.POSIXct(UberData$Drop.timestamp, format = "%d-%m-%Y %H:%M")

################################ Adding Derived Metrics ######################################

# Please note that I have created these time slots to fill all the request and drop times.
# These may not be accurate as present in dictionaries.
#--------------Time Slot----------  -----Range as in 24h clock-------- 
# early morning (4 am to 7:59 am)     >=4:00  and <8:00
# morning (8 am to 11:59 am)          >=8:00  and <12:00
# afternoon (12 to 2:59 pm)           >=12:00 and <15:00
# late afternoon (3 to 5:59 pm)       >=15:00 and <18:00
# evening (6 pm to 8:59 pm)           >=18:00 and <21:00
# late night (9 pm to 3:59 am)        >=21:00 and <=23:00 or >=0:00 and <4:00

# Extracting the Hour of Request
RequestHour <- as.numeric(format(UberData$Request.timestamp,"%H"))
UberData$RequestHour <- RequestHour

# Creating a "RequestTimeSlot" column with Time Slot as the dummy values
UberData$RequestTimeSlot <- "Time Slot"

# Creating  Time slots

#early morning
UberData$RequestTimeSlot[which(RequestHour>=4 & RequestHour<8)] <- "early morning"

# day time
UberData$RequestTimeSlot[which(RequestHour>=8 & RequestHour<12)] <- "morning"

# early afternoon
UberData$RequestTimeSlot[which(RequestHour>=12 & RequestHour<15)] <- "afternoon"

# late afternoon
UberData$RequestTimeSlot[which(RequestHour>=15 & RequestHour<18)] <- "late afternoon"

# evening
UberData$RequestTimeSlot[which(RequestHour>=18 & RequestHour<21)] <- "evening"

# late night
UberData$RequestTimeSlot[which(RequestHour>=21 & RequestHour<=23)] <- "late night"

# The rest of the values of RequestTimeSlot are filled with value as "Time Slot"
# We need to replace these values with late night as >=0:00 and <4:00 is the only time range remains.
which(UberData$RequestTimeSlot=="Time Slot")
UberData$RequestTimeSlot[which(UberData$RequestTimeSlot=="Time Slot")] <- "late night" 

# Converting RequestTimeSlot into a factor
UberData$RequestTimeSlot <- as.factor(UberData$RequestTimeSlot)

####################################### Data Analysis ##############################################


#1. Hourly demand of uber cabs
# We want to have the x-axis as hours and y axis as the number of requests for each airport-city and city-airport requests.
# Hence, we are going to use a dodged bar chart to show values for both the pickup points at a particular hour.
ggplot(UberData,aes(x=factor(RequestHour),fill=Pickup.point)) + geom_bar(position = "Dodge" ) + 
ggtitle("Hourly Demand of Cabs")+labs(x="Time in Hours", y="Number of Cabs Requested")

#2. Demand For Cabs by pickup points.
# We want to have x-axis as the pickup points and y axis as the number of requests. We are going to use a simple bar graph for that.
summary(UberData$Pickup.point)

ggplot(UberData,aes(x=Pickup.point)) + geom_bar(stat="count",col="black",fill="green")+
ggtitle("Demand For Cabs by Pick Up Points")+labs(x="Pick Up Point", y="Number of Cabs Requested")+
geom_text(stat='count',aes(label=..count..),vjust=-1)
# Conclusion: Clearly, More requests are from city-airport as compared to airport-city.

#3. Demand For Cabs by trip status
summary(UberData$Status)
ggplot(UberData,aes(x=Status)) + geom_bar(stat="count",col="black",fill="green")+
ggtitle("Demand For Cabs by Trip Status")+labs(x="Trip Status", y="Number of Cabs Requested")+
geom_text(stat='count',aes(label=..count..),vjust=-1)
# Conclusions:
# As stated in the video, when a trip is cancelled or no cars are available, it costs to the company's revenue.
# 1. The number of cancelled or no cars available status are more as compared to trip completed.
# 2. No cars availability is a bigger problem as compared to cancelled cars as number of no cars available is nearly twice as number of 
#    cancelled cars and close to number of trips completed.

#4. Segmented univariate analysis on status
ggplot(UberData,aes(x=Status,fill=Pickup.point)) + geom_bar(position = "Dodge" ) +
ggtitle("Demand For Cabs by Trip Status for each Pickup Point")+labs(x="Trip Status", y="Number of Cabs Requested") +
  geom_text(stat='count',aes(label=..count..),vjust=-0.5,position = position_dodge(width = .8))
# Conclusions:
# 1. city-airport trip is more cancelled as compared to airport-city.
# 2. Non-availability of cars is more likely to happen when a airport-city trip is requested.
# 3. As non-availability of cars is the most problematic situation, airport-city is the most problematic request.

# Now, let's identify the problematic Request time slots
#5.  Univariate analysis on Request time slots
ggplot(UberData,aes(x=RequestTimeSlot)) + geom_bar(stat="count",col="black",fill="green") +
ggtitle("Demand For Cabs by Time Slots")+labs(x="Time-Slots", y="Number of Cabs Requested")
# Conclusion:
# 1. Most requested time slot is evening. There are also significant requests in the early morning,
#    late night and daytime.

#6. Let's do a segmented univariate analysis on Request Time slot as segments of pick up points
ggplot(UberData,aes(x=RequestTimeSlot,fill=Pickup.point)) + geom_bar(position = "Dodge" ) +
ggtitle("Cabs Demand by Time Slots for each Pickup Point")+labs(x="Time-Slots", y="Number of Cabs Requested")

# Conclusions:
# 1. Most of the airport-city requests are at evening followed by late night.
# 2. Most of the city-airport requests are at early morning followed by daytime.

#7. Let's do a segmented univariate analysis on Request Time slot as segments of  trip status
ggplot(UberData,aes(x=RequestTimeSlot,fill=Status)) + geom_bar(position = "fill" ) + 
ggtitle("Cabs Demand by Time Slots for each Pickup Point")+labs(x="Time-Slots", y="Number of Cabs Requested")

# Conclusions:
# 1. Most of the cars are unavailable at evening followed by late night
# 2. Most of the trips are cancelled at early morning followed by daytime
# From the above two plots, we conclude that airport-city trips are more problematic as compared to city-airport trips,
# and the most problematic time slot is evening.

# Supply-Demand Analysis
# Demand = Total number of requests, Supply = Number of Trips completed, Gap = Demand-Supply or
# Number of cabs not available or got cancelled. Now let's analyse how severe is this gap.

# Adding a DemandStatus column
UberData$DemandStatus <- "Status"
UberData$DemandStatus[which(UberData$Status=="trip completed")] <- "Supplied"
UberData$DemandStatus[which(UberData$Status=="cancelled" | UberData$Status=="no cars available")] <- "Gap"
UberData$DemandStatus <- as.factor(UberData$DemandStatus)

# Overall supply-demand gap
ggplot(UberData,aes(x=DemandStatus)) + geom_bar(stat="count",col="black",fill="green") +
ggtitle("Supply-Demand Gap")+labs(x="Demand Status", y="Count") + geom_text(stat='count',aes(label=..count..),vjust=-1)

# Supply demand gap by time slot

ggplot(UberData,aes(x=DemandStatus, fill=RequestTimeSlot)) + geom_bar(position = "Dodge") +
ggtitle("Supply-Demand Gap By Time Slots")+labs(x="Demand Status", y="Count")+
geom_text(stat='count',aes(label=..count..),vjust=-0.5,position = position_dodge(width = .8))
# Most severe time slot is evening, followed by early morning.

#Finding the trip type for which the gap is highest in the evening time slot

UberData_Evening <- subset(UberData,UberData$RequestTimeSlot=="evening" & UberData$DemandStatus=="Gap")

ggplot(UberData_Evening,aes(x=DemandStatus, fill=Pickup.point)) + geom_bar()+
facet_wrap(~Status)+
ggtitle("Gap at Evening by Pickup Points")+labs(x="Demand Status", y="Count")+
geom_text(stat='count',aes(label=..count..,vjust=1))


#Finding the trip type for which the gap is highest in the early morning time slot

UberData_EarlyMorning <- subset(UberData,UberData$RequestTimeSlot=="early morning" & UberData$DemandStatus=="Gap")

ggplot(UberData_EarlyMorning,aes(x=DemandStatus, fill=Pickup.point)) + geom_bar() +
facet_wrap(~Status)+
ggtitle("Gap at early morning by pickup points")+labs(x="Demand Status", y="Count")+
geom_text(stat='count',aes(label=..count..,vjust=-0.25))

# We conclude that city-airport trips are with most gaps in the early morning time slot for cancellation of requests.
















  

