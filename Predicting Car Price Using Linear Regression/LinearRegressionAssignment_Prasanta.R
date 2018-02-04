########################################## Assignment- Linear Regression #############################################

rm(list=ls())

library(tidyr)
library(stringr)
library(car)
library(MASS)



# Getting the Data
Cars <- read.csv("CarPrice_Assignment.csv",stringsAsFactors = FALSE)
str(Cars)

################################################# Checking Data Quality issues #####################################

#1. Checking for unnecessary header/summary rows
head(Cars) #No unnecessary header rows
tail(Cars) #No unnecessary summary rows

#2. Identifying duplicate car_IDs
sum(duplicated(Cars$car_ID)) # Returns 0. There are no duplicate values

#3. Identifying missing datas
sum(is.na(Cars)) # Returns 0. There are no missing values

#4. Outlier Treatment

# wheelbase
quantile(Cars$wheelbase,seq(0,1,0.01))
# There is a jump between the value at 99% and 100% percentile
Cars$wheelbase[which(Cars$wheelbase > 115.544)]<-115.544

#carheight
quantile(Cars$carheight,seq(0,1,0.01))
# No Outliers

quantile(Cars$carlength,seq(0,1,0.01))
# There is a jump between the values at 99% and 100% 
Cars$carlength[which(Cars$carlength > 202.480)]<-202.480

#There is a jump between the values at 2% and 3% 
Cars$carlength[which(Cars$carlength < 155.900)]<-155.900

# carwidth
quantile(Cars$carwidth,seq(0,1,0.01))
#There is a jump between the values at 0% and 1%
Cars$carlength[which(Cars$carwidth < 62.536)]<-62.536

# curbweight
quantile(Cars$curbweight,seq(0,1,0.01))
# There is a jump between the values at 0% and 1%
Cars$curbweight[which(Cars$carwidth < 1819.72 )]<-1819.72

# enginesize
quantile(Cars$enginesize,seq(0,1,0.01))
# There is a jump between the values at 2% and 3%
Cars$enginesize[which(Cars$enginesize < 90.00 )]<-90.00 

# There is a jump between the values at 96% and 97%
Cars$enginesize[which(Cars$enginesize > 209.00 )]<-209.00

quantile(Cars$boreratio,seq(0,1,0.01))
# There is a jump between the values at 0% and 1%
Cars$boreratio[which(Cars$boreratio < 2.9100 )]<-2.9100

quantile(Cars$stroke,seq(0,1,0.01))
# There is a jump between the values at 1% and 2%
Cars$stroke[which(Cars$stroke < 2.6400 )]<-2.6400
# There is a jump between the values at 95% and 96%
Cars$stroke[which(Cars$stroke > 3.6400 )]<-3.6400

quantile(Cars$compressionratio,seq(0,1,0.01))
# There is a jump between the values at 90% and 91%
Cars$compressionratio[which(Cars$compressionratio > 10.9400 )]<-10.9400

quantile(Cars$horsepower,seq(0,1,0.01))
# There is a jump between the values at 99% and 100%
Cars$horsepower[which(Cars$horsepower > 207.00 )]<-207.00

quantile(Cars$peakrpm,seq(0,1,0.01))
# There is a jump between the values at 99% and 100%
Cars$peakrpm[which(Cars$peakrpm > 6000 )]<-6000

quantile(Cars$citympg,seq(0,1,0.01))
# There is a jump between the values at 98% and 99%
Cars$citympg[which(Cars$citympg > 38.00 )]<-38.00

quantile(Cars$highwaympg,seq(0,1,0.01))
# There is a jump between the values at 99% and 100%
Cars$highwaympg[which(Cars$highwaympg > 49.88 )]<-49.88

# price
quantile(Cars$price,seq(0,1,0.01))

#There is a jump between the values at 4% and 5% 
Cars$price[which(Cars$price < 6197.00)]<- 6197.00

#There is a jump between the values at 98% and 99% 
Cars$price[which(Cars$price > 36809.60)]<- 36809.60


################################################ Data Cleaning and preparation ############################################


# Creating Derived Metrics

# power to weight ratio. horepower/curbweight.
Cars$PWRatio <- Cars$horsepower/Cars$curbweight
# Average mpg
Cars$avgmpg <- (Cars$citympg + Cars$highwaympg)/2
# length to width ratio
Cars$LWRatio <- Cars$carlength/Cars$carwidth
# width to height ratio
Cars$WHRatio <- Cars$carwidth/Cars$carheight
# length to height ratio
Cars$LHRatio <- Cars$carlength/Cars$carheight

# Checking Individual columns and creating dummies

#1.  car_ID : No dummies required. We can remove this independent variable right from the begining as it is not a vehicular metric.
#             It is just a row number.

Cars <- Cars[,-1]

#2.  symboling : Its assigned insurance risk rating, A value of +3 indicates that the auto is risky, -3 that it is probably pretty safe.(Categorical)
# Since this is categorical, but holds numeric values, we can convert it into a factor.

# Let's convert the values -2 and -1 into low_risky, 0 and 1 into med_risky, and 2 & 3 into highly_risky
min(Cars$symboling)
max(Cars$symboling)
# Values of symboling are -2,-1,0,1,2,3

Cars$symboling[which(Cars$symboling == -2 | Cars$symboling == -1)] <- "low_risky"
Cars$symboling[which(Cars$symboling == 0 | Cars$symboling == 1)] <- "med_risky"
Cars$symboling[which(Cars$symboling == 2 | Cars$symboling == 3)] <- "highly_risky"

str(Cars$symboling)
Cars$symboling <- as.factor(Cars$symboling)
levels(Cars$symboling)
summary(Cars$symboling)

#Converting "symboling" into dummies . 
dummy_1 <- data.frame(model.matrix( ~symboling, data = Cars))
dummy_1 <- dummy_1[,-1]
Cars <- cbind(Cars[,-1], dummy_1)

#3.  CarName : Consists of Car name and model name. We are only interested in Company name. Hence, we will drop car model.
## Please consider CarName as the company name
Cars <- separate(Cars, CarName, into = c("Car_Company", "drop"), sep = " ", extra = "drop", remove = T)
Cars <- Cars[ , !names(Cars) == "drop"]
Cars$Car_Company <- as.factor(Cars$Car_Company)
levels(Cars$Car_Company)

# We can see from the car levels that there are non-standardized car names probably because of spelling errors.
# We have to fix the following:

# Fixing "maxda" to "mazda"
Cars$Car_Company <- str_replace_all(Cars$Car_Company,pattern = 'maxda','mazda')

# Fixing "nissan" to "Nissan"
Cars$Car_Company <- str_replace_all(Cars$Car_Company,pattern = 'Nissan','nissan')

# Fixing "porcshce" to "porsche"
Cars$Car_Company <- str_replace_all(Cars$Car_Company,pattern = 'porcshce','porsche')

# Fixing "toyouta" to "toyota"
Cars$Car_Company <- str_replace_all(Cars$Car_Company,pattern = 'toyouta','toyota')

# Fixing "vokswagen" and "vw" to "volkswagen"
Cars$Car_Company <- str_replace_all(Cars$Car_Company,pattern = 'vokswagen','volkswagen')
Cars$Car_Company <- str_replace_all(Cars$Car_Company,pattern = 'vw','volkswagen')

# Fixing alfa-romero to alfa-romeo

Cars$Car_Company <- str_replace_all(Cars$Car_Company,pattern = 'alfa-romero','alfa romeo')

Cars$Car_Company <- as.character(Cars$Car_Company)
str(Cars$Car_Company)

# There are 22 different car companies. Creating dummies for 22 categories will create 21 new variables which will be huge.
# So, in order to reduce the number of variables, let's further categorise the car companies into:
# "luxury" and "compact".(Took the help of google and wikipedia ) 
# These categories are based on what type of cars these companies generally produce.
# One car company may have produced significant number of cars for a different category different that for which it is generally known for


Cars$Car_Company[which(Cars$Car_Company %in% c("alfa romeo","audi","bmw","buick","jaguar","porsche","volvo"))] <- "luxury"
Cars$Car_Company[which(Cars$Car_Company %in% c("chevrolet","dodge","honda","isuzu","mazda","mercury","mitsubishi","nissan","peugeot","plymouth","renault","saab","subaru","toyota","volkswagen"))] <- "compact"

Cars$Car_Company <- as.factor(Cars$Car_Company)
levels(Cars$Car_Company)

#Converting "Car_Company" into dummies .
Cars$Car_Company <- as.factor(Cars$Car_Company)
levels(Cars$Car_Company)
levels(Cars$Car_Company) <- c(0,1)
Cars$Car_Company <- as.numeric(levels(Cars$Car_Company))[Cars$Car_Company]

#4. fueltype     : Categorical. Needs to be converted into dummies.
str(Cars$fueltype)
Cars$fueltype <- as.factor(Cars$fueltype)
levels(Cars$fueltype)

# replacing fueltype levels gas and diesel with 0 and 1
levels(Cars$fueltype) <- c(0,1)
Cars$fueltype <- as.numeric(levels(Cars$fueltype))[Cars$fueltype]

#5. aspiration    : Categorical. Needs to be converted into dummies
str(Cars$aspiration)
Cars$aspiration <- as.factor(Cars$aspiration)
levels(Cars$aspiration)

# replacing aspiration levels std and turbo with 0 and 1
levels(Cars$aspiration) <- c(0,1)
Cars$aspiration <- as.numeric(levels(Cars$aspiration))[Cars$aspiration]

#6. doornumber    : Needs to be converted into dummies.
str(Cars$doornumber)
Cars$doornumber <- as.factor(Cars$doornumber)
levels(Cars$doornumber)

# replacing doornumber levels four and two with 0 and 1
levels(Cars$doornumber) <- c(0,1)
Cars$doornumber <- as.numeric(levels(Cars$doornumber))[Cars$doornumber]

#7. carbody
summary(factor(Cars$carbody))

#Converting "carbody" into dummies . 
dummy_2 <- data.frame(model.matrix( ~carbody, data = Cars))
dummy_2 <- dummy_2[,-1]
Cars <- cbind(Cars[,-5], dummy_2)

#8. drivewheel
summary(factor(Cars$drivewheel))
# Correcting "4wd" to "fwd"
Cars$drivewheel[which(Cars$drivewheel == "4wd")] <- "fwd"

#Converting "drivewheel" into dummies.
Cars$drivewheel <- as.factor(Cars$drivewheel)
levels(Cars$drivewheel) <- c(0,1)
Cars$drivewheel <- as.numeric(levels(Cars$drivewheel))[Cars$drivewheel]

#9. enginelocation
summary(factor(Cars$enginelocation))

#Converting "enginelocation" into dummies.
Cars$enginelocation <- as.factor(Cars$enginelocation)
levels(Cars$enginelocation) <- c(0,1)
Cars$enginelocation <- as.numeric(levels(Cars$enginelocation))[Cars$enginelocation]

#10. wheelbase : No dummies required

#No dummies required for carlength,carwidth,carheight,curbweight,enginesize,boreratio,stroke,compressionratio,horsepower,peakrpm,citympg and highwaympg


#enginetype :

summary(factor(Cars$enginetype))

#Converting "enginetype" into dummies . 
dummy_3 <- data.frame(model.matrix( ~enginetype, data = Cars))
dummy_3 <- dummy_3[,-1]
grep("enginetype",colnames(Cars))
Cars <- cbind(Cars[,-12], dummy_3)

#cylinder number:

summary(factor(Cars$cylindernumber))

#Converting "cylindernumber" into dummies . 
dummy_4 <- data.frame(model.matrix( ~cylindernumber, data = Cars))
dummy_4 <- dummy_4[,-1]
grep("cylindernumber",colnames(Cars))
Cars <- cbind(Cars[,-12], dummy_4)

#fuelsystem

summary(factor(Cars$fuelsystem))

#Converting "fuelsystem" into dummies . 
dummy_5 <- data.frame(model.matrix( ~fuelsystem, data = Cars))
dummy_5 <- dummy_5[,-1]
grep("fuelsystem",colnames(Cars))
Cars <- cbind(Cars[,-13], dummy_5)

# Creating Training and Test Data

set.seed(100)


# randomly generating row indices for train dataset
trainindices= sample(1:nrow(Cars), 0.7*nrow(Cars))

# generating the train data set
train = Cars[trainindices,]

# generating test data set
test = Cars[-trainindices,]

# Building Models

# Building model 1 containing all variables
model_1 <-lm(price~.,data=train)
summary(model_1)

# using stepAIC to remove insignificant independent variables
step <- stepAIC(model_1, direction="both")
step

model_2 <- lm(formula = price ~ Car_Company + aspiration + enginelocation + 
                carlength + carwidth + carheight + enginesize + boreratio + 
                stroke + peakrpm + LWRatio + symbolinglow_risky + symbolingmed_risky + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                enginetypel + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + cylindernumberthree + fuelsystemspdi, 
                data = train)

summary(model_2)
vif(model_2)

# Removing the variables onea at a time, and remodelling.( p-value is >0.05)
# Removing fuelsystemspdi as p-value >0.05

model_3 <- lm(formula = price ~ Car_Company + aspiration + enginelocation + 
                carlength + carwidth + carheight + enginesize + boreratio + 
                stroke + peakrpm + LWRatio + symbolinglow_risky + symbolingmed_risky + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                enginetypel + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + cylindernumberthree, data = train)

summary(model_3)
vif(model_3)

# Removing peakrpm as p-value >0.05

model_4 <- lm(formula = price ~ Car_Company + aspiration + enginelocation + 
                carlength + carwidth + carheight + enginesize + boreratio + 
                stroke + LWRatio + symbolinglow_risky + symbolingmed_risky + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                enginetypel + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + cylindernumberthree, data = train)

summary(model_4)
vif(model_4)

# Removing symbolingmed_risky as p-value >0.05

model_5 <- lm(formula = price ~ Car_Company + aspiration + enginelocation + 
                carlength + carwidth + carheight + enginesize + boreratio + 
                stroke + LWRatio + symbolinglow_risky + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                enginetypel + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + cylindernumberthree, data = train)

summary(model_5)
vif(model_5)

# For model 16, all the independent variables have p-value less than 0.05.
# We can continue removing the variables till the significance level is 0.001

# Removing enginetypeohcv as p-value >0.01

model_6 <- lm(formula = price ~ Car_Company + aspiration + enginelocation + 
                carlength + carwidth + carheight + enginesize + boreratio + 
                stroke + LWRatio + symbolinglow_risky + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                enginetypel + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + cylindernumberthree, data = train)

summary(model_6)
vif(model_6)

# Removing carbodysedan as p-value >0.01

model_7 <- lm(formula = price ~ Car_Company + aspiration + enginelocation + 
                carlength + carwidth + carheight + enginesize + boreratio + 
                stroke + LWRatio + symbolinglow_risky + 
                carbodyhardtop + carbodyhatchback + carbodywagon + 
                enginetypel + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + cylindernumberthree, data = train)

summary(model_7)
vif(model_7)

# Removing carbodyhatchback as p-value >0.01

model_8 <- lm(formula = price ~ Car_Company + aspiration + enginelocation + 
                carlength + carwidth + carheight + enginesize + boreratio + 
                stroke + LWRatio + symbolinglow_risky + 
                carbodyhardtop + carbodywagon + 
                enginetypel + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + cylindernumberthree, data = train)

summary(model_8)
vif(model_8)

# Removing carbodywagon as p-value >0.01

model_9 <- lm(formula = price ~ Car_Company + aspiration + enginelocation + 
                carlength + carwidth + carheight + enginesize + boreratio + 
                stroke + LWRatio + symbolinglow_risky + 
                carbodyhardtop + enginetypel + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + cylindernumberthree, data = train)

summary(model_9)
vif(model_9)

# Removing carbodyhardtop as p-value >0.01 

model_10 <- lm(formula = price ~ Car_Company + aspiration + enginelocation + 
                carlength + carwidth + carheight + enginesize + boreratio + 
                stroke + LWRatio + symbolinglow_risky + 
                enginetypel + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + cylindernumberthree, data = train)

summary(model_10)
vif(model_10)

# Removing carheight as p-value >0.01

model_11 <- lm(formula = price ~ Car_Company + aspiration + enginelocation + 
                 carlength + carwidth + enginesize + boreratio + 
                 stroke + LWRatio + symbolinglow_risky + 
                 enginetypel + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + cylindernumberthree, data = train)

summary(model_11)
vif(model_11)

# Removing enginetypel as p-value >0.01

model_12 <- lm(formula = price ~ Car_Company + aspiration + enginelocation + 
                 carlength + carwidth + enginesize + boreratio + 
                 stroke + LWRatio + symbolinglow_risky + 
                 cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + cylindernumberthree, data = train)

summary(model_12)
vif(model_12)

# Removing symbolinglow_risky as p-value >0.01

model_13 <- lm(formula = price ~ Car_Company + aspiration + enginelocation + 
                 carlength + carwidth + enginesize + boreratio + 
                 stroke + LWRatio + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + cylindernumberthree, data = train)

summary(model_13)
vif(model_13)

# At model_13, we have all the variables with very low p-value
# but there are some variables with very high vifs.

# Checking the correlation of carlength and LWRatio

cor(Cars$carlength,Cars$LWRatio) # Very high correlation of 0.9577049

# pvalue of carlength is greater than LWRatio. Hence, we will drop carlength and build another model 

model_14 <- lm(formula = price ~ Car_Company + aspiration + enginelocation + 
                 carwidth + enginesize + boreratio + 
                 stroke + LWRatio + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + cylindernumberthree, data = train)

summary(model_14)
vif(model_14)

# Removing cylindernumberthree as p-value > 0.01

model_15 <- lm(formula = price ~ Car_Company + aspiration + enginelocation + 
                 carwidth + enginesize + boreratio + 
                 stroke + LWRatio + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix , data = train)

summary(model_15)
vif(model_15)

# Removing LWRatio as p-value >0.01

model_16 <- lm(formula = price ~ Car_Company + aspiration + enginelocation + 
                 carwidth + enginesize + boreratio + 
                 stroke + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix , data = train)

summary(model_16)
vif(model_16)

# cylindernumberfour and cylindernumbersix have vif >2.
# Checking their correlation
cor(Cars$cylindernumberfour,Cars$cylindernumbersix)
# very high negative correlation of -0.6769958



# Removing cylindernumbersix as it has greater p-value

model_17 <- lm(formula = price ~ Car_Company + aspiration + enginelocation + 
            carwidth + enginesize + boreratio + 
            stroke + cylindernumberfive + cylindernumberfour , data = train)

summary(model_17)
vif(model_17)

# Removing boreratio as p-value >0.01 

model_18 <- lm(formula = price ~ Car_Company + aspiration + enginelocation + 
            carwidth + enginesize + 
            stroke + cylindernumberfive + cylindernumberfour , data = train)

summary(model_18)
vif(model_18)

# Removing Stroke as p-value >0.01

model_19 <- lm(formula = price ~ Car_Company + aspiration + enginelocation + 
            carwidth + enginesize + cylindernumberfive + cylindernumberfour , data = train)

summary(model_19)
vif(model_19)

# Checking correlaton of cylindernumberfive and cylindernumberfour
cor(Cars$cylindernumberfive,Cars$cylindernumberfour) # High negative correlation of -0.4427057

# Removing cylindernumberfive as it has greater p-value

model_20 <- lm(formula = price ~ Car_Company + aspiration + enginelocation + 
            carwidth + enginesize + cylindernumberfour , data = train)

summary(model_20)
vif(model_20)

# Removing aspiration as p-value >0.01

model_21 <- lm(formula = price ~ Car_Company + enginelocation + 
            carwidth + enginesize + cylindernumberfour , data = train)

summary(model_21)
vif(model_21)

# carwidth and enginesize have vif >2
# checking their correlation
cor(Cars$carwidth,Cars$enginesize)

# Removing carwidth as it has greater p-value

model_22 <- lm(formula = price ~ Car_Company + enginelocation + 
            enginesize + cylindernumberfour , data = train)

summary(model_22)
vif(model_22)

# Removing cylindernumberfour as p-value >0.01

model_23 <- lm(formula = price ~ Car_Company + enginelocation + 
            enginesize , data = train)

summary(model_23)
vif(model_23)

# predicting the results in test dataset
grep("price",colnames(test))
Predict_1 <- predict(model_23,test[,-20])
test$test_price <- Predict_1

r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared

# Actual vs predicted data
plot(test$price,col = "green", type = "l" )
lines(test$test_price,col = "red", type = "l")

# Price = -6635.84 + 6816.95* Car_Company + 6039.95*enginelocation + 145.54*enginesize
# Hence, we can conclude that the price of a car depends on the
# Car Company, Engine Size and Engine's location.




