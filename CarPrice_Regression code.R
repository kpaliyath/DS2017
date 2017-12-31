# Ready the workspace

rm(list=ls())

#Load the requisite packages
#
library("car")
library("MASS")
#Load the data and save it as CarPrice dataframe

carprice<-read.csv("CarPrice_Assignment.csv",stringsAsFactors = FALSE)

#Lets have a look at the data loaded

View(carprice)

#Lets look at the summary and str of the data to understand more

summary(carprice)
str(carprice)

#As per the clue provided, the car names would need to be split into the brand and model. We can do away with
#the model as the characteristics of the vehicle would hold more importance than the model name
library(stringr)
carprice$CarName<- sapply(strsplit(carprice$CarName," "), '[', 1)
str(carprice$CarName)

#Lets look at the uniqueness of the car makes and the carID

unique(carprice$CarName)
unique(carprice$car_ID)

#Ignore below code start

#It seems there are many spelling mistakes an acronyms used to describe certain vehicles
#
#Volkswagen is mentioned as (vw,vokswagen and volkswagen)
#Mazda is mentioned as maxda
#Nissan is also mentioned as lower case nissan
#Porsche is mentioned as porcshce
#Toyota is mentioned as toyouta
#
#So lets correct these before moving forward by using gsub

carprice$CarName<-gsub("vw","volkswagen",carprice$CarName)
carprice$CarName<-gsub("vokswagen","volkswagen",carprice$CarName)
carprice$CarName<-gsub("porcshce","porsche",carprice$CarName)
carprice$CarName<-gsub("toyouta","toyota",carprice$CarName)
carprice$CarName<-gsub("maxda","mazda",carprice$CarName)
carprice$CarName<-tolower(carprice$CarName)

#Lets check the unique names left

unique(carprice$CarName)

#Ignore above code end

# For this analysis however we shall be skipping above steps as we want to look at the variables affecting
# car prices. Alternatively we can use only the car make as independent variables and once we have our model try to see the design
# of those car models which are driving prices. Hence we will be removing carnames


carprice<-carprice[,-3] 





#Lets check how many NA values are there in the data

sum(is.na(carprice))

# No NA values. Moving on we will now check for outliers in the data. We shall run this on numeric variables

quantile(carprice$symboling, seq(0,1,0.01))

# None here, moving on to wheelbase

quantile(carprice$wheelbase, seq(0,1,0.01))

# None here, moving on to carlength

quantile(carprice$carlength, seq(0,1,0.01))

# None here, moving on to carwidth

quantile(carprice$carwidth, seq(0,1,0.01))

# None here, moving on to carheight

quantile(carprice$carheight, seq(0,1,0.01))

# None here, moving on to curbweight
 
quantile(carprice$curbweight, seq(0,1,0.01))

#Here there seems to be a suddenjump from 1488 to 1819. Replacing this data with 1819 might be a good idea
carprice$curbweight[which(carprice$curbweight ==1488.00)]<-1819

# Moving on to engine size

quantile(carprice$enginesize, seq(0,1,0.01))

# Now let us look at converting the categorical variables to dummy variables so they can be used for analysis

dummy_1 <- data.frame(model.matrix( ~fueltype, data = carprice))
fueltypegas <- dummy_1[,-1]

# Combine the dummy variables and the numeric columns of housing dataset, in a new dataset called housing_1
carprice_1 <- cbind(carprice[,-3], fueltypegas)

# Similarly let us convert other categorical variables to dummy variables


#Aspiration

dummy_2 <- data.frame(model.matrix( ~aspiration, data = carprice_1))
View(dummy_2)
aspirationturbo <- dummy_2[,-1]

carprice_2 <- cbind(carprice_1[,-3], aspirationturbo)

#Doornumber, carbody, drivewheel

dummy_3 <- data.frame(model.matrix( ~doornumber, data = carprice_2))
View(dummy_3)
doornumbertwo<- dummy_3[,-1]

carprice_3 <- cbind(carprice_2[,-3], doornumbertwo)
#
dummy_4 <- data.frame(model.matrix( ~carbody, data = carprice_3))
View(dummy_4)
dummy_4<- dummy_4[,-1]

carprice_4 <- cbind(carprice_3[,-3], dummy_4)
#

dummy_5 <- data.frame(model.matrix( ~drivewheel, data = carprice_4))
View(dummy_5)
dummy_5<- dummy_5[,-1]

carprice_5 <- cbind(carprice_4[,-3], dummy_5)

# Converting enginelocation, enginetype, cylinder number, fuel system to dummy variables

dummy_6 <- data.frame(model.matrix( ~enginelocation, data = carprice_5))
View(dummy_6)
enginelocationrear<- dummy_6[,-1]

carprice_6 <- cbind(carprice_5[,-3], enginelocationrear) 

#

dummy_7 <- data.frame(model.matrix( ~enginetype, data = carprice_6))
View(dummy_7)
dummy_7<- dummy_7[,-1]

carprice_7 <- cbind(carprice_6[,-8], dummy_7) 

#

dummy_8 <- data.frame(model.matrix( ~cylindernumber, data = carprice_7))
View(dummy_8)
dummy_8<- dummy_8[,-1]

carprice_8 <- cbind(carprice_7[,-8], dummy_8)

dummy
#

dummy_9 <- data.frame(model.matrix( ~fuelsystem, data = carprice_8))
View(dummy_9)
dummy_9<- dummy_9[,-1]

carprice_9 <- cbind(carprice_8[,-9], dummy_9)

# Lets have a look at the data prepared so far

View(carprice_9)

# CarID column just represents serial number. We can remove this

carprice_9<-carprice_9[,-1]


# Lets try to BIN the symboling

unique(carprice_9$symboling)


str(carprice_9$symboling)

carprice_9$symboling[which(carprice_9$symboling==-2)]<-"verysafe"
carprice_9$symboling[which(carprice_9$symboling==-1)]<-"verysafe"
carprice_9$symboling[which(carprice_9$symboling==0)]<-"safe"
carprice_9$symboling[which(carprice_9$symboling==1)]<-"notsafe"
carprice_9$symboling[which(carprice_9$symboling==2)]<-"notsafe"
carprice_9$symboling[which(carprice_9$symboling==3)]<-"notsafe"

#Converting symboling to dummy

dummy_10 <- data.frame(model.matrix( ~symboling, data = carprice_9))
View(dummy_10)
dummy_10<- dummy_10[,-1]

carprice_10 <- cbind(carprice_9[,-1], dummy_10)
View(carprice_10)

#Lets create some new metrics out of the data
#Vehiclevolume can be approximated by multiplying length*breadth*height

carprice_10$vehiclevol<-carprice_10$carlength*carprice_10$carwidth*carprice_10$carheight

# separate training and testing data
set.seed(100)
trainindices= sample(1:nrow(carprice_10), 0.7*nrow(carprice_10))
train = carprice_10[trainindices,]
test = carprice_10[-trainindices,]

# Build model 1 containing all variables
model_1 <-lm(price~.,data=train)
summary(model_1)

#Lets run StepAIC
step <- stepAIC(model_1, direction="both")
step
# Let's execute this model here, 
model_2 <- lm(price ~ fuelsystemspdi+ enginetypeohcv+
                enginetypeohc+symbolingverysafe+
                carbodysedan+carbodyhardtop+
                enginetypel+carwidth+peakrpm+
                curbweight+aspirationturbo+
                stroke+carbodyhatchback+
                carbodywagon+carheight+
                cylindernumbersix+vehiclevol+
                carlength+cylindernumberfive+
                enginelocationrear+enginesize+
                cylindernumberfour,
              data = train)
# Lets look at the summary of Model 2

summary(model_2)

## Let us check for multicollinearity 

vif(model_2)

# Looking at the data and checking for extreme VIF and their significance we find below
# fuelsystemspdi,enginetypeohcv,enginetypeohc as they have very high p values>0.05 compared to other variables
# Lets create model_3 by removing these 2
#

model_3 <- lm(price ~ symbolingverysafe+
                carbodysedan+carbodyhardtop+
                enginetypel+carwidth+peakrpm+
                curbweight+aspirationturbo+
                stroke+carbodyhatchback+
                carbodywagon+carheight+
                cylindernumbersix+vehiclevol+
                carlength+cylindernumberfive+
                enginelocationrear+enginesize+
                cylindernumberfour,
              data = train)

summary(model_3)

#Running VIF on the model

vif(model_3)

#Lets remove carbodysedan,carbodyhardtop as it seems to be really insignificant compared to the other variables.
#Create model_4 basis this

model_4 <- lm(price ~ symbolingverysafe+
                enginetypel+carwidth+peakrpm+
                curbweight+aspirationturbo+
                stroke+carbodyhatchback+
                carbodywagon+carheight+
                cylindernumbersix+vehiclevol+
                carlength+cylindernumberfive+
                enginelocationrear+enginesize+
                cylindernumberfour,
              data = train)
summary(model_4)

vif(model_4)


#Removing carwidth

model_5 <- lm(price ~ symbolingverysafe+
                enginetypel+peakrpm+
                curbweight+aspirationturbo+
                stroke+carbodyhatchback+
                carbodywagon+carheight+
                cylindernumbersix+vehiclevol+
                carlength+cylindernumberfive+
                enginelocationrear+enginesize+
                cylindernumberfour,
              data = train)
summary(model_5)

vif(model_5)

#Removing carheight
#
model_6 <- lm(price ~ symbolingverysafe+
                enginetypel+peakrpm+
                curbweight+aspirationturbo+
                stroke+carbodyhatchback+
                carbodywagon+
                cylindernumbersix+vehiclevol+
                carlength+cylindernumberfive+
                enginelocationrear+enginesize+
                cylindernumberfour,
              data = train)
summary(model_6)

vif(model_6)

#Removing carbodyhatchback,symbolingverysafe

model_7 <- lm(price ~ enginetypel+peakrpm+
                curbweight+aspirationturbo+
                stroke+carbodywagon+
                cylindernumbersix+vehiclevol+
                carlength+cylindernumberfive+
                enginelocationrear+enginesize+
                cylindernumberfour,
              data = train)
summary(model_7)

vif(model_7)



#Removing carlength

model_8 <- lm(price ~ enginetypel+peakrpm+
                curbweight+aspirationturbo+
                stroke+carbodywagon+
                cylindernumbersix+vehiclevol+
                cylindernumberfive+
                enginelocationrear+enginesize+
                cylindernumberfour,
              data = train)
summary(model_8)

vif(model_8)

#Removing curbweight

model_9 <- lm(price ~ enginetypel+peakrpm+
                aspirationturbo+
                stroke+carbodywagon+
                cylindernumbersix+vehiclevol+
                cylindernumberfive+
                enginelocationrear+enginesize+
                cylindernumberfour,
              data = train)
summary(model_9)

vif(model_9)

#Removing cylindernumberfive

model_10 <- lm(price ~ enginetypel+peakrpm+
                aspirationturbo+
                stroke+carbodywagon+
                cylindernumbersix+vehiclevol+
                enginelocationrear+enginesize+
                cylindernumberfour,
              data = train)
summary(model_10)

vif(model_10)

#Removing enginetypel

model_11 <- lm(price ~ peakrpm+
                 aspirationturbo+
                 stroke+carbodywagon+
                 cylindernumbersix+vehiclevol+
                 enginelocationrear+enginesize+
                 cylindernumberfour,
               data = train)
summary(model_11)

vif(model_11)

#Removing carbodywagon

model_12 <- lm(price ~ peakrpm+
                 aspirationturbo+
                 stroke+
                 cylindernumbersix+vehiclevol+
                 enginelocationrear+enginesize+
                 cylindernumberfour,
               data = train)
summary(model_12)

vif(model_12)

#Removing enginesize as it still has less significance compared to others

model_13 <- lm(price ~ peakrpm+
                 aspirationturbo+
                 stroke+
                 cylindernumbersix+vehiclevol+
                 enginelocationrear+
                 cylindernumberfour,
               data = train)
summary(model_13)

vif(model_13)

#However we notice that the moment we dropped enginesize the R-squared and its adjusted value
#have come down drastically. So we will stick with model_12 as final

# predicting the results in test dataset
Predict_1 <- predict(model_12,test[,-14])
test$test_price <- Predict_1

# Now, we need to test the r square between actual and predicted. 
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared

# Now if we look at the R-squared obtained from our test and train, both are almost the same (0.7725 and 0.7558)
# From this analysis, we can inform the management of Geely that the variables that are significant in
# predicting price are
# 1. the peak rpm
# 2. aspiration turbo
# 3. stroke
# 4. 6 cylinder
# 5. 4 cylinder
# 6. The vehicle volume or space (derived metric)
# 7. Engine located at the rear
# 8. engine size
# 
# Hence Geely can produce vehicles with above variables in mind
# 

