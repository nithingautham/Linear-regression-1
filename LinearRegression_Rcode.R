setwd("E:\\Nitin")
## Rcode
library(moments)
library(ggplot2)
library(GGally)
library(MASS)
library(car)

carmileage <- read.csv("carMPG.csv" )


# Checking data redundancy using de duplication
sum( duplicated( carmileage[,] ))



# Data prepration 

# Converting all car names to lower case
carmileage$Car_Name <- tolower(carmileage$Car_Name)

## Converting all continuous variables into numeric type for further EDA analysis
carmileage[ , c('MPG','Displacement','Horsepower','Weight','Acceleration','Cylinders')] <-
  sapply(carmileage[ , c('MPG','Displacement','Horsepower','Weight','Acceleration','Cylinders')], function(x) as.numeric( as.character( x)  ))

#Checking for NA values
sum(is.na(  carmileage))

## Replacing "?"/NA values  by median
carmileage$Horsepower[is.na(carmileage$Horsepower)] <- median( carmileage$Horsepower , na.rm = T  )


## Checkpoint 1: Business Understanding and Data Understanding

## EDA & Statistics of Continuous variables in dataset


# MPG -EDA - To understand normality and other features

summary(carmileage$MPG)

boxplot(carmileage$MPG)

skewness(carmileage$MPG)

kurtosis((carmileage$MPG))

ggplot(carmileage , aes(x=carmileage$MPG)) + geom_histogram()

boxplot.stats(carmileage$MPG)$out

# Displacement -EDA - To understand normality and other features

boxplot(MPG~ as.factor(Displacement),data=carmileage, main="Car Milage Data",
        xlab="model year", ylab="Miles Per Gallon")

summary(carmileage$Displacement)

skewness(carmileage$Displacement)

skewness(  (carmileage$Displacement))

kurtosis((carmileage$Displacement))

boxplot(carmileage$Displacement)

ggplot(carmileage , aes(x=(carmileage$Displacement))) + geom_histogram()

boxplot.stats(carmileage$Displacement)$out

# Horsepower  -EDA - To understand normality and other features

boxplot(MPG~ as.factor(Horsepower),data=carmileage, main="Car Milage Data",
        xlab="model year", ylab="Miles Per Gallon")



summary(carmileage$Horsepower)

skewness(carmileage$Horsepower)

kurtosis((carmileage$Horsepower))

boxplot(carmileage$Horsepower)

ggplot(carmileage , aes(x=carmileage$Horsepower)) + geom_histogram()

## Outlier treatment - using capping and flooring method
boxplot.stats(carmileage$Horsepower)$out

carmileage$Horsepower[ carmileage$Horsepower > quantile(carmileage$Horsepower, probs = 0.95)]<-quantile(carmileage$Horsepower, probs = 0.95)

carmileage$Horsepower[ carmileage$Horsepower < quantile(carmileage$Horsepower, probs = 0.05)]<-quantile(carmileage$Horsepower, probs = 0.05)




# Weight - EDA - To understand normality and other features


boxplot(MPG~ as.factor(Weight),data=carmileage, main="Car Milage vs weight data",
        xlab="weight", ylab="Miles Per Gallon")


summary(carmileage$Weight)

skewness(carmileage$Weight)

kurtosis((carmileage$Weight))

boxplot(carmileage$Weight)

ggplot(carmileage , aes(x=carmileage$Weight)) + geom_histogram()

boxplot.stats(carmileage$Weight)$out

## Accelaration -EDA - To understand normality and other features
boxplot(MPG~ as.factor(Acceleration),data=carmileage, main="Car Milage Data",
        xlab="model year", ylab="Miles Per Gallon")


summary(carmileage$Acceleration)

skewness(carmileage$Acceleration)

kurtosis((carmileage$Acceleration))

boxplot(carmileage$Acceleration)

ggplot(carmileage , aes(x=carmileage_train$Acceleration)) + geom_histogram()


## Outlier treatment - using capping and flooring method
boxplot.stats(carmileage$Acceleration)$out

carmileage$Acceleration[ carmileage$Acceleration > quantile(carmileage$Acceleration, probs = 0.95)]<-quantile(carmileage$Acceleration, probs = 0.95)

carmileage$Acceleration[ carmileage$Acceleration < quantile(carmileage$Acceleration, probs = 0.05)]<-quantile(carmileage$Acceleration, probs = 0.05)


## multi-valued discrete

## Cylinders - To understand normality and other features

carmileage$Cylinders <- as.factor( as.numeric(  carmileage$Cylinders  ))

summary(carmileage$Cylinders  )

ggplot( carmileage , aes( x= carmileage$Cylinders ) ) + geom_bar()

## Observation : cars with Cylinders 4 & 5 have higher MPG than the rest ( 3 ,6 ,8)
boxplot(MPG~Cylinders,data=carmileage, main="Car Milage Data",
        xlab="model year", ylab="Miles Per Gallon")

## Model year

carmileage$Model_year <- as.factor(carmileage$Model_year  )

## EDA to observe pattern / relation between model year(categorical) and MPG(Quantitative)
## We can clearly observe that older vehicles(2003-2006) have higher mileage than vehicles after 2006
boxplot(MPG~Model_year,data=carmileage, main="Car Milage Data",
        xlab="model year", ylab="Miles Per Gallon")


summary(carmileage$Model_year  )

ggplot( carmileage , aes( x= carmileage$Model_year ) ) + geom_bar()

## Therefore ,i will be binning/classifying model year data into two groups ( 2003-2005) and (2006-2015)

## Origin

carmileage$Origin <- as.factor(carmileage$Origin  )

summary(carmileage$Origin  )

ggplot( carmileage , aes( x= carmileage$Origin ) ) + geom_bar()
## EDA to observe pattern / relation between origin(categorical) and MPG(Quantitative)
## We can clearly observe that origin 1 ,2 , 3 have different range and centers
boxplot(MPG~Origin,data=carmileage, main="Car Milage Data",
        xlab="origin", ylab="Miles Per Gallon")


## Car name 

unique( carmileage$Car_Name)


## As part of data understanding , Association Between Variables is analysed

ggpairs(data=carmileage[ ,c( 'Displacement','Horsepower','Weight','Acceleration'  )], 
        columns=1:4)


##### Checkpoint - 2  Data Cleaning and Preparation

## Feature extraction - Extracting Car company name from model names

carmileage$company_name <- lapply(carmileage$Car_Name , function(x) tolower(  strsplit( as.character(x) ," ")[[1]][1]) )

unique( carmileage$company_name)

## Data cleaning : replace all "chevrolet" with chevy and "toyouta" with "toyota"

carmileage$company_name[ carmileage$company_name == "chevrolet"  ] <- "chevy"
carmileage$company_name[ carmileage$company_name == "chevroelt"  ] <- "chevy"
carmileage$company_name[ carmileage$company_name == "toyouta"  ] <- "toyota"
carmileage$company_name[ carmileage$company_name == "mercedes"  ] <- "mercedes-benz"
carmileage$company_name[ carmileage$company_name == "vokswagen"  ] <- "volkswagen"
carmileage$company_name[ carmileage$company_name == "vw"  ] <- "volkswagen"
carmileage$company_name[ carmileage$company_name == "maxda"  ] <- "mazda"

carmileage$company_name <- as.factor( as.character( carmileage$company_name ))

## Clasifying company_name into three categories based on their median MPG observed in boxplot below
boxplot(MPG~company_name,data=carmileage, main="Car Milage Data",
        xlab="model year", ylab="Miles Per Gallon")


company_names_vs_mpg <- as.data.frame( aggregate(MPG ~ company_name  , data = carmileage, FUN = median) )
company_names_vs_mpg$MPG <- as.numeric( company_names_vs_mpg$MPG )

## after analysis using boxplot and () company name vs MPG median) reducing the levels from of 
## car company name into three buckets related to MPG
carmileage$company_name_three_group<- sapply(carmileage$company_name, function(x)  if(company_names_vs_mpg$MPG[company_names_vs_mpg$company_name == x] 
                                                < 18 ) "low_mileage_brand_name" 
                                            else if(company_names_vs_mpg$MPG[company_names_vs_mpg$company_name == x] 
                                               > 28 ) "high_mileage_brand_name"
                                             else  "medium_mileage_brand_name")
carmileage$company_name_three_group <- as.factor(carmileage$company_name_three_group)

company_name_three_group_dummy <- as.data.frame( model.matrix(~ company_name_three_group -1 , data = carmileage))

## removing one variable from dummy variable to meet N-1 critiria ,so chose to remove "medium_mileage_brand_name"
carmileage <- cbind( carmileage , company_name_three_group_dummy[,-3] )


## Binning /Classify model years into two groups . high mileage model years and low mileage model years 
## due to the timely trend in decrease in mileage observed across time
## viewed in boxplot
boxplot(MPG~Model_year,data=carmileage, main="Car Milage Data",
        xlab="model year", ylab="Miles Per Gallon")


carmileage$Model_year <-  as.numeric( as.character( carmileage$Model_year))

carmileage$Model_year_Two_GROUP <- sapply( carmileage$Model_year , function(x)  if(  x >=2003 & x<=2005)  1 
                                           else if(  x >=2006 & x<=2015)  0  ) 


## converting categorical variables( origin ,Model_year ) into numerical using model.matrix
carmileage$Origin <- as.factor( (  carmileage$Origin) )
origin <- as.data.frame(  model.matrix( ~ Origin -1 , data=carmileage ))
unique(  carmileage$Origin )
carmileage <- cbind( carmileage , origin[,-2] )


## Binning cylinder values into two grous 
## observation made from boxplot of cylinder and mpg 
## Cylinders 4 and 5 have high MPG values wheres the rest have low MPG values
## so Cylinders 4 and 5 will be replace by "1" and the rest by "0" ( shortcut of dummy variables)

carmileage$Cylinders <- sapply(  carmileage$Cylinders  , function(x)  if( x ==4 | x==5 ) 1 else 0 )

## Remove categorical variables like origin , model year , car name , company name

carmileage$Model_year <- NULL
carmileage$Origin <- NULL
carmileage$Car_Name <- NULL
carmileage$company_name <- NULL
carmileage$company_name_three_group <- NULL



## converting all variables into numerical before model building
carmileage$MPG <- as.numeric( carmileage$MPG)
carmileage$Cylinders <- as.numeric( carmileage$Cylinders)
carmileage$Displacement <- as.numeric( carmileage$Displacement)
carmileage$Horsepower <- as.numeric( carmileage$Horsepower)
carmileage$Weight <- as.numeric( carmileage$Weight)
carmileage$Acceleration <- as.numeric( carmileage$Acceleration)
carmileage$company_name_three_grouplow_mileage_brand_name <- as.numeric( carmileage$company_name_three_grouplow_mileage_brand_name)
carmileage$company_name_three_grouphigh_mileage_brand_name <- as.numeric( carmileage$company_name_three_grouphigh_mileage_brand_name)
carmileage$Origin1 <- as.numeric( carmileage$Origin1)
carmileage$Origin3 <- as.numeric( carmileage$Origin3)
carmileage$Model_year_Two_GROUP <- as.numeric( carmileage$Model_year_Two_GROUP)


## Splitting data into Training and test set according to business rule of 70:30 respectively

set.seed(100)

train_data_Set_indexes <- sample(1:nrow(carmileage), size=0.7*nrow(carmileage))

carmileage_train <- carmileage[train_data_Set_indexes,]

carmileage_test <- carmileage[ -train_data_Set_indexes,]

## Model Bulding Starts

model_1 <- lm( MPG ~ . ,data = carmileage_train[,] )

summary(model_1)

## Using stepAIC to apply stepwise variable reduction method
step    <- stepAIC(model_1 , direction = "both"  )

## to get LM code/formula
step$call


##  model_2 after stepwise selection method using stepAIC

model_2 <- lm(formula = MPG ~ Cylinders + Displacement + Horsepower + Weight + 
                Acceleration + company_name_three_grouphigh_mileage_brand_name + 
                Model_year_Two_GROUP, data = carmileage_train[, ])
# Summary of model 2
summary(model_2)

# Checking VIF value
vif( model_2 )

## model 3 , in previous model "Displacement" had high VIF above 2 and very low significance 
## therefore , Displacement is removed from model 3
model_3 <- lm(formula = MPG ~ Cylinders  + Horsepower + Weight + 
                Acceleration + company_name_three_grouphigh_mileage_brand_name + 
                Model_year_Two_GROUP, data = carmileage_train[, ])
# Summary of model 3
summary(model_3)

# Checking VIF value
vif( model_3 )

cor(  carmileage_train$Horsepower , carmileage_train$Weight)

## model 4 , in previous model "Acceleration" had high VIF above 2 and very low significance 
## therefore , Acceleration is removed from model 4



model_4 <-  lm(formula = MPG ~ Cylinders  + Horsepower + Weight + 
                  company_name_three_grouphigh_mileage_brand_name + 
                 Model_year_Two_GROUP, data = carmileage_train[, ])
# Summary of model 4
summary(model_4)

# Checking VIF value
vif( model_4 )

## model 5 , in previous model "Horsepower" had high VIF above 2 and  low significance 
## and High correlation between Horsepower and Weight
## therefore , Horsepower is removed from model 5
model_5 <-  lm(formula = MPG ~ Cylinders   + Weight + 
                 company_name_three_grouphigh_mileage_brand_name + 
                 Model_year_Two_GROUP, data = carmileage_train[, ])
# Summary of model 3
summary(model_5)

# Checking VIF value
vif( model_5 )

## model 6 , in previous model "Cylinder" had high VIF above 2 and  comparatively low significance 
## than weight
## therefore , Cylinder is removed from model 6
model_6 <- lm(formula = MPG ~  Weight + 
                company_name_three_grouphigh_mileage_brand_name + 
                Model_year_Two_GROUP, data = carmileage_train[, ])

# Summary of model 3
summary(model_6)

# Checking VIF value
vif( model_6 )

# Predicting test data MPG using final model(model 2)
Predict_1 <- predict(model_6 , carmileage_test[,-c(1)])
carmileage_test$predicted_mpg <- Predict_1

# Correlation between predicter test MPG and original test MPG
cor( carmileage_test$MPG , carmileage_test$predicted_mpg )
# R squared value meets business requirement of 80 above
cor( carmileage_test$MPG , carmileage_test$predicted_mpg )^2


plot(model_6)