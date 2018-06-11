---
title: "Don't Get Kicked"
author: "AnthonyChen"
---
# Analysis

### Step 1

First, after clearing the current R environment, all the R libraries such as:

* aod
* caret
* catools
- gdata
- ggplot2
- plyr
- psych
- stringr
- visreg

are installed and loaded in the R environment

```{r, echo=FALSE}
# Load Libraries and Cleanup the R environment
rm(list=ls())  # clear all env variables
library(plyr)
library(stringr)
library(gdata)
library(caret)
library(caTools)
#library(Deducer)
library(ggplot2)
library(psych)  # pairs.panels
library(aod)    #wald.test
library(visreg)
library(e1071)

## Correlation matrix with p-values. 
cor.prob <- function (X, dfr = nrow(X) - 2) {
  R <- cor(X, use="pairwise.complete.obs")
  above <- row(R) < col(R)
  r2 <- R[above]^2
  Fstat <- r2 * dfr/(1 - r2)
  R[above] <- 1 - pf(Fstat, 1, dfr)
  R[row(R) == col(R)] <- NA
  R
}

## Use this to dump the cor.prob output to a 4 column matrix
## with row/column indices, correlation, and p-value.
flattenSquareMatrix <- function(m) {
  if( (class(m) != "matrix") | (nrow(m) != ncol(m))) stop("Must be a square matrix.")
  if(!identical(rownames(m), colnames(m))) stop("Row and column names must be equal.")
  ut <- upper.tri(m)
  data.frame(i = rownames(m)[row(m)[ut]],
             j = rownames(m)[col(m)[ut]],
             cor=t(m)[ut],
             p=m[ut])
}

files.path <- "C://Users//anthchen//Documents//LemonCar//Data//"

source.path <- "C://Users//anthchen//Documents//LemonCar//Analysis//"

source(paste0(source.path, "Logit_Plots.R"))

```

### Step-2

The Car dataset files (both training and test) are downloaded from the Kaggle website, and then loaded in the R environment. Once loaded successfully, its attributes and statistics can be verified in the following steps.

```{r}
# Download the given csv file and load it in a dataframe called kcar
kcars <- read.csv(paste0(files.path, "kcar.csv"), 
                  header = TRUE, sep=",", stringsAsFactors = FALSE)
```
### Step-3

#### Number of observations, columns, and brief description
```{r}
str(kcars) # 34 columns found
```


#### Sample data of each column
```{r}
head(kcars)
```

#### Summary of each column of Cars data
```{r}
summary(kcars)
```

### Step-4 Data Cleansing

#### Remove data elements not useful for analysis
```{r}
# Since VehYear is available, Purchase Date is redundant and can be removed.
kcars$PurchDate <- NULL  
# AUCGUART has 69,564 NULLs, and other 2 levels have very low numbers - 3357 and 62. so remove it.
kcars$AUCGUART <- NULL
# PRIMEUNITE also has more than 69K records with NULL values in it. This can also be removed.
kcars$PRIMEUNIT <- NULL 
```

#### Set Transmission type either as Auto or Manual
```{r}
kcars$Transmission[grep("Manual", kcars$Transmission , ignore.case=FALSE, fixed=FALSE)] <- "MANUAL"
kcars$Transmission[grep("NULL", kcars$Transmission , ignore.case=TRUE, fixed=FALSE)] <- NA
kcars$Transmission <- factor(kcars$Transmission, levels=c("AUTO","MANUAL"))
```

#### Set Wheel Type ID and Wheel TYpe mapping appropriately
```{r}

#Wheel Type ID and Wheel Type are mapped:
# 0 -> NULL; 1 -> Alloy; 2 -> Covers; 3 -> Special
# NULL in wheelTypeID should be set to 0 to ensure it maps to all NULLs in wheelType
kcars$WheelTypeID[grep("NULL", kcars$WheelTypeID , ignore.case=TRUE, fixed=FALSE)] <- 0
kcars$WheelTypeID <- factor(kcars$WheelTypeID, levels=c(0,1,2,3))

#NULL wheelType appears to be Steel type
kcars$WheelType[grep("NULL", kcars$WheelType , ignore.case=FALSE, fixed=FALSE)] <- NA
kcars$WheelType <- as.factor(ifelse(is.na(kcars$WheelType), "Steel", kcars$WheelType))
#kcars$WheelType <- revalue(kcars$WheelType, 
#                           c("1"="Alloy", "2"="Covers", "4" = "Special", "Steel" = "Steel"))
kcars$WheelType <- factor(kcars$WheelType, c("Alloy", "Covers", "Special", "Steel"))
```

#### Set TopThreeAmericanName NULL value to Other
```{r}
kcars$TopThreeAmericanName[grep("NULL", kcars$TopThreeAmericanName , ignore.case=TRUE, fixed=FALSE)] <- "OTHER"
kcars$TopThreeAmericanName <- factor(kcars$TopThreeAmericanName, levels=c("CHRYSLER","FORD", "GM", "OTHER"))
```

#### Fix Null values of Color and Size, and Numeric series of Trim Values
```{r}
kcars$Color[grep("NULL", kcars$Color , ignore.case=TRUE, fixed=FALSE)] <- NA

kcars$Size[grep("NULL", kcars$Size , ignore.case=TRUE, fixed=FALSE)] <- NA

kcars$Trim[grep("NULL", kcars$Trim , ignore.case=TRUE, fixed=FALSE)] <- NA
kcars$Trim[grep("^1", kcars$Trim , ignore.case=TRUE, fixed=FALSE)] <-"1XX"
kcars$Trim[grep("^2", kcars$Trim , ignore.case=TRUE, fixed=FALSE)] <-"2XX"
kcars$Trim[grep("^3", kcars$Trim , ignore.case=TRUE, fixed=FALSE)] <-"3XX"
```

#### Set approrpriate value for NULL Nationality as Other
```{r}
kcars$Nationality[grep("NULL", kcars$Nationality , ignore.case=TRUE, fixed=FALSE)] <- "OTHER"
kcars$Nationality <- factor(kcars$Nationality, levels=c("AMERICAN","OTHER", "OTHER ASIAN", "TOP LINE ASIAN"))
```

##### Set all other field values
```{r}
kcars$MMRCurrentAuctionAveragePrice[grep("NULL", kcars$MMRCurrentAuctionAveragePrice , ignore.case=TRUE, fixed=FALSE)] <- NA

kcars$MMRCurrentAuctionCleanPrice[grep("NULL", kcars$MMRCurrentAuctionCleanPrice , ignore.case=TRUE, fixed=FALSE)] <- NA

kcars$MMRCurrentRetailAveragePrice[grep("NULL", kcars$MMRCurrentRetailAveragePrice , ignore.case=TRUE, fixed=FALSE)] <- NA

kcars$MMRCurrentRetailCleanPrice[grep("NULL", kcars$MMRCurrentRetailCleanPrice , ignore.case=TRUE, fixed=FALSE)] <- NA

kcars$Cyl <- "V4"
kcars$Cyl[grep("V6", kcars$Model , ignore.case=TRUE, fixed=FALSE)] <- "V6"
kcars$Cyl[grep("V8", kcars$Model , ignore.case=TRUE, fixed=FALSE)] <- "V8"

kcars$PowerTrain <- "2WD"
kcars$PowerTrain[grep("4WD", kcars$Model , ignore.case=TRUE, fixed=FALSE)] <- "4WD"
kcars$PowerTrain[grep("FWD", kcars$Model , ignore.case=TRUE, fixed=FALSE)] <- "4WD"
kcars$PowerTrain[grep("AWD", kcars$Model , ignore.case=TRUE, fixed=FALSE)] <- "AWD"

kcars$Doors <- "4D"
kcars$Doors[grep("2D", kcars$SubModel , ignore.case=TRUE, fixed=FALSE)] <- "2D"
kcars$Doors[grep("3D", kcars$SubModel , ignore.case=TRUE, fixed=FALSE)] <- "3D"
kcars$Doors[grep("5D", kcars$SubModel , ignore.case=TRUE, fixed=FALSE)] <- "5D"

kcars$SubModelType <- "SEDAN"
kcars$SubModelType[grep("CAB", kcars$SubModel , ignore.case=TRUE, fixed=FALSE)] <- "CAB"
kcars$SubModelType[grep("CUV", kcars$SubModel , ignore.case=TRUE, fixed=FALSE)] <- "CUV"
kcars$SubModelType[grep("MINIVAN", kcars$SubModel , ignore.case=TRUE, fixed=FALSE)] <- "MINIVAN"
kcars$SubModelType[grep("UTILITY", kcars$SubModel , ignore.case=TRUE, fixed=FALSE)] <- "UTILITY"
kcars$SubModelType[grep("SPORT", kcars$SubModel , ignore.case=TRUE, fixed=FALSE)] <- "SPORT"
kcars$SubModelType[grep("PASSENGER", kcars$SubModel , ignore.case=TRUE, fixed=FALSE)] <- "PASSENGER"
kcars$SubModelType[grep("SUV", kcars$SubModel , ignore.case=TRUE, fixed=FALSE)] <- "SUV"
kcars$SubModelType[grep("WAGON", kcars$SubModel , ignore.case=TRUE, fixed=FALSE)] <- "WAGON"
kcars$SubModelType[grep("CONVERTIBLE", kcars$SubModel , ignore.case=TRUE, fixed=FALSE)] <- "CONVERTIBLE"
kcars$SubModelType[grep("HATCHBACK", kcars$SubModel , ignore.case=TRUE, fixed=FALSE)] <- "HATCHBACK"

kcars$SubModel <- NULL

kcars$Make[grep("TOYOTA SCION", kcars$Make , ignore.case=TRUE, fixed=FALSE)] <-"SCION"
kcars$Make[grep("HUMMER", kcars$Make , ignore.case=TRUE, fixed=FALSE)] <-"CADILLAC"
kcars$Make[grep("CADILLAC", kcars$Make , ignore.case=TRUE, fixed=FALSE)] <-"CADI-HUM"
kcars$Make[grep("PLYMOUTH", kcars$Make , ignore.case=TRUE, fixed=FALSE)] <-"CHRYSLER"

kcars$Model <- NULL

kcars$Auction <- as.factor(kcars$Auction)
kcars$Make <- as.factor(kcars$Make)
kcars$Trim <- as.factor(kcars$Trim)
kcars$Color <- as.factor(kcars$Color)
kcars$Size <- as.factor(kcars$Size)
kcars$Cyl <- as.factor(kcars$Cyl)
kcars$PowerTrain <- as.factor(kcars$PowerTrain)
kcars$Doors <- as.factor(kcars$Doors)
kcars$SubModelType <- as.factor(kcars$SubModelType)
kcars$VNST <- as.factor(kcars$VNST)
kcars$MMRAcquisitionAuctionAveragePrice <- as.integer(kcars$MMRAcquisitionAuctionAveragePrice)
kcars$MMRAcquisitionAuctionCleanPrice <- as.integer(kcars$MMRAcquisitionAuctionCleanPrice)
kcars$MMRAcquisitionRetailAveragePrice <- as.integer(kcars$MMRAcquisitionRetailAveragePrice)
kcars$MMRAcquisitonRetailCleanPrice <- as.integer(kcars$MMRAcquisitonRetailCleanPrice)
kcars$MMRCurrentAuctionAveragePrice <- as.integer(kcars$MMRCurrentAuctionAveragePrice)
kcars$MMRCurrentAuctionCleanPrice <- as.integer(kcars$MMRCurrentAuctionCleanPrice)
kcars$MMRCurrentRetailAveragePrice <- as.integer(kcars$MMRCurrentRetailAveragePrice) 
kcars$MMRCurrentRetailCleanPrice <- as.integer(kcars$MMRCurrentRetailCleanPrice)
```

#### Run a quick summary of kcars
```{r}
summary(kcars)
```

### Step-5 Feature Engineering

#### Use only a subset of fields
```{r}
# VNST, Trim, WheelType, Make, NewModel left out
# Transmission, Cyl, Doors, WarrantyCost, MMRAcquisitionAuctionAveragePrice,
# MMRAcquisitionAuctionCleanPrice, MMRAcquisitonRetailCleanPrice,
# MMRCurrentAuctionAveragePrice, MMRCurrentRetailAveragePrice 
# found not significant in the model
useColumns <- c("IsBadBuy","Auction","VehYear","VehicleAge","Color",
                "WheelTypeID","VehOdo","Nationality","Size",
                "BYRNO","VNZIP1","VehBCost","IsOnlineSale",
                "MMRAcquisitionRetailAveragePrice",
                "MMRCurrentAuctionCleanPrice",
                "MMRCurrentRetailCleanPrice",
                "PowerTrain","SubModelType") 
```

#### Use One-Hot Encoding technique for categorical variables
```{r}
dmy <- dummyVars(" ~ .", data = kcars[,(useColumns)])
kcarsTrans <- data.frame(predict(dmy, newdata = kcars[,(useColumns)]))
str(kcarsTrans)
```

### Step-6 Data Visualization

#### List the fields in the order of their correlation
```{r}
corList <- flattenSquareMatrix(cor.prob(kcarsTrans))
corOrdList <- corList[order(-abs(corList$cor)),]
head(corOrdList)
```

#### Only list the fields that are correlated to IsBadBuy by more than 4%
```{r}
corOrdList[corOrdList$j=='IsBadBuy',]
selectedSub <- subset(corOrdList, (abs(cor)>0.04 & corOrdList$i == 'IsBadBuy'))
selectedSub
```
Plots, Aggregation, and Correlation Chart were used to get more insight on the data. An example of a correlation plot is given below.


#### Correlation Chart

The correlation chart of response variable IsBadBuy in correlation with other variables in the data (top six variables)

```{r}
bestSub <- as.character(selectedSub$j[c(1,2,3,5,6,7)])
bestSub
pairs.panels(kcarsTrans[c(bestSub,'IsBadBuy')])
```

#### Plots of Vehicle Age and Vehicle Year:

Red lines indicate total number of vehicles per vehicle age, and the following blue line indicate that IsBadBuy = 0 indicating that they are marked as good. These charts display both the range of data, and the distribution of the response classes.

```{r}
par(mfrow=c(2,1))
total <- ddply(kcars, c("VehicleAge"), summarise, N= length(RefId))
isGood <- ddply(kcars, c("VehicleAge"), summarise, N= sum(!IsBadBuy))
plot(total, type="l", col="red", xlab="Vehicle Age", ylab="Total", cex.lab=0.8, lwd=2,  xlim=c(0, 10), ylim=c(0, 20000))
par(new = T)
plot(isGood, type="l", col="blue", xlab='', ylab='', xlim=c(0, 10), (ylim=c(0, 20000)), axes=F)

```

```{r}
total <- ddply(kcars, c("VehYear"), summarise, N= length(RefId))
isGood <- ddply(kcars, c("VehYear"), summarise, N= sum(!IsBadBuy))
plot(total, type="l", col="red", xlab="Vehicle Year", ylab="Total", cex.lab=0.8, lwd=2,  xlim=c(2000, 2012), ylim=c(0, 20000))
par(new = T)
plot(isGood, type="l", col="blue", xlab='', ylab='', xlim=c(2000, 2012), (ylim=c(0, 20000)), axes=F)
```

```{r}
#total <- ddply(kcars, c("Size"), summarise, N= length(RefId))
#isGood <- ddply(kcars, c("Size"), summarise, N= sum(!IsBadBuy))
#total$Size <- factor(total$Size, labels=c(1,2,3,4,5,6,7,8,9,10,11,12,13))
#plot(total, type="l", col="red", xlab="Size", ylab="Total", xlim=c(1, 14), ylim=c(1, 32000))
#par(new = T)
#plot(isGood, type="l", col="blue", xlab='', ylab='', xlim=c(1, 14), ylim=c(1, 32000), axes=F)
```

```{r}
total <- ddply(kcars, c("IsOnlineSale"), summarise, N= length(RefId))
isGood <- ddply(kcars, c("IsOnlineSale"), summarise, N= sum(!IsBadBuy))
plot(total, type="l", col="red", xlab="Online Sale", ylab="Total", cex.lab=0.8, lwd=2,  xlim=c(0, 1), ylim=c(1000, 75000))
par(new = T)
plot(isGood, type="l", col="blue", xlab='', ylab='', xlim=c(0, 1), (ylim=c(1, 75000)), axes=F)
```


#### Tabular Results
```{r}
xtabs(~IsBadBuy + Size, data = kcars[complete.cases(kcars),(useColumns)])
xtabs(~IsBadBuy + Color, data = kcars[complete.cases(kcars),(useColumns)])
xtabs(~IsBadBuy + PowerTrain, data = kcars[complete.cases(kcars),(useColumns)])
xtabs(~IsBadBuy + SubModelType, data = kcars[complete.cases(kcars),(useColumns)])
xtabs(~IsBadBuy + Nationality, data = kcars[complete.cases(kcars),(useColumns)])

```

### Step-7 Modeling

#### Split into Training/Test Data Sets
```{r, echo = FALSE}
#Total records to be dropped due to NA
nrow(kcars)-sum(complete.cases(kcars))

# Split dataset into training and test sets
set.seed(123) 

Y = kcars[,2] # extract "IsBadBuy" labels from the data
msk = sample.split(Y, SplitRatio=7/10)
table(Y,msk)
t=sum( msk)  # number of elements in one class
f=sum(!msk)  # number of elements in the other class
#stopifnot( round((t+f)*4/10) == t ) # test ratios

# test results
U = unique(Y)       # extract all unique labels
for( i in 1:length(U)) {  # check for all labels
  lab = (Y==U[i])   # mask elements that have label U[i]
  t=sum( msk[lab])  # number of elements with label U[i] in one class
  f=sum(!msk[lab])  # number of elements with label U[i] in the other class 
  print(paste( "Label",U[i],"numbers: total=",t+f,", train=",t,", test=",f, 
               ", ratio=", t/(t+f) ) )
}

# use results
train = kcars[ msk,]  # use output of sample.split to ...
test  = kcars[!msk,]  # create train and test subsets
```

```{r}
str(train[complete.cases(train),(useColumns)])
```

#### Build Logistic Regression Model and Print Summary
```{r}
lg <- glm(formula = IsBadBuy ~ ., data = train[complete.cases(train),(useColumns)], 
          family=binomial(logit))

summary(lg)
```

#### Run ANOVA Analysis on the Model
```{r}
anova(lg)
```

#### Confidence Interval, Degrees of Freedom, and p-value
```{r}
# Confidence Interval
# To find the difference in deviance between the current and null model (i.e., the test statistic)
with(lg, null.deviance - deviance)
```

```{r}
#  The degrees of freedom for the difference between the two models is equal to the 
#  number of predictor variables in the mode, and can be obtained using ...
with(lg, df.null - df.residual)
```

```{r}
#Finally, the p-value can be obtained using: ~0
with(lg, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

# The chi-square of 6299.449 with 56 degrees of freedom and an associated 
# p-value of less than 0.001 (0) tells us that our model as a whole fits 
# significantly better than an empty model.
```

#### Drop single term from the model; result to include a test statistic also

```{r}
drop1(lg, test = "Chisq")
```

#### Logistic Regression Charts

Nonlinearity can be detected in a plot of the residuals versus the predicted/fitted
values. As we build and test our models the residuals indicate how well we predict the observed data.

In determining the presence of linearity the points should be symmetrically distributed about the
dotted horizontal line. If nonlinearity is present you should add a non-linear transformation to your
independent variable to your model (for example consider using (age)2 along with age, or try to add
another independent variable. A large deviation from the horizontal line indicates that the linear model
does not fit the data well and you should consider adding another predictor. In the series of
assumption plots below, the upper left plot indicates that model 6 is a sufficient linear model. When
compared to model 1, it is a significant improvement (not shown).

The next important assumption that we must examine is the normality of the deviance residuals (i.e.,
the residual in our models). This assumption, though very robust, is important to consider, because
non-normality would mean that the significance statistics are biased. Consequently, the validity
results would be questionable. We can test this assumption using a normal quantile-quantile plot (qqplot).
As depicted in the upper right quadrant of the series of graphs below, most of the data do fall closely to the line representing a normal distribution hence the data is consistent with a normal
distribution.

In logistic regression testing the homogeneity of variance assumption is not required because based
on the nature of the dependent variable we have a non-normal error distribution that is
heteroskedastic. For continuous outcomes focus your attention on the graph in the lower left corner.

Another concern is the presence of "Outliers". The presence of outliers can result in one of three
things: The standard method for detecting these abnormalities is the Cook's Distance (cooksD) statistic
which incorporates the Lever and the studentized deleted residual. The cooksD vs. Leverage plot
identifies these influential points so that we may test our model with or without them. Our expectation
is a straight line that follows the dashed lines on the graph. In the graph the labeled points, if any, are
those with the largest Cook's distances. The lower right graph amongst the figures below indicates
that there are at least two data points with high cooksD values

```{r}
op <- par(mfrow=c(3,2))
op <- par(lwd=2,cex.main=1.3,cex.lab=1.3,cex.axis=1.3)
plot(lg)
par(op)
```

```{r}
logit.plot.quad.lowPos(lg)
```

```{r}
logit.roc.plot(logit.roc(lg))
```

```{r}
logit.plot.ss(logit.roc(lg))
```

```{r}
visreg(lg)
```

```{r}
visreg(lg, "IsBadBuy", "Size", cond=list(Color = "WHITE"))
```

#### Prediction Performance with Test Dataset
```{r}

p1 <- predict(lg, test[complete.cases(test),(useColumns)], type='response', se.fit = FALSE)


# create a new reference to the cars test data
testing<-test[complete.cases(test),(useColumns)]
# if p > 0.5, we're predicting the car is a bad buy
testing$predicted.IsBadBuy <- 0
testing[p1>0.5,]$predicted.IsBadBuy <- 1
# the table command allows us to see how ofter our predictions were accurate.
conf <- confusionMatrix(table(testing$IsBadBuy,testing$predicted.IsBadBuy))
print(conf)
```
