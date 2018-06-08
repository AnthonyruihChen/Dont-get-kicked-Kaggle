# Dont-get-kicked-Kaggle
Kaggle Competition - Classification model to predict if a car purchased at auction is a lemon

# Background
This project analyzes few thousand rows of the Car training dataset provided during the [Kaggle  Competition - Don't Get Kicked](https://www.kaggle.com/c/DontGetKicked). The main task of this competition was to predict if a car purchased at auction is a lemon.
As per the description given on the competition website - "<I>One of the biggest challenges of an auto dealership purchasing a used car at an auto auction is the risk of that the vehicle might have serious issues that prevent it from being sold to customers. The auto community calls these unfortunate purchases "kicks".
Kicked cars often result when there are tampered odometers, mechanical issues the dealer is not able to address, issues with getting the vehicle title from the seller, or some other unforeseen problems. Kicked cars can be very costly to dealers after transportation cost, throw-away repair work, and market losses in reselling the vehicle.
Data Scientists who can figure out which cars have a higher risk of being kick can provide real value to dealerships trying to provide the best inventory selection possible to their customers.
The challenge of this competition is to predict if the car purchased at the Auction is a Kick (bad buy).</I>"

# Executive Summary
Unlike linear regression, a logistic regression is used for binary outcomes, and it provides prediction probabilities (odds ratio) rather than actual predicted values. An odds ratio is the ratio of probability of success and probability of failure. Based on the value of this odds ratio obtained, and a cutoff value used between 0 and 1, a test dataset can be predicted with it being a good or bad buy.
For model training and evaluation purposes, a hold-out method was used. The available dataset was divided into train dataset and test dataset in the proportion of 70% and 30% respectively. Both classes (IsBadBay and Not IsBadBuy) were equally distributed amongst all these three datasets. Prior to this division and distribution, the dataset was cleaned, and new fields were created based upon compound data available in Make and Model fields. 
After detailed data analysis, a logistic regression model was trained using 16 original fields and 1 newly created field. The model success rate obtained is as follows: - (a) Accuracy: 89.55%; (b) Sensitivity: 90.27%; (c) Specificity: 72.85%; and (d) Fallout: 26.15%. This model with around 90% accuracy and 10% error rate is acceptable considering the datum; it can be considered deployment-ready.

Logistic Regression Model Training:
useColumns <- c("IsBadBuy","Auction","VehYear","VehicleAge","Color", "WheelTypeID","VehOdo",
                	"Nationality","Size", "BYRNO","VNZIP1","VehBCost","IsOnlineSale",
                	"MMRAcquisitionRetailAveragePrice",  "MMRCurrentAuctionCleanPrice",
               	"MMRCurrentRetailCleanPrice",  "PowerTrain","SubModelType")               	
 
gg <- glm(formula = IsBadBuy ~ ., data = train[complete.cases(train),(useColumns)], 
          family=binomial(logit))

A future recommendation is to improve the fall-out rate, which is the false-positive rate (type II error). This can be tried by plotting true negatives and false negatives on a ROC curve, and trying to attain a good tradeoff point between fall-out rate, sensitivity, specificity, and accuracy.  Technically, it may mean choosing a threshold value other than 0.5 (in this case, it may be reducing this threshold). 
