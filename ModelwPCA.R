library(caret)
library(car)
library(pROC)
library(Hmisc)
setwd("/Users/bdign/Documents/R/AIR")



listed1 <- read.csv("ModelData4Demand.csv", stringsAsFactors = F, na.strings = c("")) 
listings2 <- read.csv("ModelData4Supply.csv", stringsAsFactors = F, na.strings = c(""))
listed1 [c(1)] <- list(NULL)
listings2 [c(1)] <- list(NULL)

listings2$host_is_superhost   <- as.factor(listings2$host_is_superhost)
listings2$neighbourhood_group_cleansed   <- as.factor(listings2$neighbourhood_group_cleansed)
listings2$price  <- as.numeric(gsub('[$,]', '', listings2$price))
listings2$cleaning_fee <- as.numeric(gsub('[$,]', '', listings2$cleaning_fee))
listings2$host_response_rate <- as.numeric(gsub('[%,]', '', listings2$host_response_rate))
listings2$host_acceptance_rate <- as.numeric(gsub('[%,]', '', listings2$host_acceptance_rate))
listings2$room_type <-as.factor(listings2$room_type)
listings2$host_identity_verified <-as.factor(listings2$host_identity_verified)
listings2$ForRent <-as.factor(listings2$ForRent)

dfTeam1_num <- listings2[,c(9:12)]
pca <- prcomp(dfTeam1_num, center=TRUE, scale=TRUE)
pred_pca <- predict(pca, newdata = dfTeam1_num)
listings2 <- cbind.data.frame(listings2[,c(1:8,13:21)], pred_pca[,1])
names(listings2)[18] <- "RentalCharPC"

listed1$host_is_superhost   <- as.factor(listed1$host_is_superhost)
listed1$neighbourhood_group_cleansed   <- as.factor(listed1$neighbourhood_group_cleansed)
listed1$price  <- as.numeric(gsub('[$,]', '', listed1$price))
listed1$cleaning_fee <- as.numeric(gsub('[$,]', '', listed1$cleaning_fee))
listed1$host_response_rate <- as.numeric(gsub('[%,]', '', listed1$host_response_rate))
listed1$host_acceptance_rate <- as.numeric(gsub('[%,]', '', listed1$host_acceptance_rate))
listed1$room_type <-as.factor(listed1$room_type)
listed1$host_identity_verified <-as.factor(listed1$host_identity_verified)
listed1$Post_20_reviews <-as.factor(listed1$Post_20_reviews)

dfTeam1_num1 <- listed1[,c(9:12)]
pca1 <- prcomp(dfTeam1_num1, center=TRUE, scale=TRUE)
pred_pca1 <- predict(pca1, newdata = dfTeam1_num1)
listed1 <- cbind.data.frame(listed1[,c(1:8, 13:21)], pred_pca1[,1])
names(listed1)[18] <- "RentalCharPC"

####Demand Model####
vif(glm(formula=Post_20_reviews~.,family = binomial(link='logit'), data=listed1))

set.seed(101)                      
trainIndex <- createDataPartition(listed1$Post_20_reviews,
                                  p=0.7,
                                  list=FALSE,
                                  times=1)



# Create Training Data
Team1df.train <- listed1[trainIndex,]

# Create Validation Data
Team1df.valid <-listed1[-trainIndex,]

basedline.model.demand <- train(Post_20_reviews~.,
                                data=Team1df.train,
                                method='glm',
                                family='binomial',
                                na.action=na.pass)


summary(basedline.model.demand)
predictionTeam1 <- predict(basedline.model.demand,newdata=Team1df.valid)



confusionMatrix(predictionTeam1,Team1df.valid$Post_20_reviews)

#Criteria 2: the ROC curve and area under the curve
pred.probabilities <- predict(basedline.model.demand,newdata=Team1df.valid,type='prob')

regressionTeam1.ROC <- roc(predictor=pred.probabilities$`1`,
                           response=Team1df.valid$Post_20_reviews,
                           levels=levels(Team1df.valid$Post_20_reviews))
plot(regressionTeam1.ROC)
regressionTeam1.ROC$auc

###SupplyModel###

vif(glm(formula=ForRent~.,family = binomial(link='logit'), data=listings2))

set.seed(101)                      
trainIndex2 <- createDataPartition(listings2$ForRent,
                                   p=0.7,
                                   list=FALSE,
                                   times=1)



# Create Training Data
Team1df.train2 <- listings2[trainIndex2,]

# Create Validation Data
Team1df.valid2 <-listings2[-trainIndex2,]

basedline.model.supply <- train(ForRent~.,
                                data=Team1df.train2,
                                method='glm',
                                family='binomial',
                                na.action=na.pass)


summary(basedline.model.supply)
prediction2Team1 <- predict(basedline.model.supply,newdata=Team1df.valid2)
confusionMatrix(prediction2Team1,Team1df.valid2$ForRent)

#Criteria 2: the ROC curve and area under the curve
pred.probabilities1 <- predict(basedline.model.supply,newdata=Team1df.valid2,type='prob')

regressionTeam12.ROC <- roc(predictor=pred.probabilities1$`1`,
                            response=Team1df.valid2$ForRent,
                            levels=levels(Team1df.valid2$ForRent))
plot(regressionTeam12.ROC)
regressionTeam12.ROC$auc
