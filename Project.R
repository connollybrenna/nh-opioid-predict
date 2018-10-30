setwd("~/School/2016/16_Fall/Predictive Analytics I/Project 3")
library(dplyr)
library(magrittr)
library(ggplot2)
library(maps)
library(data.table)
library(lme4)
library(caret)
library(gbm)
library(e1071)
t0 <- proc.time()

###DATA WRANGLING###

#Import the files
df <- read.csv("prescriber-info.csv")
opioids <- read.csv("opioids.csv")

#Remove all entries about opioids (no cheating!)
opioids <- as.character(opioids[,1]) # First column contains the names of the opiates
opioids <- gsub("\ |-",".",opioids) # replace hyphens and spaces with periods to match the dataset
df <- df[, !names(df) %in% opioids]

#Dummy variables for importance values
dummy <- data.frame(model.matrix( ~ County - 1, data = df))
dummy$NPI <- df$NPI
df <- merge(dummy, df, by = "NPI")
df$County <- NULL



#This information will most likely be conveyed just as well with Speciality, so we're dropping it
df$Credentials <- NULL

#Cleaning up Specialty - first get the common ones, which we won't change
common.specialties <- df %>%
  group_by(Specialty) %>%
  dplyr::summarise(specialty.counts = n()) %>%
  arrange(desc(specialty.counts)) %>% 
  filter(specialty.counts > 50) %>%
  select(Specialty)
common.specialties <- levels(droplevels(common.specialties$Specialty))


#The rest will be "other" by default, but we're pulling out surgeons and anyone with the word 'Pain'
new.specialties <- factor(x=rep("other",nrow(df)),levels=c(common.specialties,"Surgeon","other","Pain.Management"))
new.specialties[df$Specialty %in% common.specialties] <- df$Specialty[df$Specialty %in% common.specialties]
new.specialties[grepl("surg",df$Specialty,ignore.case=TRUE)] <- "Surgeon"
new.specialties[grepl("pain",df$Specialty,ignore.case=TRUE)] <- "Pain.Management"
new.specialties <- droplevels(new.specialties)
df$Specialty <- new.specialties

#Like with Counties, let's make dummy variables.
df <- df[!is.na(df$Specialty),]
dummy <- data.frame(model.matrix( ~ Specialty - 1, data = df))
dummy$NPI <- df$NPI
df <- merge(dummy, df, by = "NPI")
df$Specialty <- NULL

###MODEL BUILDING###

#Split the data
train_faction <- 0.8
train_ind <- sample(nrow(df),round(train_faction*nrow(df)))

#Remove nonfeatures, and convert the target label to a non-numeric
df %<>% select(-NPI)
df$Opioid.Prescriber <- as.factor(ifelse(df$Opioid.Prescriber==1,"yes","no"))
train_set <- df[train_ind,]
test_set <- df[-train_ind,]

#5 fold cross validation to optimize params for a boosted tree ensemble. This takes a while, so we're sticking to defaults.
set.seed(42)
objControl <- trainControl(method='cv', number=5, returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE)
model <- train(train_set %>% select(-Opioid.Prescriber),train_set$Opioid.Prescriber, 
               method='gbm', 
               metric = "ROC",
               trControl=objControl)

#Now let's try it on the test data
predictions <- predict(model,test_set%>% select(-Opioid.Prescriber),type="raw")
confusionMatrix(predictions,test_set$Opioid.Prescriber,positive="yes")

#Making a graph of important variables
importance <- as.data.frame(varImp(model)[1])
importance <- cbind(row.names(importance), Importance=importance)
row.names(importance)<-NULL
names(importance) <- c("Feature","Importance")
importance %>% arrange(desc(Importance)) %>%
  mutate(Feature=factor(Feature,levels=as.character(Feature))) %>%
  slice(1:15) %>%
  ggplot() + geom_bar(aes(x=Feature,y=(Importance)),stat="identity",fill="blue") + 
  theme(axis.text.x=element_text(angle=45,vjust = 1,hjust=1),axis.ticks.x = element_blank()) +ylab("Importance") +ggtitle("Feature Importance for Detecting Opioid Prescription")

print(sprintf("Finished in %f seconds",(proc.time()-t0)[3]))