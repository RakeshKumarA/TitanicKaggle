## Load Libraries
library(tidyverse)
library(rebus)
library(mice)
library(mlr)
library(randomForest)

## Read the files
train <- read_csv("train.csv")  ## training file
test <- read_csv("test.csv") ## testing file

full <- bind_rows(train,test)  ## combine test and train

str(full)

##More appropriate column names

#Variable Name	                Description
#Survived	                      Survived (1) or died (0)
#Pclass	                        Passenger’s class
#Name	                          Passenger’s name
#Sex	                          Passenger’s sex
#Age	                          Passenger’s age
#SibSp	                        Number of siblings/spouses aboard
#Parch	                        Number of parents/children aboard
#Ticket	                        Ticket number
#Fare	                          Fare
#Cabin	                        Cabin
#Embarked	                      Port of embarkation

mlr::summarizeColumns(full)  ## Summary

## Feature Engg with Names

pat <- ", " %R% one_or_more(char_class(ASCII_ALNUM)) %R% "."
pat1 <- or(", ", DOT)

full <- full %>% 
  mutate(title = str_replace_all(string = str_extract(string = full$Name, pattern = pat), 
                                 pattern = pat1, replacement = ""))    ### extract title


table(full$Sex, full$title)  ## check the sex count by title

rare_title <- c('Dona', 'Lady', 'the ','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')  ## rare titles

full[full$title == 'Mlle',]$title <- 'Miss'
full[full$title == 'Ms',]$title <- 'Miss'
full[full$title == 'Mme',]$title <- 'Mrs'
full[full$title %in% rare_title,]$title  <- 'Rare Title'

table(full$Sex, full$title) ## check the count after transformation

## grab the surname

full <- full %>% mutate(surname = str_replace(string = str_extract(string = full$Name, 
                                                                   pattern = START %R% one_or_more(char_class(GRAPH,SPACE)) %R% "," %R% SPACE), 
                                              pattern = "," %R% SPACE, replacement = ""))


dim(full %>% distinct(surname))  ## 875 unique Surnames

## full family size

full <- full %>% mutate(Fsize = SibSp + Parch + 1)

full <- full %>% mutate(Family = str_c(surname,Fsize,sep = "_"))

## family sink or survive together

ggplot(data = full[1:891,], aes(x=Fsize)) + geom_histogram(aes(fill = factor(Survived)), position = 'dodge')

## Looks like Small families have better chance to survive

full <- full %>% mutate(FsizeD = if_else(Fsize == 1, 'Single', 
                                         if_else(Fsize > 4, 'Large','Small')))

## Lot of NA in Cabin column. Extracting the deck from Cabin column

full <- full %>% mutate(Deck = str_sub(string = Cabin, start = 1, end = 1))
full$Deck <- as.factor(full$Deck)

which(is.na(full$Embarked))

ggplot(drop_na(full), aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() + geom_hline(yintercept = 80)  ## Seems like they Embarked from 'C' 

full[is.na(full$Embarked),]$Embarked <- 'C'  ## Assigning 'C' to NA values

which(is.na(full$Fare))

as.data.frame(full[1044, ])

ggplot(full %>% filter(Pclass == 3, Embarked == 'S'), aes(x=Fare)) + geom_density()  ## Looks like we have to take the median

full[1044, ]$Fare <- median(full[full$Pclass == 3 & full$Embarked == 'S',]$Fare, na.rm = TRUE)

## Age imputation

sum(is.na(full$Age))

colnames(full)

factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                 'title','surname','Family','FsizeD')


full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))

# Set a random seed
set.seed(129)

mice_mod <- mice(data = full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin','Family','surname','Survived')], method = 'rf')

mice_output <- complete(mice_mod)
mice_output

par(mfrow=c(1,2))

hist(full$Age, freq=F, main='Age: Original Data', 
     col='darkgreen', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE Output', 
     col='lightgreen', ylim=c(0,0.04))

full$Age <- mice_output$Age

sum(is.na(full$Age))

## Another feature engineering

full %>% filter(Sex=='female')

full <- full %>% mutate(child = if_else(Age>18,'Adult','Child'))

table(full$Survived, full$child)

full <- full %>% mutate(mother = if_else(Sex == 'female' & Age > 18 & Parch > 0 & title != 'Miss', 'Mother','Not Mother'))


table(full$mother, full$Survived)

full$child <- as.factor(full$child)
full$mother <- as.factor(full$mother)

md.pattern(full)    


## Prediction

train <- full[1:891,]
test <- full[892:1309,]

set.seed(754)

colnames(train)

rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + 
                           Fare + Embarked + title + 
                           FsizeD + child + mother,
                         data = train)

rf_model

plot(rf_model, ylim=c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)

# Get importance
importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))
varImportance <- varImportance %>% arrange(desc(Importance))

## Prediction

prediction <- predict(rf_model,test)

table(prediction, 
