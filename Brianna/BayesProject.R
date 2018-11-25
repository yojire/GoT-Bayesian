# Bayes project
# Reading in all the data files

# SIF data contains book name, page, chapter number, POV (?), character name
# gender, alive, titel, forename, surname, old surname, alias, regnal no (?)
# battles data contains info on battles, attacker, defender, location, region, etc.

install.packages("xlsx")
library(xlsx)
SIF <- read.csv("/Users/briannaking/Documents/Fall 2018 umich/Bayes/Bayes Project/A Song of Ice and Fire Character Spreadsheet.csv")
battles <- read.csv("/Users/briannaking/Documents/Fall 2018 umich/Bayes/Bayes Project/battles.csv")
char_deaths <- read.csv("/Users/briannaking/Documents/Fall 2018 umich/Bayes/Bayes Project/character-deaths.csv")
char_preds <- read.csv("/Users/briannaking/Documents/Fall 2018 umich/Bayes/Bayes Project/character-predictions.csv")
chapters <- read.csv("/Users/briannaking/Documents/Fall 2018 umich/Bayes/Bayes Project/Chapters.csv")
name_freq <- read.csv("/Users/briannaking/Documents/Fall 2018 umich/Bayes/Bayes Project/Name Frequency.csv")
houses <- read.csv("/Users/briannaking/Documents/Fall 2018 umich/Bayes/Bayes Project/Houses.csv")

# examining summary stats of houses data set
# first reformating house data to get rid of blank rows
toDelete <- seq(0, nrow(houses),2)
# toDelete
houses <- houses[-toDelete,]
table(houses$Region)
houses$PropAlive <- NA
for(i in 1:nrow(houses)){
  houses$PropAlive[i] <- houses$Alive[i] / (houses$Alive[i] + houses$Dead[i])
}
summary(houses$PropAlive)

# SIF
table(SIF$Gender)
table(SIF$Alive)

# battles...all the battles occur between the years 298 and 300 (what years was the entire series based on?)
table(battles$attacker_outcome) # usually the attacker wins
table(battles$attacker_1)
summary(battles$attacker_size)
summary(battles$defender_size)
table(battles$major_capture)
table(battles$major_death)
table(battles$region)
table(battles$summer) # more battles happen during the summer

# char_deaths is probably an interesting data set
table(char_deaths$Allegiances) # Night's Watch has a lot of allegiances
# Lannister and Greyjoy also have a good amount of allegiances
table(char_deaths$GoT)
table(char_deaths$CoK)
table(char_deaths$SoS)
table(char_deaths$FfC)
table(char_deaths$DwD)
table(char_deaths$Book.of.Death) # book 3 has the most death with 97 characters dying
table(char_deaths$Gender) # 760 males, 157 females
require(gmodels)
CrossTable(char_deaths$Gender,char_deaths$Book.of.Death)


# Merging char-deaths and preds data
require(readr)
require(data.table)
require(tidyverse)
deaths <- char_deaths
predictions <- char_preds
deaths <- as.data.frame(deaths)
deaths$`Book.of.Death`[157]=2
predictions = data.table(predictions)
deaths = data.table(deaths)
colnames(deaths)[1]="name"
setkey(deaths, name)
setkey(predictions, name)
merged <- merge(deaths, predictions)

SIF2 <- data.table(SIF)
colnames(SIF2)[5]="name"
setkey(SIF2,name)
setkey(merged,name)
merged2 = merge(merged,SIF2)

colnames(SIF)[5]="name"
mergedData <- merge(merged,SIF,by="name")
names(merged)

# Frequentist: logistic regression
# variables to include: gender, nobility status, allegiances
# popularity score, POV?, numDeadRelations, book1-book5?, house
# first creating variable of num books they appear in
merged$totalBooks <- NA
for(i in 1:nrow(merged)){
  merged$totalBooks[i] <- merged$book1[i] + merged$book2[i] + merged$book3[i] + merged$book4[i] + merged$book5[i]
}
model1 <- glm(isAlive ~ Allegiances + Gender + male + totalBooks + Nobility + isMarried +
                numDeadRelations + popularity,data=merged, family="binomial")
summary(model1)
# note: gender and male variables have discrepancies. Ruikun thinks there's erros in both
# also, isNoble and Nobility have discrepancies. Ruikun said isNoble looks better

table(SIF$Gender)
