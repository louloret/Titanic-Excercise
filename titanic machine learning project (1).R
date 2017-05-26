library(ggplot2)

#load raw data
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

#add a survived variable to the test set to 
#allow for combination of datasets

test.survived <- data.frame(Survived = rep("None", nrow(test)), 
                            test[,])
## 1. using dataframe function allows for creation o fnew dataframes
## 2. rep function, allows us to replicate the value of none which 
## we set to repeat the number times by the number of rows in the test set
## which came back as 418, and assign to survive variable
## and then 
## 3. and then i want to you to combine that variable with the test variable
## this syntax deserves some explanation

## ^ dataframes can be indexed using this kinda syntax between square brackets
## you went over this in datacamp >> first row, 4th column = test[1,4]
## if you leave brackets blank it tells R to use all rows and all columns
## ex. ===> test[,]

## basically code is saying take the entire data frame of test and
## and combine it with a list of 418 strings of none and 
## return it all back with a new dataframe with the 418 nones under
## survived variable


# now lets combine train and test data frames since they have the same dimensions

## we want to rbind, aka bind by rows, this tells R to append test to train
## row by row

data.combined <- rbind(train, test.survived)
?rbind

## survived once capitalized in one data set and not the other
## lol

## reorder survive column in test to be able to bind

#test.survived_fix <- test.survived[c(2,1,3,4,5,6,7,8,9,10,11,12)]

#data.combined <- cbind(train, test.survived_fix, check.names = FALSE)

# a bit about R data types (e.g., factors)
str(data.combined)
#Name is a factor
#what is a factor?
?factor
#factors are categorical type variables
# encoding data in discrete sense
# sex is also a factor and this makes sense
# NA denotes absence of value, equivalent to null
# i dont think name should be a factor tho...
# Pclass is class of ticket, where you richer or poorer
## this is read in as integer, but should be a factor

data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)
# we also turned survive into factor,
# machine learning algos do not like character strings
str(data.combined)

#lets look at survival rates in aggregate

table(data.combined$Survived)
#data is skewed, more people perished than survived
# machine learning algos fit data fed in, 
# will predict skewness towards perishing (most common scenario)

#be cognizant of skewness in data

# remember we want to make a machine learning model based on predicting
#whether passenger survives titanic or not based on
# certain attributes

#next lext look at distribution across classes
table(data.combined$Pclass)
#a lot more people in third class than in first class
# expected... think about flights

#let us visualize
library(ggplot2)
library(plyr)
#Hypothesis <- Rich folks survived at a higher rate
train$Pclass <- as.factor(train$Pclass)

str(train)

train$Count_class <- count(train$Pclass)
ggplot(train, aes(x=Pclass, fill = factor(Survived))) +
  geom_bar(width = 0.5) +
  xlab("Pclass") + 
  ylab("Total Count") +
  labs(fill ="Survived")

#pclass is important in determining whether someone survived or not on the titanic 


# first thing we pass through ggplot is the dataset we
# want to plot, then the next function is based on
# aesthetics aka aes, this means we control way
# plot looks, we say aesthetic will have an x axis of 
# Pclass from train data set, and color code plot based
# on the Survived variable using the fill = option
# which we convert to a factor on the fly by using a
# factor function
# plot out histogram with width of 0.5
# then specify your x and y lable and fill

#next we look at name variable, we are doing descriptive stats on every variable

str(train)
#name is labeled a factor variable

head(as.character(train$Name))
#as.character tells R to just give back string of names and not think of names as factor
# head command allows us to just see the first few of the top

#how many unique names in test and train?
length(unique(as.character(data.combined$Name)))
#unique function extracts unique elements from vector, df or array
#length funciton gives us length of object
#this grabs names for data.combined, convert to character string, find out how many
# are unique and then tell me total unique

#we have 1307 unique names but we have 1309 rows, we expect two duplicates in names

#lets take a closer look at potential dups
#lets determine whether dups are legitamite or do we have bad data in datasets,
# duplicate records inaccurately reported


dup.names <- as.character(data.combined[which(duplicated
                                              (as.character(data.combined$Name))), "Name"])
#gets names from data combined, converts to character string, then invoke duplicated function
# on these list of strings, will go through strings and determine which elements are duplicates
#which function ==>> basically a where clause

#we indeed get two distinct names that are duplicated

# we want to look at duplicates and determine whether to take them out or not 

data.combined[which(data.combined$Name %in% dup.names),]
#grab rows in data.combined where Name is in dup.names and no numbers in index just
# a comma , so R will return all of data hence the comma 

# we see the duplicates are legitamate by looking at variation in other variables, 
# these are distinct people and not just duplicates of the same record

#within the names there are titles , which seem to be pretty important
# maybe there is predictive power within titles: "Ms., Mr. Dr, "

install.packages("stringr")
library(stringr)
misses <- data.combined[which(str_detect(data.combined$Name
                                         , "Miss.")),]

#str_detect function detects pattern in string , in this case it grabs
# every name in data.combined and detect record where "Miss." is  in name

#range of 1 to 5 rows of observation for all columns 
misses[1:5,]
#out of 5 records, 4 of them survived, 80% 
# 4 out of the 5 misses were also in first class 
# a lot of variance in age

#miss denotes a non married women, generally speaking 
#tend to be younger in age 

mrses<-data.combined[which(str_detect(data.combined$Name, "Mrs.")),]
mrses[1:5,]

#check out males to see if pattern continues
males <- data.combined[which(train$Sex=="male"),]
males[1:5,]

#titles seem to be pretty interesting 
#expand on relationship between survival and Pclass by creating new variable based
# on title


extractTitle <- function(Name){
  name <- as.character(Name)
  if (length(grep("Miss.", Name)) > 0) {
  return("Miss.")
  } else if (length(grep("Master.", Name)) > 0) {
  return("Master.")
  } else if (length(grep("Mrs.", Name)) > 0 ) {
  return("Mrs.")
  } else if (length(grep("Mr.", Name)) > 0){
  return("Mr.")
  }else {
  ("Other")
  }
}

str(extractTitle)

#grep function is a pattern matching function, if you recognize Miss within Name
# return Name witihn the string, if you find anything else return other

titles <- NULL
for (i in 1:nrow(data.combined)) {
  titles <-c(titles, extractTitle(data.combined[i, "Name"]))
}

#combining values into titles using c function
#creating new variable Title, using titles list from loop and converting
# to factor

data.combined$Title <- as.factor(titles)

#now do visualization on train data
# first 891 rows
ggplot(data.combined[1:891,], aes(x=Title, fill = Survived)) +
  geom_bar(width = 0.5) +
  facet_wrap(~Pclass) + #group by statement within ggplot
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")

##getting started on data science R part 2

#what where the porportions of sex
table(data.combined$Sex)

#visualize this to see pop
ggplot(data.combined[1:891,], aes(x=Sex, fill = Survived)) +
  geom_bar(width = 0.5) +
  facet_wrap(~Pclass) + #group by statement within ggplot
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")

#lets look at age now, keep in mind age is a continuous variable
summary(data.combined$Age)
#alarming 263 missing obs as NA
#missing values are a big deal and handling missing values are difficult
# we can try to infer value, by replacing missing value with median or mean 
# this makes david nervous

#imputation is an option, create predictive model for missing data
#academic research shows imputation works pretty well, particularly when 
#using k means clustering 

#you can also find a proxy for missing data
# in this case we know there is a strong correlation between title 
# and age
 
# lets look at how bad this problem really is

summary(data.combined[1:891, "Age"])
# most of our missing values are in training data, this is even worse

#now let us plot this problem 

ggplot(data.combined[1:891,], aes(x=Age, fill = Survived)) +
  geom_bar(width = 10) +
  facet_wrap(~Sex + Pclass) + #group by statement within ggplot
  ggtitle("Age by sex and Pclass") +
  xlab("Age") +
  ylab("Total Count") +
  labs(fill = "Survived")

#lets take closer look at master title to
#get better feel for data

boys <- data.combined[which(data.combined$Title
                            == "Master."),]
#remember which is same as where clause in sql
#grabs all indices where title == Master
summary(boys$Age)

#now lets repeat process for misses

misses <- data.combined[which(data.combined$Title == "Miss."),]
print(misses) #misses contains row of info for that obs where Title = "Miss." 
summary(misses$Age)
#this confirms our suspicion that misses variable is more complex
#now lets plot 

ggplot(misses[misses$Survived != "None",], aes(x=Age, fill = Survived)) +
  geom_bar(width = 5) +
  facet_wrap(~Pclass) + #group by statement within ggplot
  ggtitle("Age for Miss by Pclass") +
  xlab("Age") +
  ylab("Total Count") +
  labs(fill = "Survived")

#eventually we want to do some feature engineering on this Miss by adult or child by class
# information there seems to be a lot more females perishing in 3rd class than 
# first and second, there is some predictive power here, also female children 
# are much more likely to survive

#create subset that consist of misses without siblings or parents on titanic
misses.alone <- misses[which(misses$SibSp == 0 & misses$Parch == 0),]
summary(misses.alone$Age)
length(which(misses.alone$age <=14.5))

#how many of this alone travelers are less than 14.5 years old (equivalent of title master for males)
# when comparing age of misses and age of misses alone, we see that alone travelers are older,
#confirming our hypothesis

#for feature engineering purposes we will classify anyone miss that is traveling alone as an adult female

#now lets look at sipbsp variable, summarize the variable
summary(data.combined$SibSp)
#no NAs , which is good we dont like missing values
# half the records are 0!!
#this variable is heavily skewed to zero
#can we treat this variable as a categorical variable to make things easier

?unique

#lets find out how many unique values we have for SibSp variable to test if we can define it as a factor
length(unique(data.combined$SibSp))
#7 unique values, seems reasonable to transform to factor
data.combined$SibSp <- as.factor(data.combined$SibSp)

# we believe title is predictive, visualize survival rates by sibsp, pclass and title

ggplot(data.combined[1:891,], aes(x=SibSp, fill=Survived)) +
  geom_bar(stat="count") +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("SibSp") +
  ylab("Total Count") +
  ylim(0,300)+
  labs(fill = "Survived")

#looking at 4 things simultaneously!! pretty cool

#do same thing for parch variable which represents how many parents? children abourd the titanic
#first convert to factor

data.combined$Parch <- as.factor(data.combined$Parch)

ggplot(data.combined[1:891,], aes(x=Parch, fill=Survived)) +
  geom_bar(stat="count") +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("Parch") +
  ylab("Total Count") +
  ylim(0,300)+
  labs(fill = "Survived")

#Now for feature engineering!!
#we will infer you are traveling alone if parch and sibsp is equal to zero

# we use combined function to store one long array for both train and test
#grabbing from train and test instead of data.combined because we transformed values into factors
#data.combined
temp.sibsp <- c(train$SibSp, test$SibSp)
temp.parch <- c(train$Parch, test$Parch)
#we get to integer vectirs of 1309

#we create new var in data.combined as a family size var that is a factor
data.combined$family.size <- as.factor(temp.sibsp+ temp.parch +1)

#plot it!
ggplot(data.combined[1:891,], aes(x=family.size, fill=Survived)) +
  geom_bar(stat="count") +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("Family Size") +
  ylab("Total Count") +
  ylim(0,300)+
  labs(fill = "Survived")

#data analysis part 3

#lets look at ticket variable
str(data.combined$Ticket)
#has 929 values, probably not a factor

#lets convert to character
data.combined$Ticket <- as.character(data.combined$Ticket)
# check what ticket variable now looks like
data.combined$Ticket[1:20]
#this data is random numbers, we have no idea what we are lookin at
#lets look at the letters in each ticket and see if we can find pattern


Ticket.first.char <- ifelse(data.combined$Ticket == "", "", substr
                            (data.combined$Ticket, 1,1))
#substring is built in function that grabs part of string
# this function will grab ticket and start and position 1 and stop 1 
??first.char

# substring call is wrapped in an if else
?ifelse 
#if true then do this if not then do that

#we first check if ticket is empty and if that is the case we 
#just return empty space if not then i want the first string 

#this will be run for the whole vector of 1309 values

#unique function will return unique values of vector
unique(Ticket.first.char)
length(unique(Ticket.first.char))
summary(Ticket.first.char)
#get a sense of variable created

#add variable created to our existing data set
data.combined$Ticket.first.char <- as.factor(Ticket.first.char)

#16 unique values now, this can be considered a factor variable now

#plot the variable 
ggplot(data.combined[1:891,], aes(x=Ticket.first.char, fill=Survived)) +
  geom_bar(stat="count") +
  #facet_wrap(~Pclass + Title) +
  ggtitle("Survival Rate by Ticket") +
  xlab("Ticket.first.char") +
  ylab("Total Count") +
  ylim(0,350)+
  labs(fill = "Survived")

#now lets replot using pclass
ggplot(data.combined[1:891,], aes(x=Ticket.first.char, fill=Survived)) +
  geom_bar(stat="count") +
  facet_wrap(~Pclass) +
  ggtitle("Survival Rate by Ticket and Pclass") +
  xlab("Ticket.first.char") +
  ylab("Total Count") +
  ylim(0,350)+
  labs(fill = "Survived")

#combo of pclass and title have been most predictive so far lets see if ticket adds any
#additional predictive power

ggplot(data.combined[1:891,], aes(x=Ticket.first.char, fill=Survived)) +
  geom_bar(stat="count") +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass Title") +
  xlab("Ticket.first.char") +
  ylab("Total Count") +
  ylim(0,350)+
  labs(fill = "Survived")

#seems like ticket is not really indicative of survival

#next variable is fares, amount of money passengers paid for ticket
#we assume there will be correlation between pclass and amont of money paid for ticket

#looking into fare variable
summary(data.combined$Fare)
length(unique(data.combined$Fare))

#some folks didnt pay any fare at all!
# 25% of fares were 25 pounds or less
# 50 % of fares were less than 14.5 pounds
# mean is much larger than median, are distribution is heavily right skewed

#lets check distribution

# Can't make fare a factor, treat as numeric & visualize with histogram
ggplot(data.combined, aes(x = Fare)) +
  stat_count(width = 5) +
  ggtitle("Combined Fare Distribution") +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,200)

#large bulk of folks in titanic did not pay a whole lot

#lets drill down and see if it has predictive power in addition to pclass and title
ggplot(data.combined[1:891,], aes(x=Fare, fill=Survived)) +
  geom_bar(width=5) +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass Title") +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,50)+
  labs(fill = "Survived")
warnings()

#doesnt seem like there is additional predictive power, in some specific cases there seems to be
#but we dont want to overfit model, that is this cases have very little volume and
#will probably not generalize well outside of this training set


#now lets look at cabin, this will probably be correlated with pclass
str(data.combined$Cabin)
#read in as a factor with 187 vars, we should change this, way too many levels

data.combined$Cabin <- as.character(data.combined$Cabin)
data.combined$Cabin[1:100]
# lots of missing data!

#letters are prob decks, and numbers are rows

#are missing values indicative of 3rd class?
#some have multiple cabins
# we need to manipulate 

#replace empty cabins with U for unknown
data.combined[which(data.combined$Cabin == ""), "Cabin"] <- "U"
#which is where in sql clause
#grab cabin variable after that and cram a U into it
data.combined$Cabin[1:100]
#if there is any signal in this variable, it is probably due to decks,
#if we focus on row number, we can run into overfitting problem

#lets take a look at deck and treat as factor
Cabin.first.char <-as.factor(substr(data.combined$Cabin, 1,1))
#substring(var, start position, stop position)
levels(Cabin.first.char)
#we see all expected variables in factor

#cram this new variable into our existing data frame
data.combined$Cabin.first.char <- Cabin.first.char

ggplot(data.combined[1:891,], aes(x=Cabin.first.char, fill=Survived)) +
  geom_bar() +
  ggtitle("Survivability by cabin.first.char") +
  xlab("cabin.first.char") +
  ylab("Total Count") +
  ylim(0,750) +
  labs(fill = "Survived")

#interesting, but probably indiicative of pclass
#lets graph relationship between deck and pclass

ggplot(data.combined[1:891,], aes(x = Cabin.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("Survivability by cabin.first.char") +
  xlab("Pclass") +
  ylab("Total Count") +
  ylim(0,500) +
  labs(fill = "Survived")

#vast mahority of folks with no tickets are in 2nd and 3rd classs

#maybe cabin is not helpful for us because it is highly correlated with class

#adding in pclass and title to see if there is some interesting insigh, just in case
ggplot(data.combined[1:891,], aes(x = Cabin.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("cabin.first.char") +
  ylab("Total Count") +
  ylim(0,500) +
  labs(fill = "Survived")

#what about folks with multiple cabins?
# representative of more wealth

?str_detect
#used here to go through cabin variable and detect blank space in cabin which would 
#be indicative of multiple cabins

data.combined$Cabin.multiple <- as.factor(ifelse(str_detect(data.combined$Cabin, " "),
                                                 "Y", "N"))

ggplot(data.combined[1:891,], aes(x = Cabin.multiple, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("cabin.multiple") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill = "Survived")        

#again we dont see a signal that pops out at us
#multiple cabins are very rare, prone to overfit if we account for this

#where you got onboard, what city?
str(data.combined$Embarked)
levels(data.combined$Embarked)

ggplot(data.combined[1:891,], aes(x = Embarked, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("embarked") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

#if you embard from queensland you are most likely to be a third class passenger,
#but this doesnt help us with survivability

#in summary we want to use pclass and title, which we derived using sex and age feature
#there is some useful info in sigsb and parch which we used to make family size

#next vid exploratory modelling 1


#==============================================================================
#
# Video #4 - Exploratory Modeling
#
#==============================================================================

#explore top predictors pclass and title, build a model with these two vars
#train random forest with default parameters


#minimal data frame to explore two vars
rf.train.1 <-data.combined[1:891, c("Pclass", "Title")]
#did you survive or not
rf.label <- as.factor(train$Survived)

#random forest are random, set seed to replicate forest with different vars and parameters
set.seed(1234)
rf.1 <-randomForest(x=rf.train.1, y=rf.label, importance =TRUE, ntree = 1000)
#as you train trees keep track of importance of variables, and keep number of trees at 1000
#very fast cause data is mall

rf.1
#must expand viewer to see plot
varImpPlot(rf.1)
#title is way more predictive than pclass!

#now add sibsb, travelling with sibling or spuse
rf.train.2 <- data.combined[1:891, c("Pclass", "Title", "SibSp")]

set.seed(1234)
rf.2 <- randomForest(x = rf.train.2, y = rf.label, importance = TRUE, ntree = 1000)
rf.2
varImpPlot(rf.2)
#only about a 1% improvement in error rate, but this is actually a huge deal

#dramatically increases accuracy rate for those who survive

#now try parch var, traveling with parent or child
rf.train.3 <- data.combined[1:891, c("Pclass", "Title", "Parch")]

set.seed(1234)
rf.3 <- randomForest(x = rf.train.3, y = rf.label, importance = TRUE, ntree = 1000)
rf.3
varImpPlot(rf.3)

#improves OOB error but not as much as SibSP
#now lets try them all together

rf.train.4 <- data.combined[1:891, c("Pclass", "Title", "SibSp", "Parch")]
set.seed(1234)

rf.4 <-randomForest(x=rf.train.4, y=rf.label, importance = TRUE, ntree = 1000)
rf.4
varImpPlot(rf.4)

#woah even better improvement

#women and children first
#wealthier folks tend to survive
#smaller families in third class had greater chance of survival

# we ended up creating family size variable
rf.train.5 <-data.combined[1:891, c("Pclass", "Title", "family.size")]
rf.5 <-randomForest(x=rf.train.5, rf.label, importance = TRUE, ntree = 1000)
rf.5
varImpPlot(rf.5)
#even betterrrr!!!

#this gives us a better idea for feature engineering
# family size matters and we should look into further
#breaking it down

#how does model react when we add correlated vars
#Train a Random Forest using pclass, title, sibsp, & family.size
rf.train.6 <- data.combined[1:891, c("Pclass", "Title", "SibSp", "family.size")]

set.seed(1234)
rf.6 <- randomForest(x = rf.train.6, y = rf.label, importance = TRUE, ntree = 1000)
rf.6
varImpPlot(rf.6)



# Train a Random Forest using pclass, title, parch, & family.size
rf.train.7 <- data.combined[1:891, c("Pclass", "Title", "Parch", "family.size")]

set.seed(1234)
rf.7 <- randomForest(x = rf.train.7, y = rf.label, importance = TRUE, ntree = 1000)
rf.7
varImpPlot(rf.7)


