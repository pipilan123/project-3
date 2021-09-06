# project-3
z <- cbind(x, d)
if(!is.null(y))
z <- z + y
else
z <- z + f
g <- x + y / z
if(d == 3L)
return(g)
g <- g + 10
g
}
class(g)
pollutantmean <- function(directory, pollutant, id= 1:332){
pollutants = c()
filenames = list.files(directory)
for(i in id){
filepath=paste(directory,"/" ,filenames[i], sep="")
data = read.csv(filepath, header = TRUE)
pollutants = c(pollutants, data[,pollutant])
pollutants_mean = mean(pollutants, na.rm=TRUE)
pollutants_mean
}
pollutantmean <- function(directory, pollutant, id= 1:332){
pollutants = c()
filenames = list.files(directory)
for(i in id){
filepath=paste(directory,"/" ,filenames[i], sep="")
data = read.csv(filepath, header = TRUE)
pollutants = c(pollutants, data[,pollutant])
pollutants_mean = mean(pollutants, na.rm=TRUE)
}
pollutants_mean
}
pollutantmean <- function(directory, pollutant, id= 1:332){
pollutants = c()
filenames = list.files(directory)
for(i in id){
filepath=paste(directory,"/" ,filenames[i], sep="")
data = read.csv(filepath, header = TRUE)
pollutants = c(pollutants, data[,pollutant])
pollutants_mean = mean(pollutants, na.rm=TRUE)
}
pollutants_mean
}
pollutantmean <- function(directory, pollutant, id= 1:332){
pollutants = c()
filenames = list.files(directory)
for(i in id){
filepath=paste(directory,"/" ,filenames[i], sep="")
data = read.csv(filepath, header = TRUE)
pollutants = c(pollutants, data[,pollutant])
pollutants_mean = mean(pollutants, na.rm=TRUE)
}
pollutants_mean
}
pollutantmean("C:/Users/Lenovo/Desktop/Rstudio/specdata", "sulfate", 1:10)
pollutantmean <- function(directory, pollutant, id= 1:332){
pollutants = c()                                            
filenames = list.files(directory)                          
for(i in id){
filepath=paste(directory,"/" ,filenames[i], sep="")      
data = read.csv(filepath, header = TRUE)                  
pollutants = c(pollutants, data[,pollutant])             
}
pollutants_mean = mean(pollutants, na.rm=TRUE)          
pollutants_mean                                            
}
install.packages("rda")
install.packages("rda")
add<- function(x,y){}
add<- function(x,y){x+y}
add(2,3)
pollutantmean <- function(directory, pollutants, id= 1:332){
pollutants = c()                                            
filenames = list.files(directory)                           
filepath=paste(directory,"/" ,filenames[i], sep="")     
colnames(dataf) <- c("id", "nobs")
dataf
}
cc <- complete("C:/Users/Lenovo/Desktop/Rstudio/specdata", 54)                               #cc涓湁"id" "nobs" 鍏ヽolumns
print(cc$nobs)                                               #nobs鐨� vector
rankall <- function (outcome, num = "best"){
data <- read.csv("outcome-of-care-measures.csv")
disease_list <- c("heart attack", "heart failure", "pneumonia")
if (!outcome %in% disease_list){
stop ("invalid outcome")
}
## Extract the hospital and rate colume from the data.
if (outcome == "heart attack") {
data <- data[c("Hospital.Name", "State", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")]
}
else if (outcome == "heart failure"){
data <- data[c("Hospital.Name", "State", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")]
}
else {
data <- data[c("Hospital.Name", "State", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
}
## Assign a common colume name for the StateData whatever the disease is.
colnames(data)[3] <- "Disease.Rate"
## Transform the disease.rate colume from Factor to numeric for the purpose of ordering.So does the hospital colume.
data[, "Disease.Rate"] <- as.numeric(as.character(data[, "Disease.Rate"]))
data[, "Hospital.Name"] <- as.character(data[, "Hospital.Name"])
## Create a list to store all of the state names in US, and order it alphabetically.
Statelist <- as.character(unique(data$State))
Statelist <- Statelist[order(Statelist)]
Final <- data.frame()
for (i in seq_len(length(Statelist))){
## Create the sub-data.frame for the specific state.
StateData <- subset(data, State == Statelist[i])
## Order the data.frame by disease rate and hospital names.
StateData <- StateData[order(StateData$Disease.Rate, StateData$Hospital.Name), ]
## Specify the exact value for num input.
N <- sum(!is.na(StateData$Disease.Rate))
if (num == "best"){
num <- 1
}
else if (num == "worst"){
num <- N
}
else{}
Hospital <- StateData[num, "Hospital.Name"]
tmp <- data.frame(Hospital, Statelist[i])  # Create each row for the final data.frame.
colnames(tmp) <- c("hospital", "state")
Final <- rbind(Final, tmp)
}
Final
}
r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)
rankall <- function (outcome, num = "best"){
data <- read.csv("C:/Users/Lenovo/Desktop/RStudio/outcome-of-care-measures.csv")
disease_list <- c("heart attack", "heart failure", "pneumonia")
if (!outcome %in% disease_list){
stop ("invalid outcome")
}
## Extract the hospital and rate colume from the data.
if (outcome == "heart attack") {
data <- data[c("Hospital.Name", "State", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")]
}
else if (outcome == "heart failure"){
data <- data[c("Hospital.Name", "State", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")]
}
else {
data <- data[c("Hospital.Name", "State", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
}
## Assign a common colume name for the StateData whatever the disease is.
colnames(data)[3] <- "Disease.Rate"
## Transform the disease.rate colume from Factor to numeric for the purpose of ordering.So does the hospital colume.
data[, "Disease.Rate"] <- as.numeric(as.character(data[, "Disease.Rate"]))
data[, "Hospital.Name"] <- as.character(data[, "Hospital.Name"])
## Create a list to store all of the state names in US, and order it alphabetically.
Statelist <- as.character(unique(data$State))
Statelist <- Statelist[order(Statelist)]
Final <- data.frame()
for (i in seq_len(length(Statelist))){
## Create the sub-data.frame for the specific state.
StateData <- subset(data, State == Statelist[i])
## Order the data.frame by disease rate and hospital names.
StateData <- StateData[order(StateData$Disease.Rate, StateData$Hospital.Name), ]
## Specify the exact value for num input.
N <- sum(!is.na(StateData$Disease.Rate))
if (num == "best"){
num <- 1
}
else if (num == "worst"){
num <- N
}
else{}
Hospital <- StateData[num, "Hospital.Name"]
tmp <- data.frame(Hospital, Statelist[i])  # Create each row for the final data.frame.
colnames(tmp) <- c("hospital", "state")
Final <- rbind(Final, tmp)
}
Final
}
r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)
r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)
r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)
r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)
set.seed(1)
rpois(5, 2)
set.seed(10)
x <- rep(0:1, each = 5)
e <- rnorm(10, 0, 20)
y <- 0.5 + 2 * x + e
system.time()
library(datasets)
Rprof()
fit <- lm(y ~ x1 + x2)
Rprof(NULL)
data<- read.csv("C:/Users/Lenovo/Desktop/cov/rep_query.xlsx")
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(fileURL,destfile = "./data/test.csv",method = "curl")
list.files("./data")
caichan<- properties()
sum(data[!is.na(data$VAL),]$VAL)
sum(data[!is.na(data$VAL),]$VAL==24)
if(!file.exists("data")) dir.create("data")
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(fileUrl, destfile = "./data/United States communities.csv")
data <- read.csv("./data/United States communities.csv")
sum(data[!is.na(data$VAL),]$VAL == 24)
if(!file.exists("data")) dir.create("data")
library(xlsx)
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"
download.file(fileUrl, destfile = "./data/Natural Gas Aquisition Program.xlsx", mode = "wb")
dateDownloaded <- date()
dat <- read.xlsx("./data/Natural Gas Aquisition Program.xlsx", sheetIndex = 1, rowIndex = 18:23, colIndex = 7:15, header = TRUE)
sum(dat$Zip*dat$Ext,na.rm=T)
if(!file.exists("data")) dir.create("data")
library(xlsx)
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"
download.file(fileUrl, destfile = "./data/Natural Gas Aquisition Program.xlsx", mode = "wb")
dateDownloaded <- date()
data <- read.xlsx("./data/Natural Gas Aquisition Program.xlsx", sheetIndex = 1, rowIndex = 18:23, colIndex = 7:15, header = TRUE)
sum(data$Zip*dat$Ext,na.rm=T)
if(!file.exists("data")) dir.create("data")
library(xlsx)
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"
download.file(fileUrl, destfile = "./data/Natural Gas Aquisition Program.xlsx", mode = "wb")
dateDownloaded <- date()
data <- read.xlsx("./data/Natural Gas Aquisition Program.xlsx", sheetIndex = 1, rowIndex = 18:23, colIndex = 7:15, header = TRUE)
sum(data$Zip*data$Ext,na.rm=T)
if(!file.exists("data")) dir.create("data")
library(XML)
fileUrl <- "http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
doc <- xmlTreeParse(fileUrl, useInternal = TRUE)
rootNode <- xmlRoot(doc)
sum(xpathSApply(rootNode, "//zipcode", xmlValue) == "21231")
if(!file.exists("data")) dir.create("data")
library(XML)
fileUrl <- "http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
doc <- xmlTreeParse(fileUrl, useInternal = TRUE)
rootNode <- xmlRoot(doc)
sum(xpathSApply(rootNode, "//zipcode", xmlValue) == "21231")
if(!file.exists("data")) dir.create("data")
library(XML)
fileUrl <- "http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
doc <- xmlTreeParse(fileUrl, useInternal = TRUE)
rootNode <- xmlRoot(doc)
sum(xpathSApply(rootNode, "//zipcode", xmlValue) == "21231")
install.packages(xml)
install.packages("xml")
install.packages("XML")
if(!file.exists("data")) dir.create("data")
library(XML)
fileUrl <- "http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
doc <- xmlTreeParse(fileUrl, useInternal = TRUE)
rootNode <- xmlRoot(doc)
sum(xpathSApply(rootNode, "//zipcode", xmlValue) == "21231")
if(!file.exists("data")) dir.create("data")
library(xlsx)
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"
download.file(fileUrl, destfile = "./data/Natural Gas Aquisition Program.xlsx", mode = "wb")
dateDownloaded <- date()
dat <- read.xlsx("./data/Natural Gas Aquisition Program.xlsx", sheetIndex = 1, rowIndex = 18:23, colIndex = 7:15, header = TRUE)
sum(dat$Zip*dat$Ext,na.rm=T)
if(!file.exists("data")) dir.create("data")
library(xlsx)
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"
download.file(fileUrl, destfile = "./data/Natural Gas Aquisition Program.xlsx", mode = "wb")
dateDownloaded <- date()
data <- read.xlsx("./data/Natural Gas Aquisition Program.xlsx", sheetIndex = 1, rowIndex = 18:23, colIndex = 7:15, header = TRUE)
sum(data$Zip*data$Ext,na.rm=T)
if(!file.exists("data")) dir.create("data")
library(xlsx)
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"
download.file(fileUrl, destfile = "./data/Natural Gas Aquisition Program.xlsx", mode = "wb")
dateDownloaded <- date()
dat <- read.xlsx("./data/Natural Gas Aquisition Program.xlsx", sheetIndex = 1, rowIndex = 18:23, colIndex = 7:15, header = TRUE)
sum(dat$Zip*dat$Ext,na.rm=T)
if(!file.exists("dat")) dir.create("dat")
library(xlsx)
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"
download.file(fileUrl, destfile = "./data/Natural Gas Aquisition Program.xlsx", mode = "wb")
dateDownloaded <- date()
dat <- read.xlsx("./data/Natural Gas Aquisition Program.xlsx", sheetIndex = 1, rowIndex = 18:23, colIndex = 7:15, header = TRUE)
sum(dat$Zip*dat$Ext,na.rm=T)
# From slides, we can select the second one as solution. But here I will use systerm.time() function too see their time.
if(!file.exists("data")) dir.create("data")
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
download.file(fileUrl, destfile = "United States communities.csv")
library(data.table)
DT <- fread("United States communities.csv")
system.time(tapply(DT$pwgtp15, DT$SEX, mean))
system.time(DT[, mean(pwgtp15), by = SEX])
system.time(mean(DT[DT$SE == 1, ]$pwgtp15)) + system.time(DT[DT$SEX == 2, ]$pwgtp15)
system.time(mean(DT$pwgtp15, by = DT$SEX))
system.time(sapply(split(DT$pwgtp15,DT$SEX),mean))
if(!file.exists("data")) dir.create("data")
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(fileUrl, destfile = "./data/United States communities.csv")
data <- read.csv("./data/United States communities.csv")
data$FES
library(dplyr)
setwd("UCI HAR Dataset")
x_train   <- read.table("./train/X_train.txt")
y_train   <- read.table("./train/Y_train.txt")
sub_train <- read.table("./train/subject_train.txt")
x_test   <- read.table("./test/X_test.txt")
y_test   <- read.table("./test/Y_test.txt")
sub_test <- read.table("./test/subject_test.txt")
features <- read.table("./features.txt")
activity_labels <- read.table("./activity_labels.txt")
x_total   <- rbind(x_train, x_test)
y_total   <- rbind(y_train, y_test)
sub_total <- rbind(sub_train, sub_test)
sel_features <- variable_names[grep(".*mean\\(\\)|std\\(\\)", features[,2], ignore.case = FALSE),]
x_total      <- x_total[,sel_features[,1]]
colnames(x_total)   <- sel_features[,2]
colnames(y_total)   <- "activity"
colnames(sub_total) <- "subject"
total <- cbind(sub_total, y_total, x_total)
total$activity <- factor(total$activity, levels = activity_labels[,1], labels = activity_labels[,2])
total$subject  <- as.factor(total$subject)
total_mean <- total %>% group_by(activity, subject) %>% summarize_all(funs(mean))
write.table(total_mean, file = "./tidydata.txt", row.names = FALSE, col.names = TRUE)
library(dplyr)
setwd("C:/Users/Lenovo/Desktop/UCI HAR Dataset")
x_train   <- read.table("./train/X_train.txt")
y_train   <- read.table("./train/Y_train.txt")
sub_train <- read.table("./train/subject_train.txt")
x_test   <- read.table("./test/X_test.txt")
y_test   <- read.table("./test/Y_test.txt")
sub_test <- read.table("./test/subject_test.txt")
features <- read.table("./features.txt")
activity_labels <- read.table("./activity_labels.txt")
x_total   <- rbind(x_train, x_test)
y_total   <- rbind(y_train, y_test)
sub_total <- rbind(sub_train, sub_test)
sel_features <- variable_names[grep(".*mean\\(\\)|std\\(\\)", features[,2], ignore.case = FALSE),]
x_total      <- x_total[,sel_features[,1]]
colnames(x_total)   <- sel_features[,2]
colnames(y_total)   <- "activity"
colnames(sub_total) <- "subject"
total <- cbind(sub_total, y_total, x_total)
total$activity <- factor(total$activity, levels = activity_labels[,1], labels = activity_labels[,2])
total$subject  <- as.factor(total$subject)
total_mean <- total %>% group_by(activity, subject) %>% summarize_all(funs(mean))
write.table(total_mean, file = "./tidydata.txt", row.names = FALSE, col.names = TRUE)
source("~/.active-rstudio-document")
source("~/.active-rstudio-document")
source("~/.active-rstudio-document")
source("C:/Users/Lenovo/Desktop/UCI HAR Dataset/run_analysis.R")

