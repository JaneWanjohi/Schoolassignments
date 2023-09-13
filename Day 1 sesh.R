df <- read.csv("~/Documents/AIMS2022/Review block4/supervised and unsupervised algorithms/practicals/R scripts/Practicals/indmover.csv")
str(df)
age <- sample(20:70,50, replace =TRUE)
Tribe <- sample(c(1,2,3),50, replace = T)
Income <- sample(20:70,50,replace = T)
weight <- sample(c(1,2),50,replace =T)
grade <- sample(c(1,2),50,replace = T)
gender <- sample(c(1,2),replace = T)
edulevel <- sample(c(1,2,3),50,replace = T)
mstatus <- sample(c(1,2),50,replace =T)
hunit <- sample(c(1,2,3,4),50,replace =T)


####continous variable
anyvar <- runif(50)

#dataframe 
mdata <- data.frame(age,Tribe,Income,weight,grade,gender,edulevel,
                    mstatus,hunit,anyvar)
summary(mdata)

write.csv(mdata, "Practdata.csv",row.names = F)
datf <- read.csv("Practdata.csv")
head(datf)

str(datf)
f_var <- c('Tribe', 'weight', 'grade', 'gender','edulevel','mstatus','hunit')
datf[f_var] <- lapply(datf[f_var], factor) 
str(datf)

# Run Boruta Algorithm
#checks the important features
#yellow it can be 
#green = it is important
#the higher the importance the better the predictor but yu have to consider the color
set.seed(123)
boruta = Boruta(Income~., data = datf, doTrace = 100)
print(boruta)
plot(boruta)

data1= read.csv ("indmover.csv", sep=",", stringsAsFactors=FALSE, header=TRUE).
Indmover= sort(sample(nrow(data1), nrow(data1)*0.70))
trainD=Indmover[Indmover,]
testD=Indmover[-Indmover,]
#This code splits the data into 70% training set and 30% testing set.

#TASK 3
#filter selects the rows and select gets the columns
agedf <- filter(datf, age>30)
agedf

library(dplyr)
dtage <- datf %>% filter(age>30) 
dtage

#columns
dtage1 <- datf %>% select(Tribe)
dtage1

###educationlevel is greater than 2
dt1 <- mdata %>% filter(edulevel>2)
dt1

#age is greater than 25and hunit is less or equal to 3
dt2 <- mdata %>% filter(age>2 & hunit<=3)
dt2

str(mdata)
#filter tribe and grade
dt3 <- mdata %>% filter(Tribe>=3 & grade==2)
dt3

#subset

## subset all continous 
dt4 <- datf %>% select_if(is.numeric)
dt4

length(dt4)
write.csv(dt1, "dt1.csv")
write.csv(dt2, "dt2.csv")
write.csv(dt2, "dt3.csv")
write.csv(dt4, "dt4.csv")


###task 4
head(df)
#ind <- read.csv("indmover.csv", sep =",",stringsAsFactors = FALSE, header = TRUE)
Indmover= sort(sample(nrow(df), nrow(df)*0.70))
trainD=df[Indmover,]
testD=df[-Indmover,]
boruta <- Boruta(overwgt~. ,data=df,doTrace =100)
print(boruta)
plot(boruta)
