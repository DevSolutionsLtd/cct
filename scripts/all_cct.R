#Population data i.e All CCt enrolees
library(data.table)
library(ggplot2)
library(Amelia)

#Load data
pop <- fread("consolidated data.csv", na.strings = "")
pop <- data.table(pop)
pop
tables()
str(pop)
names(pop)

#Cleaning Data
levels(pop$state)
levels(pop$state) <- 
  c("Anambra", "Bauchi","Bayelsa","Ebonyi","FCT",
    "Kaduna","Niger","Ogun","Zamfara") #Changing case
levels(pop$state)
#Deal with Missing values
apply(pop, 2, function(x) sum(is.na(x)))

# Plot missing values
missmap(pop,
        main = "Missingness Map of CCT Dataset",
        y.labels = NULL,
        y.at = NULL)


data$age[is.na(data$age)] <- mean(data$age, na.rm = TRUE)

#Explore 'Age'
summary(pop$age)
hist(pop$age,xlim=c(0,60),
     main="Age Distribution of CCT Enrollees",
     xlab= "Age (years)",
     ylim=c(0,6000),
     xlim=c(10,60),
     ylab="No. of Enrollees")

boxplot(pop$age,main="Age  of CCT Enrollees")

boxplot(pop$age~pop$state,
        main="Age of CCT Enrollees in different States",
        ylab="Age of Enrollee") #comparing among States

#Look at some of the extreme values
max(pop$age,na.rm=T)
which(pop$age==52)
pop[29689,]

min(pop$age,na.rm=T)
which(pop$age==10)
pop[26306,]

which(pop$age>45)
length(which(pop$age>45))

#ANOVA
aov(data$age~data$state)
