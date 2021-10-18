
#By: khushal Das
#khushaldasparmar@gmail.com

#installing dplyr
install.packages("dplyr")

#installing ggplot2
install.packages("ggplot2")

library(dplyr)

library(ggplot2)


#path of directiry - set
setwd("E:/Acedimics/MS/Sems_1/Bussiness_Analytics/GroupAssignment_1")

#reading csv file
pisaTraining = read.csv('pisa2009train.csv')

#number of columns in data
length(pisaTraining) #24

nrow(pisaTraining) #3663

#structure of data
str(pisaTraining)

# remove null values from dataset
pisaTraining = na.omit(pisaTraining)
is.null(pisaTraining) #returns false

nrow(pisaTraining) #2414

#3663-2414=1249 null values

# draw histogram of student schools reading score
ggplot(pisaTraining, aes(readingScore)) + geom_histogram(bins = 30) +
  ggtitle('Student schools reading score') + 
  xlab('reading score')

#normally distributed #mean median values are almost same

median(pisaTraining$readingScore) #520
#mean of reading score 
mean(pisaTraining$readingScore, na.rm=TRUE) #517


# bar plot of students race or ethnicity reading score
ggplot(pisaTraining, aes(x=raceeth, y=readingScore)) + geom_bar(stat='identity') +
  xlab('race or ethnicity') + 
  ylab('reading scores') +
  ggtitle('students race or ethnicity reading score') + 
  coord_flip()

# Sum of urban school student
sum(pisaTraining$urban==1) #urabn

sum(pisaTraining$urban==0) #not urban

# total school students who speaks english at home
sum(pisaTraining$englishAtHome, na.rm=TRUE) #2128

# strength of urban school students who speaks english at home
urban_englishAtHome = filter(pisaTraining, urban==1 & englishAtHome==1)
sum(urban_englishAtHome$englishAtHome) #returns 696

# strength of students educated family background
edu_df = filter(pisaTraining, motherBachelors==1 & fatherBachelors==1)
length(edu_df$motherBachelors) #return 611

# bar graph of students that spend there time in english classes
ggplot(pisaTraining, aes(x=raceeth,y=minutesPerWeekEnglish)) + geom_bar(stat='identity') +
  ggtitle('students that spend there time in english classes') +
  xlab('race or ethnicity') + 
  ylab('spend minutes') +
  coord_flip()
#White rce students spends more time in english classes then others

# graph of reading scores against strength of students in a school
ggplot(pisaTraining, aes(x=readingScore, y=schoolSize, color=raceeth)) + geom_point()

# graph of reading scores against students english classes in a school
ggplot(pisaTraining, aes(x=minutesPerWeekEnglish, y=readingScore, color=raceeth)) + geom_point() +
  ggtitle('Scatter plot of students reading score against students minutes in english classes') +
  xlab('students minutes in english classes') +
  ylab('students reading score')

# maximum Asian students strength
df_asian = subset(pisaTraining, pisaTraining$raceeth=='Asian')
df_asian$schoolSize[which.max(df_asian$schoolSize)] #3595

# pie chart of schools student strength against race or ethnicity 
ggplot(pisaTraining, aes(x="", y=schoolSize, fill = raceeth)) + 
  geom_bar(stat='identity', width = 1) +
  coord_polar('y',start=0)
#white has max school strength size

# box plot of every school size against race or ethnicity
boxplot(pisaTraining$schoolSize ~ pisaTraining$raceeth,
        xlab = "race or ethnicity",
        ylab = "school sizes",
        main = "race or ethnicity box plot")


#-------------Multiple Regression--------------

#dividing data into train and test set 

# 80% of the sample size
smp_size <- floor(0.8 * nrow(pisaTraining))

## set the seed to make your partition reproducible
set.seed(123)
train_ <- sample(seq_len(nrow(pisaTraining)), size = smp_size)

training_data <- pisaTraining[train_, ]
testing_data <- pisaTraining[-train_, ]

relation_1 <- lm(training_data$readingScore~raceeth,data=training_data)

summary(relation_1)


relation_2 <- lm(training_data$readingScore~englishAtHome,data=training_data)

summary(relation_2)

#residual standard error  is Standard deviation of residual 
#Multiple R- squared is percentage of variance 

relation <- lm(training_data$readingScore~
grade+
male+
raceeth+
preschool+
expectBachelors+
motherHS+
motherBachelors+
motherWork+
fatherHS+
fatherBachelors+
fatherWork+
selfBornUS+
motherBornUS+
fatherBornUS+
englishAtHome+
computerForSchoolwork+
read30MinsADay+
minutesPerWeekEnglish+
studentsInEnglish+
schoolHasLibrary+
publicSchool+
urban+
schoolSize,data=training_data)

summary(relation)

print(relation)

confint(relation)


predictions = predict.lm(relation, testing_data)

predictions

RMS = mean(predictions-testing_data$readingScore)^2 #Root mean square 

print('Residual Error (MSE): ')
RMS

plot(relation)





