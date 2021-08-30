load("eval.Rdata")

head(eval)

## No missing data
sum(complete.cases(eval))
library("Amelia")
missmap(eval)

#### RESEARCH QUESTIONS
### 1. Is there evidence that professor rank is independent of gender?
# We convert into factors both attributes rank and gender.
table(eval$rank)
table(eval$gender)

eval$rank <- as.factor(eval$rank)
eval$gender <- as.factor(eval$gender)

# We import ggplot2 library and plot a bar plot that differentiates 
# ranks of professors.
library("ggplot2")
ggplot(eval, aes(x = rank, fill = gender)) +
  geom_bar() +
  xlab("Rank") +
  ylab("Total Count") +
  labs(fill = "Gender") 

# Now we also separate in male and female for each teacher rank to
# analyze if there's independency.
ggplot(eval, aes(x = rank, y=..prop.., fill = gender, group=gender)) +
  geom_bar(position=position_dodge()) +
  xlab("Rank") +
  ylab("Total Count") +
  labs(fill = "Gender")

# Finally we draw a mosaic plot to show more in detail the linear dependency
# between rank and gender.
table0=xtabs(~gender+rank, data=eval)
prop.table(table0,1)
mosaicplot(table0,shade=TRUE, type="pearson",main="Relationship between Rank and Gender")

## CONCLUSIONS: They are not independent, as the possibility of being tenured
## for men is higher than for women. This is a coherent conclussion, as one of
## real life problems is the lack of equality in their jobs for females.


#### 2. There are many categorical variables that can have an influence on 
## course evaluations. Focus on two of them.

library(dplyr)
library(ggplot2)

# We create a new variable evalN with all numeric attributes, to do correlation.
# We find those variables that are more correlated with ratings of subjects, and
# we will try to explain why.

evalN = select(eval)

evalN$eval <- eval$eval
evalN$rank <- as.numeric(eval$rank)
evalN$ethnicity <- as.numeric(eval$ethnicity)
evalN$gender <- as.numeric(eval$gender)
evalN$language <- as.numeric(eval$language)
evalN$age <- as.numeric(eval$age)
evalN$cls_level <- as.numeric(eval$cls_level)
evalN$pic_outfit <- as.numeric(eval$pic_outfit)
evalN$pic_color <- as.numeric(eval$pic_color)
evalN$cls_did_eval <- as.numeric(eval$cls_did_eval)
evalN$cls_level <- as.numeric(eval$cls_level)
evalN$cls_profs <- as.numeric(eval$cls_profs)
evalN$cls_students <- as.numeric(eval$cls_students)
evalN$cls_credits <- as.numeric(eval$cls_credits)
evalN$bty_f1lower <- as.numeric(eval$bty_f1lower)
evalN$bty_f1upper <- as.numeric(eval$bty_f1upper)
evalN$bty_f2upper <- as.numeric(eval$bty_f2upper)
evalN$bty_m1lower <- as.numeric(eval$bty_m1lower)
evalN$bty_m1upper <- as.numeric(eval$bty_m1upper)
evalN$bty_m2upper <- as.numeric(eval$bty_m2upper)
evalN$cls_perc_eval <- eval$cls_perc_eval
evalN$bty_avg <- eval$bty_avg

# Once all attributes are casted to numerical, we can do covariance and
# correlation matrices to see which ones correlate the most with evaluation
# of professors.

library(Hmisc)
cov(evalN[,-22])
r = cor(evalN[,-22])

## cls_credits = 0.23 and cls_perc_eval = 0.22
evalN$NewEval <- cut(evalN$eval, c(2,3,4,5))
evalN$cls_perc_evalN <- cut(evalN$cls_perc_eval, c(0,25,50,75,100))

table(evalN$NewEval)
table(evalN$cls_credits)
table(evalN$cls_perc_evalN)

## cls_credits and NewEval
ggplot(evalN, aes(x = cls_credits, y=..prop.., fill = NewEval, group=NewEval)) +
  geom_bar(position=position_dodge()) +
  xlab("ECTS of subject") +
  ylab("Total Count") +
  labs(fill = "Rating")

## There are much more multiple credit subjects than one ECTS subjects, but they
# are pretty well rated (with only (3.4) and (4,5) interval ratings). This means 
# that single ECTS subjects are more likely to be better rated, and also explains
# that there is a negative correlation between both variables. This is easy to know
# looking at the plot because one single credit subjects are of a lower level than
# mutltiple credit ones, but they have better mean average in rating, for example.

## cls_perc_evalN and NewEval
# We first create a discretized attribute for each one, and then plot it using ggplot.
ggplot(evalN, aes(x = cls_perc_evalN, y=..prop.., fill = NewEval, group=NewEval)) +
  geom_bar(position=position_dodge()) +
  xlab("Class % of completed evaluation") +
  ylab("Total Count") +
  labs(fill = "Rating")

## As before, there are much more values in the (75,100) interval than in others. In this
# case, there is also a relationship between the rating and the % of passed students in 
# regular evaluation. For subjects with less than 50% of passed people, it is more probable
# to be rated with a lower evaluation. However, in subjects that it is very easy to pass 
# (75, 100) ratings are more probable to be rated very well. This makes sense, as long as
# student's opinion changes depending of the toughness of the subject they are facing.

## CONCLUSIONS: Both attributes have a dependency with eval, but it is small, as their
# correlation are not that high enough. 


#### 3. Explore the relationship between course evaluations and a beauty score
# for each professor. Is there evidence of a different relationship 
# depending on gender?

table(eval$bty_f1lower)
table(eval$bty_f1upper)
table(eval$bty_f2upper)
table(eval$bty_m1lower)
table(eval$bty_m1upper)
table(eval$bty_m2upper)
table(eval$bty_avg)
table(eval$eval)

# We perform discretize operation for both attributes.
eval$beauty <- cut(eval$bty_avg, c(0,2,4,6,8,10))
# No values for ratings (0,2) interval. Therefore, we start "cutting" at 2.
eval$NewEval <- cut(eval$eval, c(2,3,4,5))

# Summary of new attributes' values.
table(eval$beauty)
table(eval$NewEval)

## We start comparing rating of subjects and beauty of professors.
# We import ggplot2 and plot a bar plot with beauty segments in x-axis.
library("ggplot2")
ggplot(eval, aes(x = beauty, fill = NewEval)) +
  geom_bar() +
  xlab("Beauty") +
  ylab("Total Count") +
  labs(fill = "Rating") 

# In this chart, bars are divided into the different ratings of subjects.
# We can see in (0,2) interval that almost all subjects of poor ratings 
# are taught by ugly teachers. Also, in the other intervals ratings seem to
# linearly depend on the "subjective beauty" of professors.
ggplot(eval, aes(x = beauty, y=..prop.., fill = NewEval, group=NewEval)) +
  geom_bar(position=position_dodge()) +
  xlab("Beauty") +
  ylab("Total Count") +
  labs(fill = "Rating")

# Finally we draw a mosaic plot to show more in detail the linear dependency
# between beauty and rating. In this one, we can see that there's a dependency.
# However, it is not very strong, as intermediate values are ones with more
# relevance.
table0=xtabs(~beauty+NewEval, data=eval)
prop.table(table0,1)
mosaicplot(table0,shade=TRUE, type="pearson",main="Ratings on subjects by Professors Beauty")

for (i in c(1:463)){
  eval$bty_f = (eval$bty_f1lower + eval$bty_f1upper + eval$bty_f2upper)/3
}

for (i in c(1:463)){
  eval$bty_m = (eval$bty_m1lower + eval$bty_m1upper + eval$bty_m2upper)/3
}

# We perform discretize operation for both attributes, grouped by gender.
# Now we analyse if depending on gender of students rating beauty, there
# is a significant change in the results.
# No values for ratings (0,2) interval. Therefore, we start "cutting" at 2.
eval$beauty_m <- cut(eval$bty_m, c(0,2,4,6,8,10))
eval$beauty_f <- cut(eval$bty_f, c(0,2,4,6,8,10))
table(eval$beauty_m)
table(eval$beauty_f)


## beauty_m and eval
## We start comparing rating of subjects and beauty of professors, rated by
# 3 students that are males.
# We plot a bar plot with beauty segments in x-axis.
ggplot(eval, aes(x = beauty_m, fill = NewEval)) +
  geom_bar() +
  xlab("Beauty (rated by men)") +
  ylab("Total Count") +
  labs(fill = "Rating") 


ggplot(eval, aes(x = beauty_m, y=..prop.., fill = NewEval, group=NewEval)) +
  geom_bar(position=position_dodge()) +
  xlab("Beauty (rated by men)") +
  ylab("Total Count") +
  labs(fill = "Rating")

## beauty_f and eval
# We compare both rating of subjects done by females (3 students) and
# course evaluations ratings.
ggplot(eval, aes(x = beauty_f, fill = NewEval)) +
  geom_bar() +
  xlab("Beauty (rated by women)") +
  ylab("Total Count") +
  labs(fill = "Rating") 


ggplot(eval, aes(x = beauty_f, y=..prop.., fill = NewEval, group=NewEval)) +
  geom_bar(position=position_dodge()) +
  xlab("Beauty (rated by women)") +
  ylab("Total Count") +
  labs(fill = "Rating")

# From these plots we can only say that women rate in a more optimistic way
# the beauty of their professors. Also, it can be remarked the fact that
# worst rated subjects are the ones with ugliest professors.

### 4. Explore the relationship between course evaluations and professors' 
# rank.
# We use previous data preparation of both attributes. We can directly
# explore on this data, as it was previously treated.
table(eval$rank)
table(eval$NewEval)


# We import ggplot2 library and plot an histogram with rating in x-axis and
# rank in the y-axis.
library("ggplot2")
ggplot(eval, aes(x = NewEval, fill = rank)) +
  geom_bar() +
  xlab("Rating") +
  ylab("Total Count") +
  labs(fill = "Rank") 

# Now we set one bar for each rank to have a better perspective.
ggplot(eval, aes(x = NewEval, y=..prop.., fill = rank, group=rank)) +
  geom_bar(position=position_dodge()) +
  xlab("Rating") +
  ylab("Total Count") +
  labs(fill = "Rank")

# Finally we draw a mosaic plot to show more in detail the linear dependency
# between rank and rating. It is so small that we can consider them as almost
# independent. Interval (2,3] is smaller only because there are less 
# observations.
table0=xtabs(~NewEval+rank, data=eval)
prop.table(table0,1)
mosaicplot(table0,shade=TRUE, type="pearson",main="Ratings dependency on rank")

## CONCLUSIONS: They are almost independent, as their linear dependecy is so small. 
# In this case, correlation between both attributes is 0.1.