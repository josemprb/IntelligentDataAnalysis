setwd("C:/DATOS/MasterEIT/EntryYear/1Semester/IntelligentDataAnalysis/Labs/HomeDir")

# Importing data set
cars=read.table("cars-PCA.txt")
colnames(cars)=c("mpg","cylinders","engine_displacement","horsepower",
                    "weight","acceleration","model_year","origin","car_name")

#####
# 1.2.1
### a) Choose a quantitative variable and explore its distribution in terms of 
# descriptive measures of center, dispersion, skewness and kurtosis. Is a normal 
# model a plausible one for its distribution? If the answer is no, can you think 
# of a transformation of the variable that improves normality. Are there any 
# outliers?





### b) Choose two quantitative variables and describe its joint bivariate 
# distribution. Does it seem to be Normal? Are there any outliers?




### c) Choose a subset of 4 or 5 quantitative variables and explore linear 
# relationships through:
# --> R matrix of pairwise correlations


# --> Matrix of partial correlations


# --> Coefficient of determination (function r2multv() we define in R)


# --> The determinant of R (correlation matrix) as an overall measure of 
#linear relationships.


# --> An eigenanalysis of matrix R, looking for really small eigenvalues.



##### 
# 1.2.2 Permutation test
install.packages("devtools")
install.packages("ggpubr")

library("devtools")
library("ggpubr")
library("dplyr")

# We first load the data contained in RestaurantTips.rda
load("RestaurantTips.rda")

### A)
## A1) Choose variables Bill and PctTip to analyse their linear dependency through Pearson's
# correlation coefficient. Just looking at the scatterplot, it is hard to tell whether this
# coefficient is significantly different from zero (check this!). 

cor.test(RestaurantTips$Bill, RestaurantTips$PctTip, method= "pearson")

ggscatter(RestaurantTips, x = "Bill", y = "PctTip", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Total Bill", ylab = "Percentage of Tip")

# The correlation coefficient is 0.14, different from 0. This means that correlation between 
# Bill and PctTip is different from 0. Therefore, there's a linear relationship between the 
# amount paid as bill, and the tip % that people gives after knowing such bill. This is also
# verified when p-value is bigger than significance value (0.05).

## A2) Conduct a permutation test to test the null hypothesis that the correlation coefficient 
# is 0 vs the alternative that it is different from 0. Run R = 10000 simulations.

# We follow the guide given to do the permutation test. First of all, we compute the observed
# correlation between the variables Bill and PctTip.
Ptest_Tips <- select(RestaurantTips, Bill, PctTip)
r_obs = cor(RestaurantTips$Bill, RestaurantTips$PctTip) # r_obs = 0.135

# Then we set number of permutations up to R = 10000 and use set.seed command.
R = 10000
set.seed(1)
# Second and third steps, permute Bill's among PctTip's, and do it 10000. Then,
# calculate r correlation value between both variables. It is stored in r array.

for (i in 1:R) {
  Tip_Rep <- data.frame("PctTip" = RestaurantTips$PctTip, "Bill" = sample(RestaurantTips$Bill,157,TRUE))
  r_aux = cor(Tip_Rep)
  r[i] = r_aux[1,2]
  aux[i] <- ifelse(r[i]>r_obs,1,0)
}

p_value_A = sum(aux)/R

# We import ggplot2 library and plot a bar plot that differentiates 
# bills and percentage of tip's given.
RestaurantTips$PctTip_Aux <- cut(RestaurantTips$PctTip, c(0,10,20,30,40))
RestaurantTips$Bill_Aux <- cut(RestaurantTips$Bill, c(0,15,30,45,60,75))

table(RestaurantTips$PctTip_Aux)
table(RestaurantTips$Bill_Aux)

ggplot(RestaurantTips, aes(x = PctTip_Aux, y=..prop.., fill = Bill_Aux, group=Bill_Aux)) +
  geom_bar(position=position_dodge()) +
  xlab("% of Tips") +
  ylab("Total Count") +
  labs(fill = "Total Bill")

### B) Repeat the analysis deleting the values for three customers that left a tip greater than
# 30% of the bill. These generous customers seem to be outliers.

## B1) Repeat the correlation test, with its scatterplot.
# Include the filter
TipsFiltered <- filter(RestaurantTips, PctTip < 30)

cor.test(TipsFiltered$Bill, TipsFiltered$PctTip, method= "pearson")

ggscatter(TipsFiltered, x = "Bill", y = "PctTip", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Total Bill", ylab = "Percentage of Tip")

# When plotting, we realyse that those three values were outliers as distribution keeps the
# same without them. Correlation between variables increase as outliers usually decrease this value.
# Also, p-value decreases but still is below 0.05, so we select the null hypothesis (H0).

## B2) Conduct again the permutation test, without the outliers.

# We follow the guide given to do the permutation test. First of all, we compute the observed
# correlation between the variables Bill and PctTip.
Ptest_Tips <- select(TipsFiltered, Bill, PctTip)
r_obs = cor(TipsFiltered$Bill, TipsFiltered$PctTip) # r_obs = 0.2198

R = 10000
set.seed(1)
# Second and third steps, permute Bill's among PctTip's, and do it 10000. Then,
# calculate r correlation value between both variables. It is stored in r array.
for (i in 1:R) {
  Tip_Rep_B <- data.frame("PctTip" = TipsFiltered$PctTip, "Bill" = sample(TipsFiltered$Bill,154,TRUE))
  r_aux = cor(Tip_Rep_B)
  r[i] = r_aux[1,2]
  aux[i] <- ifelse(r[i]>r_obs,1,0)
}

# Compute the p-value
p_value_B = sum(aux)/R

TipsFiltered$PctTip_Aux <- cut(TipsFiltered$PctTip, c(0,10,20,30))
TipsFiltered$Bill_Aux <- cut(TipsFiltered$Bill, c(0,15,30,45,60,75))

table(TipsFiltered$PctTip_Aux)
table(TipsFiltered$Bill_Aux)

ggplot(TipsFiltered, aes(x = PctTip_Aux, y=..prop.., fill = Bill_Aux, group=Bill_Aux)) +
  geom_bar(position=position_dodge()) +
  xlab("% of Tips") +
  ylab("Total Count") +
  labs(fill = "Total Bill")
