#installing required packages
install.packages("openxlsx")
install.packages("chisq.posthoc.test")

#loading packages
library(readxl)
library(openxlsx)
library(tidyr)
library(rstatix)
library(chisq.posthoc.test)

#answer1
#Since we are comparing means, we will be using a paired samples T-test.
#Null Hypothesis: There is no decline in the population of horseshoecrabs.
#Alternative: There is decline in the population of horseshoecrabs.
#reading the excel file
crabpop <- read_excel("A2.1-HorseshoeCrabs.xlsx")
#since we can't do much with the population figures of 2011 and 2012 in different groups, so we will gather them in one group called population.
crabpop1 <- gather(crabpop, key = "group", value = "population", Year.2011, Year.2012)
crabpop.grouping <- group_by(crabpop1, group)
#summary statistics
get_summary_stats(crabpop.grouping, population, type="mean_sd")
#since we are concerned about differences here we mutate the differences column
crabpop <- mutate(crabpop, difference = Year.2011 - Year.2012)
#identifying the outliers in the differences column
identify_outliers(crabpop,difference)
#finding the normality of the difference variable by Shapiro-Wilk Test
shapiro_test(crabpop, difference)
#conducting the t-test and finding the significance as well
t_test(crabpop1, population ~ group, paired = TRUE)
t.result <- t_test(crabpop1, population ~ group, paired = TRUE)
add_significance(t.result)
#finding the effect size by cohen's d
cohens_d(crabpop1, population~group, paired = TRUE)

#answer 2
car_sales_obs <- c(150, 65, 385, 400)
car_sales_exp <- c(0.18, 0.10, 0.35, 0.37)
chisq.test(x = car_sales_obs, p = car_sales_exp)

#answer 3
data3 <- read_excel("A2.2-HardWater.xlsx")
region.grouping <- group_by(data3, Region)
get_summary_stats(region.grouping, Mortality, type = "mean_sd")
identify_outliers(region.grouping, Mortality)
shapiro_test(region.grouping, Mortality)
levene_test(data3, Mortality~ Region)
t_test(data3, Mortality~ Region, var.equal = TRUE)
t.result <- t_test(data3, Mortality~ Region, var.equal = TRUE)
add_significance(t.result)
cohens_d(data3, Mortality~ Region, var.equal = TRUE)

#answer 4
data4 <- read_excel("A2.3-PopularKids.xlsx")
choices <- table(data4$Gender, data4$Goals)
chisq.test(x = choices, simulate.p.value = TRUE)
chisq.posthoc.test(choices)

#trial
sales_obs <- c(150, 65, 385, 400)
sales_exp <- c(0.18, 0.10, 0.35, 0.37)
chisq.test(x = sales_obs, p = sales_exp)

