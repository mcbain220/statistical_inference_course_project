
# load tooth growth data & create subsets for later analysis

require("ggplot2")
library(ggplot2)
library(datasets)
teeth <- ToothGrowth

teeth$dose <- as.factor(teeth$dose) 

supp_vc <- teeth[teeth$supp == "VC",]
supp_oj <- teeth[teeth$supp == "OJ",]

dose_05 <- teeth[teeth$dose == "0.5",]
dose_1 <- teeth[teeth$dose == 1,]
dose_2 <- teeth[teeth$dose == 2,]


# summaries and plots

summary(teeth)

qplot(dose,len,data=teeth,facets=.~supp)  # a quick scatterplot of each delivery method

plot1 <- ggplot(teeth, aes(supp,len))
plot1 <- plot1 + geom_boxplot()

plot2 <- ggplot(teeth, aes(dose, len))
plot2 <- plot2 + geom_boxplot()

mean(teeth$len)
var(teeth$len)
sd(teeth$len)

# t tests

supp_test_1 <- t.test(teeth$len, supp_vc$len, paired=FALSE)$conf.int
supp_test_1

supp_test_2 <- t.test(teeth$len, supp_oj$len, paired=FALSE)$conf.int
supp_test_2

dose_test_1<- t.test(teeth$len, dose_05$len, paired=FALSE)$conf.int
dose_test_1

dose_test_2 <- t.test(teeth$len, dose_1$len, paired=FALSE)$conf.int
dose_test_2

dose_test_3 <- t.test(teeth$len, dose_2$len, paired=FALSE)$conf.int
dose_test_3



