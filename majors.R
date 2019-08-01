# read in data
stem <- read.table(file = "http://grimshawville.byu.edu/STEMswitchers.csv", header = TRUE, sep = ",")

# check data
str(stem)

# change categorical variables to factors
stem$PrevCalc <- as.factor(stem$PrevCalc)
stem$Major <- as.factor(stem$Major)
stem$Gender <- as.factor(stem$Gender)

# visualize data
par(mfrow=c(1,3))
boxplot(StandardizedTest ~ Switcher, data = stem, main = "Test Scores vs. Switching",
        xlab = "Switched", ylab = "Test Score Index")
boxplot(ClassPractice ~ Switcher, data = stem, main = "Class Practice vs. Switching", 
        xlab = "Switched", ylab = "Class Practice")
boxplot(Teacher ~ Switcher, data = stem, main = "Teacher Quality vs. Switching", 
        xlab = "Switched", ylab = "Teacher Quality")

# create table of Switcher w/ qualitative variables
# library(MASS)
table(stem$PrevCalc, stem$Switcher)
prop.table(table(stem$PrevCalc, stem$Switcher), margin = 1)

table(stem$Major, stem$Switcher)
prop.table(table(stem$Major, stem$Switcher), margin = 1)

table(stem$Teacher, stem$Switcher)
prop.table(table(stem$Teacher, stem$Switcher), margin = 1)

table(stem$Gender, stem$Switcher)
prop.table(table(stem$Gender, stem$Switcher), margin = 1)

# Fit model:
# Switcher = beta0 + beta1 * PrevCalc + beta2 * StandardizedTest + beta3 * Major + beta4 * Teacher +
#            beta5 * ClassPractice + beta6 * Gender
stem_out <- glm(Switcher ~ PrevCalc + StandardizedTest + Major + Teacher + ClassPractice + Gender, 
                data = stem, family = "binomial")

summary(stem_out)

# make model more interpretable
exp(coef(stem_out)[-1])

# confidence interval
exp(confint(stem_out))

# look at PrevCalc anova for test statistic, p-value
stem_red <- glm(Switcher ~ StandardizedTest + Major + Teacher + ClassPractice + Gender, 
                data = stem, family = "binomial")

anova(stem_red, stem_out, test='Chisq')

# look at Major anova for test statistic, p-value
stem_red2 <- glm(Switcher ~ PrevCalc + StandardizedTest + Teacher + ClassPractice + Gender, 
                data = stem, family = "binomial")

anova(stem_red2, stem_out, test='Chisq')

# construct ROC curve
library(ROCR)
stem_pred<-prediction(predict(stem_out, type="response"), stem$Switcher)
stem_perf<-performance(stem_pred,measure="tpr",x.measure="fpr")
par(mfrow=c(1,1))
plot(stem_perf,xlab="1-specificity",ylab="sensitivity",main="ROC curve")
abline(0,1,col='grey')

# compute AUC
performance(stem_pred, measure="auc")
