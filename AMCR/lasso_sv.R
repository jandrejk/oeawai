##########################################################################
#                          Lasso Regression                              #
##########################################################################

# Linear model:
install.packages("ISLR")
library(ISLR)
data("Hitters")
d <- na.omit(Hitters)
names(d)
d$Salary <- log(d$Salary)
hi# Why take the log of Salary?

n <- nrow(d)

set.seed(124)
train <- sample(1:n,round(n/2))
test <- c(1:n)[-train]

model.lm <- lm(d$Salary~.,data=d, subset=train)
summary(model.lm)
plot(model.lm)

pred.lm <- predict(model.lm, newdata = d[test,])
#cor(d[test,""],pred.lm)^2
mean((d$Salary[test]-pred.lm)^2)
plot(d$Salary[test],pred.lm,xlab = "Measured y", ylab = "Predicted y")
abline(c(0,1))

install.packages("glmnet")
library("glmnet")
res <- glmnet(data.matrix(d[train,-19]),d[train,19])
plot(res)
res.cv <- cv.glmnet(data.matrix(d[train,-19]),d[train,19])
plot(res.cv)

coef(res.cv,s="lambda.1se")
pred.res.cv <- predict(res.cv, newx = data.matrix(d[test,-19]),s="lambda.1se")

plot(d[test,19],pred.res.cv,xlab = "Measured y", ylab = "Predicted y")
abline(c(0,1))
mean((d$Salary[test]-pred.res.cv)^2)
#model2.lm <- lm(d$Salary~d$Hits+d$Walks,data=d, subset=train)
#summary(model.lm)
