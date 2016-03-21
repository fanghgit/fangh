#Project
library(MASS)
library(DAAG)
library(glmnet)
library(pls)
#Preliminary Investigation
data = read.csv("~/academic/Sta207/project/mortality.csv", header = TRUE)
data = data[, 1:7]
sapply(data, class)
var = names(data)
par(mfrow = c(3, 3))
for(i in 1:length(var)){
  hist(data[, i], main = var[i], xlab = var[i])
}
#The distribution of MORTALITY, NONWHITE POOR NOX and SO2  are left skewed, so we consider logrithm transformation to them.
data$NONWHITE = log(data$NONWHITE)
data$POOR = log(data$POOR)
data$NOX = log(data$NOX)
data$SO2 = log(data$SO2)
data$MORTALITY = sqrt(data$MORTALITY)
#After logrithm transformation, their distributions are more normal

#Standerlization
data = as.data.frame(scale(data))

pairs(data)
cor(data)
#The effect of multicollinearility
X = as.matrix(data[,-7])
Y = as.matrix(data[,7])
eigen(t(X) %*% X)

#Calculate VIF
par(mfrow = c(1, 1))
fit = lm(MORTALITY ~ . -1, data = data)
summary(fit)
plot(fit, which = 1)
plot(fit, which = 2)
#There are nonlinear pattern in the residuals vs. fitted values plot, and the distribution of residuals is heavy-tailed.

vif(fit)
#D = t(X) %*% X
#diag(D) * diag(solve(D))

#Stepwise regression according to AIC criterion.
stepfit = stepAIC(fit, scope = list(upper = fit, lower = ~1), direction = "backward", k = 2)

fit_step_reg = lm(MORTALITY ~ PRECIP + EDUC + NONWHITE + NOX + SO2 - 1, data = data)
plot(fit_step_reg, which = 1)
plot(fit_step_reg, which = 2)



#ridge
par(mfrow = c(1, 1))

select(lm.ridge(MORTALITY ~ . -1, data = data, lambda = seq(0, 30, 0.01)))
#GCV plot
fit_ridge = lm.ridge(MORTALITY ~ . -1, data = data, lambda = 7.61)
coef(fit_ridge)
beta = matrix(coef(fit_ridge))
fv = X %*% beta
res = data$MORTALITY - fv
plot(fv, res, main = "residuals vs. fitted values", xlab = "fitted values", ylab = "residuals")
qqnorm(res)


#lasso
fit_lasso = cv.glmnet(X, Y, intercept = FALSE)
plot(fit_lasso)
fit_lasso$lambda.min
coef(fit_lasso)


yfit = predict(fit_lasso, newx = X)
par(mfrow = c(2, 2))
plot(yfit, Y, main = "obs vs. fitted", xlab = "fitted values", ylab = "observed values")
plot(yfit, Y - yfit, main = "residuals vs. fitted", xlab = "fitted values", ylab = "residuals")
abline(0, 0)
hist(Y - yfit, main = "hist of residuals", xlab = "residuals")
qqnorm(Y - yfit)


#Partial Least Square
set.seed(100)
fit_pls = plsr(MORTALITY ~ ., 6, data = data, validation = "CV")
summary(fit_pls)





#Model Selection
CV = function(train_data, test_data, method){
  if(method == "stepreg"){
    fit = lm(MORTALITY ~ . - 1, data = train_data)
    stepfit = stepAIC(fit)
    res = test_data$MORTALITY - predict(stepfit, test_data)
    return( sum(res^2)/nrow(test_data) )
  }
  else if(method == "ridge"){
    result = lm.ridge(MORTALITY ~ . - 1, data = data_train, lambda = seq(0, 30, 0.01))
    k_opt = 0.01 * which.min(result$GCV)
    fit = lm.ridge(MORTALITY ~ . - 1, data = data_train, lambda = k_opt)
    beta = matrix(coef(fit))
    X = as.matrix(test_data[, -7])
    fv = X %*% beta
    res = test_data$MORTALITY - fv
    return( sum(res^2)/nrow(test_data) )
  }
  else if(method == "lasso"){
    X = as.matrix(train_data[, -7])
    Y = as.matrix(train_data[, 7])
    fit = cv.glmnet(X, Y, intercept = FALSE)
    #l_opt = fit$lambda.min
    #fit = cv.glmnet(X, Y, lambda = l_opt, intercept = FALSE)
    X_pred = as.matrix(test_data[, -7])
    Y_pred = as.matrix(test_data[, 7])
    yfit = predict(fit, X_pred)
    res = Y_pred - yfit
    return( sum(res^2)/nrow(test_data) )
  }
  else if(method == "pls"){
    set.seed(100)
    fit_pls = plsr(MORTALITY ~ ., 6, data = data_train, validation = "CV")
    k_opt = which.min(fit_pls$validation$PRESS)
    fit = plsr(MORTALITY ~ ., k_opt, data = data_train, validation = "CV")
    #beta = as.matrix(coef(fit))
    #X_pred = as.matrix(test_data[, -7])
    #Y_pred = as.matrix(test_data[, 7])
    n = nrow(test_data)
    fv = predict(fit, newdata = test_data)[((k_opt - 1)*n + 1):(k_opt*n)]
    res = test_data$MORTALITY - fv
    return( sum(res^2)/nrow(test_data) )
  }
}

#Cross validation - 10 folders
set.seed(123)
data_new = sample(data)

getCV = function(method, data_new, nfolder){
  cv_value = rep(0, nfolder)
  n = floor(nrow(data_new)/nfolder)
  for(i in 1:nfolder){
    test_data = data_new[((i-1)*n +1):(i*n), ]
    train_data = data_new[-c(((i-1)*n +1):(i*n)), ]
    cv_value[i] = CV(train_data, test_data, method)
  }
  for(i in 1:nfolder){
    print(cv_value[i])
  }
  return(sum(cv_value)/nfolder)
}










