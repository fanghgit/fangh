#HW5
#2
#(a)
property = read.table("~/academic/Sta206/property.txt", header = FALSE)
#colnames(property) = c("age", "operating_expense", "vacancy_rate", "total_sqaure_footage", "rental_rates")
colnames(property) = c("Y", "X1","X2", "X3", "X4")
par(mfrow = c(2,2))
invisible(
  sapply(c(2:5), function(x) hist(property[,x], main = paste0("Histogram of ", names(property)[x]), xlab = names(property)[x]))
)
invisible(
  sapply(c(2:5), function(x) boxplot(property[,x], main = paste0("Boxplot of ", names(property)[x]), xlab = names(property)[x]))
)
#The range of X1 is [0,20], mainly distributed on [0,5] and [10,20]
#The range of X2 is [2,16], mainly distributed on [4,14]
#The range of X3 is [0,0.8], mainly distributed on [0,0.3]
#The range of X4 is [0,3e+05], mainly distributed on [0,4e+05]
#The X variables do not appear to be in the same  scale


#(b)
options(digits = 3)
sample_mean = colMeans(property)
sample_std = apply(property, 2, sd)
n = nrow(property)
property_trans = scale(property)/sqrt(n-1)
property_trans = as.data.frame(property_trans)
head(property_trans)
#This our data after the correlation transformation
colMeans(property_trans)
#THe sample means of transformed variables are all 0.
apply(property_trans, 2, sd)
#The sample standard deviations of transformed variables are all 0.112 which is 1/(sqrt(n)-1)

#(c)
#The model equation: Y^*i = beta^*_1X^*_{i1} + beta^*_2X^*_{i2} + beta^*_3X^*_{i3} + beta^*_4X^*_{i4} + epsilon^*_i, i = 1,2,...,n

fit = lm(Y~ X1 + X2 + X3 + X4, data = property)
fit_trans = lm(Y~ X1 + X2 + X3 + X4, data = property_trans)
summary(fit_trans)$coefficient
#The intercept of the standardized first-order regression model is 0.
coef = summary(fit_trans)$coefficient
coef[,"Estimate"][-1]*sample_std["Y"]/sample_std[-1]
#This is the coefficients after the transformation from the fitted standardized regression coefficients.
summary(fit)$coefficient[,"Estimate"][-1]
#This is the coefficients of X1 .. X4 form Homework 4, Problem 4.
#We can find that our result is exactly the same as the result form Homework 4, Problem 4.
#(d)
coef[,"Std. Error"][-1]*sample_std["Y"]/sample_std[-1]
#This is the standard errors of coefficients after the transformation from the fitted standardized regression coefficients. 
summary(fit)$coefficient[,"Std. Error"][-1]
#This is the standard errors of coefficients from Homework 4, Problem 4.
anova(fit_trans)
#The SSTO_trans is 1, the SSE_trans is 0.415, the SSR_trans is 0.585.
anova(fit)
#The SSTO is 237, the SSE is 98.2, the SSR_trans is 138.8
#Although the value of these two anova tables are different, the value of SSR_trans/SSTO_trans and SSR/SSTO are the same.
#(f)
summary(fit_trans)$r.squared
summary(fit)$r.squared
summary(fit_trans)$adj.r.squared
summary(fit)$adj.r.squared
#We can find that the value of R^2 and R_a^2 in these two models are exactly the same.



#3.
#(a)
#r_XX
cor(property[-1])
cor()
X_trans = as.matrix(property_trans[-1])
Y_trans = as.matrix(property_trans[1])
t(X_trans)%*%X_trans
#Their results are exactly the same, so X^TX = r_XX
cor(Y_trans,X_trans)
t(t(X_trans)%*%Y_trans)
##Their results are exactly the same, so X^TY = r_XY

#(b)
invr = solve(t(X_trans)%*%X_trans)
invr
#This is r^-1_XX
VIF = diag(invr)
names(VIF) = colnames(invr)
VIF
#So VIF_1 = 1.24, VIF_2 = 1.65, VIF_3 = 1.32, VIF_4 = 1.41
#Define a function to get R_k
getrSquare = function(index){
  res_index = index
  var_index = c(1:4)[-index]
  data = property[,-1]
  fit = lm(data[,res_index] ~ data[,var_index[1]] + data[,var_index[2]] + data[,var_index[3]])
  return(summary(fit)$r.squared)
}

R_square = sapply(1:4, getrSquare)
VIF_confirm = 1/(1-R_square)
names(VIF_confirm) = colnames(invr)
#The value of VIF and VIF_confirm are exactly the same, so we confirmed that VIF_k = 1/(1-R^2_k)
#The value of max(VIF) is 1.65 which is quite small value for VIF, so the degree of multicollinearity is not high.

#(c)
fit_yx4 = lm(Y~X4, data = property) 
fit_yx3x4  =lm(Y~X3+X4, data = property)
summary(fit_yx4)$coefficients
summary(fit_yx3x4)$coefficients
#The estimated regression coefficients of X_4 in these two models are very close to each other.
anova(fit_yx4)
#SSR(X_4) = 67.8
anova(fit_yx3x4)
#SSR(X_4|X_3) = 66.9
#We find that the value of SSR(X_4) and SSR(X_4|X_3) are very close.
#Because the correlation of X_4 and X_3 is very small, so the change of X_4 cannot be explained by the change of X_3 and hat_var(X_3+X_4) $\approx $ hat_var(X_3)+hat_var(X_4)
#So SSR(X_4) $\approx $ SSR(X_4|X_3)

#(d)
fit_yx2 = lm(Y~X2, data = property) 
fit_yx2x4  =lm(Y~X2+X4, data = property)
summary(fit_yx2)$coefficients
summary(fit_yx2x4)$coefficients
#We find that the value of SSR(X_2) and SSR(X_4) are not close to each other.
#Same to the previous question, but the correlation of X_2 and X_4 are not small, so some proportion of the change of X_2 can be explained by X_4, and hat_var(X_2+X_4) cannot be saperate as hat_var(X_3)+hat_var(X_4)
#So SSR(X_2) is significant different from SSR(X_2|X_4)


#4
#(a)
par(mfrow = c(1,1))
plot(property$X1, property$Y, xlab = "X1(age of property)", ylab = "Y(rental rates)", main = "Y ~ X1")
#The relationship between Y and X1 is not obvious, probably not linear. maybe curvilinear

#(b)
#model equation: Y_i = beta_0 + beta_1X_{i1} + beta^*_2X^*_{i2} + beta^*_3X^*_{i3} + beta^*_4X^*_{i4} + epsilon^*_i, i = 1,2,...,n


property$X1_centered = property$X1 - mean(property$X1)
property$X1_quad = property$X1_centered^2
fit_quad = lm(Y~X1_centered+X2+X4+X1_quad, data = property)
#PPPPPPPP
#hat{Y}_i = 10.2 - 0.182X_1 + 0.314X_2 + 8.05$\times$10^{-6}X4 + 0.0141X1^2
plot(predict(fit_quad), property$Y, xlab = "fitted value", ylab = "Y", main = "fitted Y vs. Y")
#From the plot, we can find that the model provide a good fit.


#(c)
fit = lm(Y~X1+X2+X4, data = property)
summary(fit)$r.squared
summary(fit_quad)$r.squared
summary(fit)$adj.r.squared
summary(fit_quad)$adj.r.squared
#We can find that the value of R^2 and R_a^2 with above model are larger than those with model2 from homwork4.

#(d)
#Null hypothesis H_0: beta_4 = 0
#Alternative hypothesis H_a: beta_4 $\neq$ 0
#PPPPP
#test statistics: F^* = MSR(X_1^2|X_1,X_2,X_3)/MSE(Full)
#Null distribution: F^* ~ F(df(SSR(Full)-df(SSR(Reduced)),df(SSE(Full))) = F(1, 76)
#If F^* > F(0.95, 1, 76), conludes H_a, otherwise concludes H_0
#In our case, F^* = 7.1/(91.5/76) = 5.9 and F(0.95, 1, 76) = 3.97
#F* > F(0.95, 1, 76)
#So we conclude H_a: beta_4 $\neq$ 0, which means that the quadratic term cannot be dropped.

#(e)
newX = data.frame(X1_centered = 4-mean(property$X1), X2 = 10, X4 = 80000, X1_quad = (4-mean(property$X1)^2))
predict(fit_quad, newX, interval = "prediction", level = 0.99)
#The predict value is 13.9, the prediction interval is [10.6, 17.1]
#The prediction interval in Homework4, problem4 is [12.09, 18.15]
#Compared with the result in Homework4, our prediciton interval of the Polynomial Regression is wider.
