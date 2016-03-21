#Sta207 Assignment2
#Q18.15
#a.
data = read.csv("~/academic/Sta207/PR18.15.csv", header = TRUE, sep = ",")
data$Shift = as.factor(data$Shift)
aov.out = aov(Frequency ~ Shift, data = data)
#fitted value
aov.out$fitted.values
#residuals
aov.out$residuals

#b.
plot(aov.out, which = 1)
#Variances are not equal for four shifts

#c.
data_split = split(data, data$Shift)
for(i in 1:4){
  data_split[[i]][,"Frequency"] = abs(data_split[[i]][,"Frequency"] - median(data_split[[i]][,"Frequency"]))
}
data_new = as.data.frame(do.call("rbind", data_split))
aov.out_new = aov(Frequency ~ Shift, data = data_new)
#F* value is 1.701
#F(0.9, 3, 76) = 2.157293
#P-value is 0.174.
#F* < F(0.9, 3, 76), so we conlude that the equal variance assumption holds.
#This result is different from our diagnosis in part(b).

#d.
#sample mean
avg = with(data, tapply(Frequency, list(Shift), mean))
avg
#sample variance
std = with(data, tapply(Frequency, list(Shift), sd))
std
myTable = cbind(std^2/avg, std/avg, std/avg^2)
colnames(myTable) = c("s^2/avg", "s/avg", "s/avg^2")
myTable
#The relation s^2/avg is the most stable, so we should do Frequency = sqrt(Frequency) transformation.

#e.
boxcox.sse <- function(lambda, model)
{
  x  <- model.frame(model)[,2]
  y  <- model.frame(model)[,1]
  K2 <- prod(y)^( 1/length(y))            
  K1 <- 1 / (lambda * K2^(lambda - 1))     
  ifelse(lambda != 0,                     
         assign("W", K1 * (y^lambda - 1)),
         assign("W", K2 * log(y)))
  # Deviance = Residual Sum of Squares
  return(deviance(aov(W ~ x)))  
}

model = aov(Frequency+1 ~ Shift, data = data)

lambda = seq(-1, 1, .2)
sse = sapply(lambda, function(x) boxcox.sse(x, model))
result = as.data.frame(cbind(lambda, sse))
result
#SSE is small when lamba is around 0.5, so a square-root transformation is appropriate.

#f.
bartlettf = function(lambda){
  data_f = data
  n = nrow(data)
  k2 = (Reduce(`*`, data$Frequency + 1))^(1/n)
  k1 = k1 = 1/(lambda * k2^(lambda - 1))
  if(lambda != 0){
    data_f[,"Frequency"] = k1 * ((data[,"Frequency"] + 1)^lambda - 1)/lambda
  }
  else{
    data_f[,"Frequency"] = k2 * log(data[,"Frequency"] + 1)
  }
  result = bartlett.test(Frequency ~ Shift, data = data_f)
  return(result$statistic)
}

bartlett = sapply(lambda, bartlettf)
bartlett = unname(bartlett)
result = as.data.frame(cbind(lambda, bartlett))
result
#Get minimum at around 0.2, log transformation is useful.


Hartleyf = function(lambda){
  data_f = data
  n = nrow(data)
  k2 = (Reduce(`*`, data$Frequency))^(1/n)
  k1 = k1 = 1/(lambda * k2^(lambda - 1))
  if(lambda != 0){
    data_f[,"Frequency"] = k1 * ((data[,"Frequency"] + 1)^lambda - 1)/lambda
  }
  else{
    data_f[,"Frequency"] = k2 * log(data[,"Frequency"] + 1)
  }
  s = with(data_f, tapply(Frequency, list(Shift), var))
  return(max(s)/min(s))
}

Hartley = sapply(lambda, Hartleyf)
result = as.data.frame(cbind(lambda, Hartley))
result
#Similar to the bartlett test, we get our minimum statistics at 0.2

#18.16
#a.
data_new = data
data_new$Frequency = sqrt(data_new$Frequency)
aov.out_new = aov(Frequency ~ Shift, data = data_new)
aov.out_new$residuals

#b.
plot(aov.out_new, which = 1)
plot(aov.out_new, which = 2)

expvalue = rep(1, 80)
for(i in 1:80)
{
  expvalue[i] = qnorm((i - 0.375)/(80 + 0.25), 0, 1)
}
ordered_res = sort(aov.out_new$residuals, decreasing = FALSE)
cor(ordered_res, expvalue)
#
#The correlation coefficient is large, which shows that the transformation appears to be effictive

#c.
#H_0: \sigma_{ij}^2 = \sigma^2
#H_a: \sigma_{ij} are not all equal
data_split = split(data_new, data_new$Shift)
for(i in 1:4){
  data_split[[i]][,"Frequency"] = abs(data_split[[i]][,"Frequency"] - median(data_split[[i]][,"Frequency"]))
}
data_new2 = as.data.frame(do.call("rbind", data_split))
aov.out_new = aov(Frequency ~ Shift, data = data_new2)
summary(aov.out_new)
#F^* = MSTR_d/MSE_d, if F^* > F(0.9, 3, 76), we conclude H_a, otherwise we conclude H_0.
#F^* = 0.386 < 2.1, so we conclude H_0, which means that the equal variance assumption holds.
#This result is consistant with our conclusion in part(b).

#21.7
#a.
element = c(0.73, 0.86, 0.94, 1.40, 1.62, 0.67, 0.75, 0.81, 1.32, 1.41, 0.15, 0.21, 0.26, 0.75, 0.78)
block = rep(1:5, 3)
fat = rep(1:3, each = 5)
data = as.data.frame(cbind(block, fat, element))
data$block = as.factor(data$block)
data$fat = as.factor(data$fat)
#Because it is a feature of a person, it can not be changed and can not be randomly assigned.

#b.
aov.out = aov(element ~ block + fat, data = data)
aov.out$residuals
plot(aov.out, which = 1)
plot(aov.out, which = 2)
#There is nonlinear pattern in the residuals vs. fitted values plot.
#The residuals are heavy tailed.

#c.
with(data, interaction.plot(fat, block, element, main = "interaction plot"))
#The no-interaction assumption is does not hold.

#d.
#install.packages("additivityTests")
library(additivityTests)
data_matrix = matrix(element, 5, 3)
colnames(data_matrix) = c("1", "2", "3")
rownames(data_matrix) = c("1", "2", "3", "4", "5")
tukey.test(data_matrix, 0.01)
#F^* < F(0.99, 1, 7), we conclude H_0, D = 0
1 - pf(6.445, 1, 7)
#P-value = 0.039

#21.8
#a.
#ANOVA table
aov.out = aov(element ~ block + fat, data = data)
summary(aov.out)

#b.
treatment_means = with(data, tapply(element, list(fat), mean))
mse_root = 0.04914265
s = mse_root/sqrt(5)
upr = treatment_means + qt(0.975, 8)*s
lwr = treatment_means - qt(0.975, 8)*s
require(plotrix)
x = 1:3
F = rep(0, 3)
plotCI(treatment_means, ui = upr, li = lwr, ylab = "value")

#c.
summary(aov.out)
#H_0: t_i = 0, H_a: exist t_i \neq 0
#F^* = MSTR/MSE = 0.6601/0.0024 = 273.4
#If F^* > F(0.975, 2, 8), we conclude H_a, otherwise we conclude H_0
#F^* = 273.4 > F(0.975, 2, 8) = 6.059, so we conclude H_a, exist t_i \neq 0
#The P-value = 4.33e-08

#d.
#block_means = with(data, tapply(element, list(block), mean))
L1 = treatment_means[1] - treatment_means[2]
L2 = treatment_means[2] - treatment_means[3]
s = mse_root*sqrt(2)/sqrt(5)
B = qt(1 - 0.05/(2*2), 8)
L1_CI = c(L1 - B*s, L1 + B*s)
L2_CI = c(L2 - B*s, L2 + B*s)
L1_CI
#The confidence interval for L1 is (0.0325, 0.2035)
#The confidence interval for L2 is (0.4765, 0.6475)
#Both L1 and L2 are not equal to 0

#e.
#Since the main effect of control treatment is already significant, so a adding a standard diet will not do any help to our analysis.


#21.19
summary(aov.out)
nb = 5
r = 3
MSBL = 0.3547
MSBLTR = 0.0024
#According to our book, unbiased estimator
sr2_unbiased = ((nb-1)*MSBL + (nb)*(r-1)*MSBLTR)/(nb*r-1)
#According to our lecture
sr2 = (0.0193 + 1.4190)/(4+8)

E_unbiased = sr2_unbiased/MSBLTR
E = sr2/MSBLTR
df1 = 12
df2 = 8

E_modified_unbiased = (df2 + 1)*(df1 + 3)/((df2 + 3)*(df1 + 1))*E_unbiased
E_modified = (df2 + 1)*(df1 + 3)/((df2 + 3)*(df1 + 1))*E






