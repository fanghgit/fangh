#************************Sta206 HW7***************************
diabetes = read.table("~/academic/Sta206/diabetes.txt", header = TRUE)
#Q2.
#(a)
summary(diabetes$frame)
diabetes$frame[diabetes$frame == ""] = NA
diabetes$frame = factor(diabetes$frame)
levels(diabetes$frame)

#(b)
drops = c("id","bp.2s", "bp.2d")
data = diabetes[, !(names(diabetes) %in% drops)]

#(c)
data_class = sapply(data, class)
data_class
#Quantitative variables
names(data_class[data_class != "factor"])
#Qulitative variables
names(data_class[data_class == "factor"])
par(mfrow = c(1,1), mar = c(5,5,5,5), cex.lab = 1)
hist(data$glyhb, main = "The distribution of glyph", xlim = range(na.omit(data$glyhb)), xlab = "glyph")
summary(data$glyhb)
#The distribution of "glyhb" is right-skewed, and mainly distributed on [4, 6]
quant_var = names(data_class[data_class != "factor"])
rest_quant_var = quant_var[quant_var != "glyhb"]
par(mfrow = c(4,3), mar = c(2,2,2,2))
invisible(
  sapply(rest_quant_var, function(x) hist(data[[x]], main = x))
)
qual_var = names(data_class[data_class == "factor"])
par(mfrow = c(2,2))
invisible(
  sapply(qual_var, function(x) pie(table(data[[x]]), main = x))
)

#(d)
par(mfrow = c(2,2))
hist(log(data$glyhb), main = "log(glyhb)")
hist(sqrt(data$glyhb), main = "sqrt(glyhb)")
hist(1/(data$glyhb), main = "1/glyhb")
#1/glyhb appears to be most Normal like among the three.

#(e)
data$glyhb = 1/data$glyhb

#(f)
index.na=apply(is.na(data), 1, any)
data.s=data[index.na==FALSE,]
any(is.na(data.s))
dim(data.s)
table(data.s$frame)

#(g)
pairs_var = paste(quant_var, collapse = "+")
pairs_var = paste0("~", pairs_var)
pairs(~., data = data.s[, quant_var], cex = 0.1)
cor(data.s[, quant_var])
#I observe nonlinearity.

#(h)
par(mfrow = c(1,1), mar = c(4,4,4,4))
boxplot(glyhb ~ gender, data = data.s, main = "glyhb ~ gender")
#There is no obvious relationship between "glyhb" and "gender". 
boxplot(glyhb ~ frame, data = data.s, main = "glyhb ~ frame")
#There is a relationship between "glyhb" and "frame". 

#(i)
set.seed(10)
n.s=nrow(data.s)
index.s=sample(1: n.s, size=366/2, replace=FALSE)
data.c=data.s[index.s,]
data.v=data.s[-index.s,]

#(j)PPPPPPPPPPPPPPPP

par(mfrow = c(2,1))
hist(data.c$glyhb)
hist(data.v$glyhb)
#They approximately have the same distribution.

#Q3. PPPPPPPPPPPPPPP
#(a)
fit.full = lm(glyhb~., data = data.c)
summary(fit.full)
#There are 16 regression coefficients.
#MSE = 0.0364
library(MASS)
boxcox(fit.full)
#No need transformation

#(b)
library("leaps")

sub_set = regsubsets(glyhb~., data = data.c, nbest = 1, nvmax = 16, method = "exhaustive")
sum_sub = summary(sub_set)
sum_sub
p.m = as.integer(rownames(sum_sub$which)) + 1
n = nrow(data.c)
ssto = sum((data.c$glyhb-mean(data.c$glyhb))^2)
sse = (1-sum_sub$rsq)*ssto
aic = n*log(sse/n)+2*p.m
bic = n*log(sse/n)+log(n)*p.m
res_sub = cbind(sum_sub$which, sse, sum_sub$rsq, sum_sub$adjr2, sum_sub$cp, bic, aic)
colnames(res_sub)[19:21] = c("r2p", "r2ap", "cp")
min_for_each = lapply(c("r2p", "r2ap", "cp", "bic", "aic"), function(x) res_sub[order(res_sub[, x]),][1,])
names(min_for_each) = c("r2p", "r2ap", "cp", "bic", "aic")
min_for_each
#According to ....

#The minmum value of Cp is 0.05, it means that the sum of the squared prediction error is 0.05


#(c)
fit.null = lm(glyhb~1, data = data.c)
fit.full = lm(glyhb~., data = data.c)
stepAIC(fit.null, scope=list(upper=fit.full), direction="both", k=2)
#The best model is the model with variables "stab.glu", "age", "ratio", "waist", "time.ppn", "location", "chol". 
#This result is identified in the previous question.

#(d)
fs1 = lm(formula = glyhb ~ stab.glu + age + waist + ratio, data = data.c)
par(mfrow = c(1,1))
plot(fs1, which = 1, cex = 0.5)
#No obvious nonlinear pattern, but we observe a little heteroscedasticity.
plot(fs1, which = 2, cex = 0.5)
#The distribution of residuals is a little bit heavy-tailed.
#The model seems to be inadequate.  

#Q4
#(a)
fit.full = lm(glyhb~. + .^2, data = data.c)
sum_full = summary(fit.full)
nrow(sum_full$coefficients)
#There are 136 regression coefficients
#The MSE is 0.035
#There are many insignificant variables included in this model, and the variances of estimations are generally high, and the total in-sample variance is high.

#(b)
stepAIC(fit.null, scope=list(upper=fit.full), direction="forward", k=2)
#The model being selected is the model with varibles "stab.glu", "age", "ratio", "waist", "time.ppn", "location", "stab.glu:time.ppn", "stab.glu:age" and "age:ratio" 
fs2 = lm(formula = glyhb ~ stab.glu + age + ratio + waist + time.ppn + 
           location + stab.glu:time.ppn + stab.glu:age + age:ratio, 
         data = data.c)
#The AIC value in this model is just slightly samller than model fs1.

#(c)
plot(fs2, which = 1, cex = 0.5)
plot(fs2, which = 2, cex = 0.5)
#No obvious help, it still seems to be inadequate.

#(d)
#sub_set = regsubsets(glyhb~. + .^2, data = data.s, nbest = 1, nvmax = 16, method = "exhaustive", really.big = )
sum_sub = summary(sub_set)
sum_sub

#Q5
#(a)
#Model3
var = c("glyhb", "stab.glu", "age", "ratio", "waist", "time.ppn", "location", "chol")
#To get a more precise estimation of sigma2, we will use the full dataset "data.s"
data_model = data.s[,var]
fs3 = lm(glyhb~. + .^2, data = data_model3)
summary(fs3)
p = nrow(summary(fs3)$coefficients)
p
#There are 29
#MSE = 0.03564^2 = 0.00127021
sigma2 = 0.03564^2


#For Model2
summary(fs2)
#SSE = MSE * df(SSE) = 0.0364^2*173 = 0.2292181
#MSE = 0.0364^2 = 0.00132496
MSEp_2 = 0.0364^2
SSEp_2 = MSEp_2*173
#Cp for Model2
Cp_2 = SSEp_2/sigma2 - (n-2*p)
Cp_2
#Calculat Pressp
Pressp_2 = sum(fs2$residuals^2/(1-influence(fs2)$hat^2))
Pressp_2

#For Model1
summary(fs1)
#SSE = MSE * df(SSE) = 0.0369^2*175 = 0.2382818
#MSE = 0.0369^2 = 0.00136161
MSEp_1 = 0.03611^2
SSEp_1 = MSEp_1*175
#Cp for Model2
Cp_1 = SSEp_1/sigma2 - (n-2*p)
Cp_1
#Calculat Pressp
Pressp_1 = sum(fs1$residuals^2/(1-influence(fs1)$hat^2))
Pressp_1
#The value of SSE, MSE, Cp, Press for fs1 and fs2 are close to each other, and fs2 is a little better than fs1 since all these values are smaller in fs2.
#Overfitting is not a big concern since the value of Pressp/n is relatively small.

#(b)
fs1_v = lm(formula = glyhb ~ stab.glu + age + ratio + waist + time.ppn + location + chol, data = data.v)
sign(summary(fs1_v)$coefficients[,1]) == sign(summary(fs1)$coefficients[,1])
fs2_v = lm(formula = glyhb ~ stab.glu + age + ratio + waist + time.ppn + 
             location + stab.glu:time.ppn + stab.glu:age + age:ratio, 
#For Model1, the sign of location is different between   training and validation set.
           data = data.v)
sign(summary(fs2_v)$coefficients[,1]) == sign(summary(fs2)$coefficients[,1])
#For Model2, the sign of ratio is different between training and validation sets.

mod_sum1 = cbind(coef(summary(fs1))[,1], coef(summary(fs1_v))[,1],
                coef(summary(fs1))[,2], coef(summary(fs1_v))[,2])
colnames(mod_sum1) = c("Train Est","Valid Est","Train s.e.","Valid s.e.")
mod_sum2 = cbind(coef(summary(fs2))[,1], coef(summary(fs2_v))[,1],
                coef(summary(fs2))[,2], coef(summary(fs2_v))[,2])
colnames(mod_sum2) = c("Train Est","Valid Est","Train s.e.","Valid s.e.")
list_mod_sum = list(mod_sum1, mod_sum2)
names(list_mod_sum) = c("fs1", "fs2")
list_mod_sum
#Most the values of regression coefficients are similar
#The standard errors of these estimation is similar between training and validation sets.
#It appears that fs1 and fs2 have consistant estimates on training and validation data sets.

#For Model1 
newdata = data.v[, -5]
MSPE_1 = mean((data.v$glyhb - predict(fs1, newdata))^2)
MSPE_2 = mean((data.v$glyhb - predict(fs2, newdata))^2)
MSPE = c(MSPE_1, MSPE_2)
SSEp = c(SSEp_1/n, SSEp_2/n)
Pressp = c(Pressp_1/n, Pressp_2/n)
crit = cbind(MSPE, SSEp, Pressp)
rownames(crit) = c("fs1", "fs2")
crit
#The value of "MSPE", "SSEp" and "Pressp" are close to each other.
#Model2 has smaller MSPE

#(c)
#Internal validation is based on Pressp, and external validation is base on MSPE, either we consider "MSPE" or "Pressp", fs2 Model2 has a better result.
#So I would choose model2 as my final model
fit.final = lm(formula = glyhb ~ stab.glu + age + ratio + waist + time.ppn + 
           location + stab.glu:time.ppn + stab.glu:age + age:ratio, 
         data = data.s)
#The fitted regression function is ....
summary(fit.final)
anova(fit.final)


#Q6
#(a)
plot(fit.final, which = 1, cex =0.5)
#No obvious nonlinear pattern, but we observe is a lttle heteroscedasticity.
plot(fit.final, which = 2, cex = 0.5)
#Approximately Normal distribution, but heavy-tailed.

#(b)
n = nrow(data.s)
stu.res.del = studres(fit.final)
test = qt(1-0.1/(2*n), n-10-1)
stu.res.del[abs(stu.res.del) > test]
#There are 3 outlying Y observations, case 37, 334 and 363.

#(c)
h = as.vector(influence(fit.final)$hat)
index.X = which(h>(2*mean(h)))
#These are the index of outlying X observations. totally 28 cases.
#plot(residuals(fit.final)~h, xlab = "Leverage", ylab = "Residuals", main = "Residuals ~ Leverage", cex = 0.5)
plot(fit.final, which = 5, cex = 0.5)

#(d)
res = residuals(fit.final)
mse = anova(fit.final)["Residuals", 3]
p = nrow(coef(summary(fit.final)))
cook.d = res^2*h/(p*mse*(1-h)^2)
plot(fit.final, which = 4)

head(round(sort(pf(cook.d, p, n-p), decreasing = TRUE),5))
#The maximum of pi is about 1.66%, so there is no obvious influential cases.


#(e)
#With all cases.
fv_all = predict(fit.final)
fv_all_drop = fv_all[names(fv_all) != "195"]  #Drop the case 195 in order to compare with model without case 195.

data_drop = data.s[rownames(data.s) != "195",]
fit.final_drop = lm(formula = glyhb ~ stab.glu + age + ratio + waist + time.ppn + 
                 location + stab.glu:time.ppn + stab.glu:age + age:ratio, 
               data = data_drop)
fv_all_without = predict(fit.final_drop)
avg_abs_per_diff = mean(abs(fv_all_drop/fv_all_without-1))
avg_abs_per_diff
#So the average absolue percentage difference is 0.47%,



set.seed(110)
sample(1:10)
