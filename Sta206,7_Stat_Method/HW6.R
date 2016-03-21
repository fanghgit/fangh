#Q1
#(a)
property = read.table("~/academic/Sta206/property.txt", header = FALSE)
#colnames(property) = c("age", "operating_expense", "vacancy_rate", "total_sqaure_footage", "rental_rates")
colnames(property) = c("Y", "X1","X2", "X3", "X4")
fit = lm(Y ~ X1 + X2 + X4 + X3, data = property)
summary(fit)
#This is the summary of our regression of Y on X_1, X_2, X_3, X_4.

#(b)
#The fitted regression coefficient of X_3
anova(fit)
fit2 = lm(Y ~ X1 + X2 + X4,data = property)
fit3 = lm(X3 ~ X1 + X2 + X4,data = property)
res2 = residuals(fit2)
res3 = residuals(fit3)
lm(res2 ~ res3)
plot(res2~res3, main = "Added-variable plot for X3: slope 0.619", xlab = "e(X3|others)", ylab = "e(Y|others)", cex = 0.3)
abline(lm(res2~res3))
abline(0, 0, lty = 2)

#(e)
anova(lm(res2 ~ res3))

#(f)
cor(res2,res3)

#(g)
lm(property$Y ~ res3)$coefficients
par(mfrow = c(2,1))
plot(property$Y~res3,cex = 0.5)
abline(lm(property$Y~res3))
plot(res2~res3, cex = 0.5)
abline(lm(res2~res3))


#2
n=30  ##sample size
X=seq(-3,3,length.out=n) ## design points: fixed throughout the simulation 
f.X=sin(X) + sin(2*X)  ## the values of the true regression function on the design points.
par(mfrow = c(1,1))
plot(X, f.X, type='l',  xlab="x", ylab="f(x)", col=1,  main="true regression function") ## look at the true regression function

sigma.e = c(0.5, 2, 5)
rep=1000
Y=array(0, dim = c(n, rep, length(sigma.e)))

for (k in 1:rep){
  for (m in 1: length(sigma.e)){
    set.seed(k + 10000*m)    ##set seed for the random number generator; for reproducibility of the result 
    e.c=rnorm(n,0,sigma.e[m]) ##generate the random errors
    Y.c=f.X+e.c   ## generate observations for kth replicate: true mean response + error 
    Y[,k,m]=Y.c
  }
}

par(mfrow=c(3,3)) ## create a plot with 3 by 3 panels
for (k in 1:3){
  for(m in 1:3){
    plot(X, f.X, type='l', xlab="x", ylab="f(x)", ylim=range(Y), col=1, main=paste("rep",k, "sigma",sigma.e[m]), cex = 0.5) ## true regression function; same across replicates
    Y.c=Y[,k,m]  
    points(X, Y.c)  ## observations of the kth replicate
  }
}  

par(mfrow=c(1,1))

l.order=c(1,2,3,5,7,9) ## order of the polynomial models to be fitted
Y.fit=array(0, dim=c(n,rep,length(sigma.e),length(l.order))) ## record the fitted values; 1st index corresponds to cases; 2nd index corresponds to replicates, 3rd index corresponds to models

for (k in 1:rep){
  for(m in 1:length(sigma.e)){
    Y.c=Y[,k,m] ##observations of the kth replicate
    for (l in 1:length(l.order)){
      fit.c=lm(Y.c ~ poly(X, l.order[l], raw=TRUE)) ## fit a polynomial model with order l.order[l]; raw=TRUE means raw polynomial is used; raw= FALSE mean orthogonal polynomial is used
      Y.fit[,k,m,l]=fitted(fit.c)
    } ## end of l loop
  }## end of m loop
}## end of k loop



label.m=paste(l.order,"order") ## label for each model 
par(mfrow=c(3,3)) ## create a plot with 2 by 2 panels
for (k in 1:3){
  for(m in 1:3){
    plot(X, f.X, type='l', xlab="x", ylab="f(x)", lwd=2.5, ylim=range(Y[,1:3,m]),main=paste("rep",k,"sig",sigma.e[m])) ##true regression function (true mean response curve)
  
    Y.c=Y[,k,m]  
    points(X, Y.c)  ## observations of the kth replicate
  
    for (l in 1:length(l.order)){
      points(X, Y.fit[,k,m,l], type='l', col=l+1, lty=l+1, lwd=1.5) ## fitted regression function (fitted mean response curve)
    }## end of l loop
  
    legend(x=-1, y=37,legend=c("true", label.m), col=1:(length(l.order)+1), lty=1:(length(l.order)+1), cex=0.5) ## legend for the plot
  }
}## end of k loop

par(mfrow=c(1,1))

Y.fit.ave=apply(Y.fit, c(1,3,4), mean) ## average across  replicates (2nd index)

par(mfrow=c(3,3))

for (l in 4:length(l.order)){
  for(m in 1:3){
    plot(X, f.X, type='n', xlab="x", ylab="f(x)", ylim=range(Y.fit), main=paste(l.order[l],"order","sig",sigma.e[m])) ## set plot axis label/limit, title, etc.
  
    for (k in 1:rep){

      points(X, Y.fit[,k,m,l], type='l', lwd=1, col=grey(0.6)) ## fitted response curves of lth model: grey
    }## end of k loop
  
    points(X, f.X, type='l',  col=1) ## true mean response: solid black
    points(X, Y.fit.ave[,m,l], type='l', col=2, lty=2) ## averaged (across replicates) fitted mean reponse of the lth model: broken red
  
    legend(x=-0.5,y=20, legend=c("true", "ave.fit"), col=c(1,2), lty=c(1,2), cex = 0.5) ## legend of the plot
  }
}##end l loop


SSE=array(0, dim=c(rep, length(sigma.e), length(l.order))) ## record SSE for each model on each replicate
resi=array(0, dim=c(n,rep, length(sigma.e), length(l.order))) ## record residuals : residual := obs-fitted
error.fit=array(0, dim=c(n,rep, length(sigma.e), length(l.order))) ## record estimation errors in the fitted values: error := fitted value - true mean response

for (l in 1:length(l.order)){
  #for(m in 1:length(sigma.e)){
  temp=Y-Y.fit[,,,l]
  resi[,,,l]=temp ## residuals
  SSE[,,l]=apply(temp^2,c(2,3), sum) ## SSE=sum of squared residuals across cases
  error.fit[,,,l]=Y.fit[,,,l]-array(f.X, dim=c(n, rep,length(sigma.e))) ## estimation error = fitted value - true mean response
  #}
}

SSE.mean=apply(SSE,c(2,3),mean) ## mean SSE (averaged over the replicates); this is the empirical version of E(SSE)
bias=apply(error.fit, c(1,3,4), mean)  ## bias= mean (averaged across replicates) errors in the fitted values
variance=apply(Y.fit, c(1,3,4), var) ## variance (across replicates) of the fitted values
err2.mean=apply(error.fit^2,c(1,3,4), mean) ## mean-squared-estimation errors: squared estimation errors of the fitted values averaged across replicates



bias2.sum=apply(bias^2, c(2,3), sum) ## average bias^2 across design points  for each model: overall in-sample bias
variance.sum=apply(variance, c(2,3), sum) ## average variance across design points for each model: overall in-sample variance
err2.mean.sum=apply(err2.mean,c(2,3), sum) ## average mean-squared-estimation-error across design points for each model: over-all in-sample msee




#Q3
cars = read.csv("~/academic/Sta206/Cars.csv", header = TRUE)
a = as.matrix(cars)
#(a)
pairs(~mpg+cylinders+displacement+horsepower+weight+acceleration+year.of.make+country.code, data=cars, cex = 0.3)
sapply(cars, class)


#(c)
cars$horsepower = as.numeric(as.character(cars$horsepower))
cars$country.code = as.factor(cars$country.code)

#(d)
par(mfrow = c(3,3))
quant_varablies = c("mpg", "cylinders","displacement", "horsepower", "weight", "acceleration", "year.of.make")
invisible(
  sapply(quant_varablies, function(x) hist(cars[[x]], main = paste("hist of", x), xlab = x))
)
par(mfrow = c(1,1))
fit = lm(mpg~., data = cars)
library(MASS)
boxcox(fit, main = "boxcox plot")
#cars$mpg = log(cars$mpg)

cars$mpg = log(cars$mpg)
pairs(~mpg+cylinders+displacement+horsepower+weight+acceleration+year.of.make, data = cars, cex = 0.3)

lab = c("1 62.5%", "2 17.6%", "3 19.9%")
pie(table(cars$country.code),labels=lab,col=c("blue","purple","green"), main="pie chart example")

boxplot(mpg ~ country.code, data = cars, main = "mpg ~ country.code", ylab = "mpg", xlab = "country.code")
fit = lm(mpg ~ cylinders + displacement + horsepower + weight + acceleration + year.of.make + factor(country.code), na.action = na.omit, data = cars)
plot(fit,which=1, cex = 0.5)
plot(fit,which=2, cex = 0.5)

cars_new = cars[!is.na(cars$horsepower),]
diag(solve(cor(cars_new[,-8])))



