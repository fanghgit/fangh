#Assignment 3
#Huang Fang  ID:913439658
digitsTrain = read.csv("~/academic/Sta141/digitsTrain.csv")

getImage =
  function(vals)
  {
    matrix(as.integer(vals), 28, 28, byrow = TRUE)
  }

draw = function(vals, colors = rgb((255:0)/255, (255:0)/255, (255:0)/255), ...)
{
  if(!is.matrix(vals))
    vals = getImage(vals)
  
  m = t(vals)  # transpose the image
  m = m[,nrow(m):1]  # turn up-side-down
  
  image(m, col = colors, ..., xaxt = "n", yaxt = "n")
}

#Randomization
set.seed(123)
permutation = sample(nrow(digitsTrain))
dist_data = dist(digitsTrain[permutation,-1], method = "euclidean")
dist_matrix = as.matrix(dist_data)
#After getting the distance matrix, the value of pixels are not useful to us anymore.
#In order to simplify the computation, we only take the information that is useful for us, the "label" and their corresponding "index"
sample_data = digitsTrain[permutation,1]
#Set the names of sample_data as index.
names(sample_data) = as.character(permutation)

#Divide our sample into 5 groups in order to apply cross validation
cross_validation_index = lapply(c(1:5), function(x) list(as.character(permutation[c((1000*x-999):(1000*x))]), as.character(permutation[-c((1000*x-999):(1000*x))])))

#By 
#Design a function to get prediction of test set. The input of our function is training set, test set and the value of k.
knn = function(dist_matrix, training_set_index, test_set_index, k){
  #Simplify the dist_matrix for this test_set and training_set.
  dist_submatrix = dist_matrix[test_set_index, training_set_index]
  pred = apply(dist_submatrix, 1, function(x) find_mostcommon(x, k))
  result = cbind(sample_data[test_set_index], pred)
  colnames(result) = c("true label", "prediction")
  result = as.data.frame(result)
  return(result)
}

#Design a function to find the most common digits for each observation among their k-nearest digits.
find_mostcommon = function(dist_vector, k){
  k_nearest_dist = sort(dist_vector, decreasing = FALSE)[1:k]
  k_nearest_index = names(k_nearest_dist)
  k_nearest_label = sample_data[k_nearest_index]
  k_nearest_table = table(k_nearest_label)
  #breaking tie
  if(length(k_nearest_table[as.logical(k_nearest_table == max(k_nearest_table))]) != 1){
      find_mostcommon(dist_vector, k+1)
  }
  #Find the most common digits among the k-nearest observations.
  else{
    return(as.integer(names(sort(k_nearest_table, decreasing = TRUE)[1])))
  }
}  

CV_false_rate = function(k, dist_matrix){
  pred_result = lapply(cross_validation_index, function(x) knn(dist_matrix, x[[2]], x[[1]], k))
  true_pred = do.call(rbind, pred_result)
  return(table(true_pred[,1] == true_pred[,2])[1]/nrow(true_pred))
}

#To get time of computation


#Design our final function to apply different distance metrcs
false_rate_metric = function(n, dist_metric){
  dist_data = dist(digitsTrain[permutation,-1], method = dist_metric)
  dist_matrix = as.matrix(dist_data)
  return(sapply(c(1:n), function(x) CV_false_rate(x, dist_matrix)))
}

false_rate_eculidean = false_rate_metric(50, "euclidean")
names(false_rate_eculidean) = paste0("eculidean, k=", as.character(c(1:50)))
false_rate_manhattan = false_rate_metric(50, "manhattan")
names(false_rate_manhattan) = paste0("manhattan, k=", as.character(c(1:50)))
false_rate_bind = c(false_rate_eculidean, false_rate_manhattan)
false_rate_bind[false_rate_bind == min(false_rate_bind)]

#Draw a plot showing the overall cross-validation misclassification rate versus k and the distance metrics.
plot(c(1:50), false_rate_eculidean, main = "misclassification rate vs. k", xlab = "k" , ylab = "misclassification rate", ylim = range(false_rate_bind), pch = 17, col = "red", cex = 0.7)
points(c(1:50), false_rate_manhattan, pch = 16, col = "blue", cex = 0.7)
legend("bottomright", legend = c("eculidean", "manhattan"), col = c("red", "blue"), pch = c(17, 16))

#confusion matrix
dist_data = dist(digitsTrain[permutation,-1], method = "euclidean")
dist_matrix = as.matrix(dist_data)
k = 3
pred_result = lapply(cross_validation_index, function(x) knn(dist_matrix, x[[2]], x[[1]], k))
pred = do.call(rbind, pred_result)
dim(pred)
confusion_matrix = table("true digits" = pred[, 1] , "predictions" = pred[, 2] )
confusion_matrix

#(4)
mis_rate = 1 - diag(confusion_matrix)/rowSums(confusion_matrix)
mis_rate
sort(mis_rate, decreasing = TRUE)[1]
sort(mis_rate, decreasing = FALSE)[1]

#(5)
mis_rate_col = 1 - diag(confusion_matrix)/colSums(confusion_matrix)
mis_rate_col
sort(mis_rate_col, decreasing = TRUE)[1]

#(6)
mis_pred = pred[pred[, 1] != pred[, 2], ]
mis_index = rownames(mis_pred)
par(mfrow = c(10, 10), mar = rep(0, 4))
invisible(sapply(1:100, function(x) draw(digitsTrain[mis_index[x], -1])))

#This is the image of the first 100 misclassified images, from these images, we can find that some of them are even difficult for a human to classify, for example, the image in the 1st row and 1st col, it can hardly be recognized as "2", and the image in the 2nd row and 4th col, if its label is not given, we can not tell which digit it represents.  
#hese observations are misclassified mainly because they are not well written, and they are significant different from its standard format.  
#Beyond these external reasons, some digits are not easy to distinguish by our normal knn algorithm, for example, human are easy to tell the difference between "4" and "9", however, in our algorithm, the distance between "4" and "9" is usually small.  

#TASK2
find_label = function(obs, avg_pixel, metric){
  obs_dist = sapply(avg_pixel, function(x) dist(rbind(x, obs[-1]), method = metric))
  #Find the minimum distance and its corresponding label
  pred_label = names(sort(obs_dist, decreasing = FALSE))[1]
  return(as.integer(pred_label))
}

distavg = function(training_set_index, test_set_index, metric){
  split_label = split(digitsTrain[training_set_index,], digitsTrain[training_set_index, 1])
  avg_pixel = lapply(split_label, function(x) colMeans(x[,-1]))
  test_data = digitsTrain[test_set_index,]
  pred = apply(test_data, 1,  function(x) find_label(x, avg_pixel, metric))
  result = cbind(sample_data[test_set_index], pred)
  colnames(result) = c("true label", "prediction")
  result = as.data.frame(result)
  return(result)
}


CV_falserate_distavg = function(cross_validation_index, metric){
  CV_pred = lapply(cross_validation_index, function(x) distavg(x[[2]], x[[1]], metric))
  CV_result = do.call(rbind, CV_pred)
  return(table(CV_result[, 1] == CV_result[, 2])[1]/nrow(CV_result))
}

system.time(CV_falserate_distavg(cross_validation_index, "euclidean"))
avg_mis_rate_euclidean = CV_falserate_distavg(cross_validation_index, "euclidean")
names(avg_mis_rate_euclidean) = "euclidean"
avg_mis_rate_manhattan = CV_falserate_distavg(cross_validation_index, "manhattan")
names(avg_mis_rate_manhattan) = "manhattan"
sort(c(avg_mis_rate_euclidean, avg_mis_rate_manhattan), decreasing = FALSE)[1]


