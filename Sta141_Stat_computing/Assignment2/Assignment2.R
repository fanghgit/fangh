#************Assignment 2******************
install.packages("RColorBrewer")
library(RColorBrewer)
library(maps)
#Step 1
#define our function to get the dataframe we want
get_dataframe = function(path, name){
  #ll = readLines("~/academic/Sta141/NASA/cloudhigh1.txt")
  data = readLines(path)
  data_time = data[5]
  data_time = strsplit(data_time," +")[[1]][4]
  #ll_time = as.Date(strsplit(ll_time, " ")[[1]][1], "%d-%b-%Y")
  data_long = data[6]
  data_long = strsplit(data_long, " +")[[1]][-1]
  #ll_long = as.numeric(gsub("W","",ll_long))
  data_lat_lines =data[substring(data, 1 ,3) != "   "]
  data_lat = unlist(lapply(data_lat_lines, function(x) gsub(" ", "", strsplit(x, " /")[[1]][1])))
  long_lat = outer(data_long, data_lat, paste)
  long = unlist(lapply(long_lat, function(x) strsplit(x, " ")[[1]][1]))
  lat = unlist(lapply(long_lat, function(x) strsplit(x, " ")[[1]][2]))
  time = rep(data_time,length(long))
  result = data.frame(cbind(time, long, lat))
  result$time = as.Date(result$time, "%d-%b-%Y")

  tmp = (lapply(data[-c(1:7)], function(x) strsplit(sapply(x, function(y) strsplit(y, ":"))[[1]][2], "  ")[[1]][-1]))
  tmp = lapply(tmp, function(x) gsub(" ", "", x))
  tmp = lapply(tmp, as.numeric)
  value_matrix = do.call(rbind, tmp)
  value_df = as.data.frame(value_matrix)
  rownames(value_df) = data_lat
  colnames(value_df) = data_long
  value = unlist(lapply(long_lat, function(x) value_df[strsplit(x, " ")[[1]][2], strsplit(x, " ")[[1]][1]]))
  #value = mapply(function(x, y) value_df[x, y], lat, long)
  result = cbind(result, value)
  colnames(result) = c(names(result)[1:3], name)
  return(result)
}

variable = c("cloudhigh", "cloudmid", "cloudlow", "ozone", "pressure", "surftemp", "temperature")
filelist = list.files("~/academic/Sta141/NASA/")
path0 = "~/academic/Sta141/NASA/"
#combine the strings
file_path = paste0(path0, filelist)
#define the second function
dataframe_variable<-function(variable){
  file1 = file_path[grepl(variable, file_path)]
  
  tmp = lapply(file1, function(x) get_dataframe(x, variable))
  result = do.call(rbind, tmp)
  return(result)
}
sum_dataframe = lapply(variable, dataframe_variable)



#Step 2
#Now we have 7 big dataframe stored in sum_dataframe.
sapply(sum_dataframe, nrow)
#And all of then have the same number of observations.
#Then we are going th check if their observations are correspond to each other.
#Extract the time long and lat of each dataframe.
test_df = lapply(sum_dataframe, "[", -4)
#Check if each dataframe in test_df is indentical to test_df[[1]]
sapply(test_df, function(x) identical(x,test_df[[1]]))
#The result show that their time, long and lat are exactly the same.

#Now we are going to combine the 7 dataframes and get our dataframe with all 7 variables.
#Get our time, long and lat first.
mydf1 = sum_dataframe[[1]][-4]
#Get a dataframe with all the 7 variables.
mydf2 = do.call(cbind, lapply(sum_dataframe, function(x) x[, 4]))
#Combine the first and second dataframe and set the columns' name.
mydf = cbind(mydf1, mydf2)
colnames(mydf)[4:10] = variable
#Change the class of long&lat to character, otherwise their class will be factor, this transformation is necessary, we will use it in Step 3.
mydf$long = as.character(mydf$long)
mydf$lat = as.character(mydf$lat)
#mydf is the dataframe with all variables we want.
mydf[1:3, ]

#Step3
path1 = file_path[grepl("intlvtn",file_path)]
intlvtn = readLines(path1)
intlvtn[1]
table(abs(-as.numeric(gsub("W", "", mydf$long[1:24])) - as.numeric(strsplit(intlvtn[1]," ")[[1]]))<=0.05)
#Although the format of longtitude in intlvtn is different from our dataframe, its order is still the same to our longtitude, further more the absolute difference for each position between them is no greater than 0.5, so we can replace the longtitude and latitude in intlvtn with our original long ans lat 
#Transform intlvtn to a dataframe.
#Exclude the first line longtitude
intlvtn_df = intlvtn[-1]
#Exclude the latitude which is the first string in each line
intlvtn_list = lapply(intlvtn_df, function(x) strsplit(x, " ")[[1]][-1])
#Convert characters to numerics
intlvtn_list = lapply(intlvtn_list, as.numeric)
intlvtn_df = do.call(rbind, intlvtn_list)
#Set its colnames and rownames
rownames(intlvtn_df) = data_lat
colnames(intlvtn_df) = data_long
#Add it to our dataframe obtained from Step 2

mydf$elevation = mapply(function(x, y) intlvtn_df[x, y], mydf$lat, mydf$long)
#Notice that if we construct our dataframe in this way, the elevation must correspond to the long&lat of the original dataframe, because we extract our elevation from intlvtn with the value of long&lat from mydf.


#Step 4.1
summary(mydf$cloudlow)
#Notice that there are some NAs, so we have to exclude them in the plot.
#Using the function cut to divide cloulow into 5 groups.
cut_cloudlow = cut(na.omit(mydf$cloudlow),5)
#Set each group of cloudlow a particular color.
cut_cloudlow_int = as.integer(cut_cloudlow)
color = c("blue","orange","green","red","black")
cloudlow_color = color[cut_cloudlow_int] 
#plot with mydf[!is.na(mydf$cloudlow), ] 
#To aviod overlapping, I used the function "jitter".
par(mar = c(5,5,5,5))
with(mydf[!is.na(mydf$cloudlow), ], plot(jitter(temperature),jitter(pressure), main = "temperature vs pressure", xlab = "temperature", ylab = "pressure", col = cloudlow_color, cex = 0.3))
legend("bottomright", legend = levels(cut_cloudlow), col = color, pch = 18, cex = 0.8)

#Step4.2
corner_long = c("113.8W", "56.2W")
corner_lat = c("36.2N", "21.2S")
corner_lat_long = outer(corner_long, corner_lat, paste)
#Store the long&lat of four corners in a list.
corner_lat_long = lapply(corner_lat_long, function(x) strsplit(x, " ")[[1]])
#Get the dataframe of each corner
corner_df = lapply(corner_lat_long, function(x) mydf[mydf$long == x[1] & mydf$lat == x[2], ])
#order these dataframes by time
corner_df = lapply(corner_df, function(x) x[order(x$time), ])
#Plot for each corner, title these plot by their long&lat
par( mar = c(2.5,2.5,2.5,2.5))
par(mfrow = c(2,2))
invisible(
  sapply(corner_df, function(x) plot(x$time, x$temperature, xlab = "time", ylab = "temperature", main = paste(x$long[1], x$lat[1]), type = "l"))
)




#Step 4.3
#Using the funciton lapply with long and lat to construct the mean and SD of each position, Notice that I won't use mapply because I need the result to be a list.
#This one is a little bit hard to read, we use two apply functions to get the mean of each position, the first apply is used to get the dataframe of each position, and the second apply is used to get the mean of each variable from the dataframe we obtained from the first apply function.
mean_each_position = lapply(split(mydf, list(mydf$long, mydf$lat)), 
                            function(x) sapply(variable, 
                                               function(z) mean(x[,z], na.rm = TRUE))
                            )
tmp = as.data.frame(do.call(rbind, mean_each_position))
colnames(tmp) = paste(variable, "mean")
#Now we have a problem, the order of long&lat is different from our previous dataframe, so we have to use the names(split(mydf, list(mydf$long, mydf$lat))) to construct our new long&lat.
rnames = names(split(mydf, list(mydf$long, mydf$lat)))
#Notice that the structure of our rnames like this "101.2W.1.2N"
#In order to split it, first we have to find the position of W, and then split the string into 2 parts without the middle "." 
find_position = function(x){
  which(strsplit(x, "")[[1]]=="W")
}
long_lat = lapply(rnames, function(x) c(substring(x, 1, find_position(x)), substring(x, find_position(x) + 2, nchar(x))))
result = do.call(rbind, long_lat)
colnames(result) = c("long", "lat")
result = cbind(result, tmp)
#Applying the same method to SD
SD_each_position = lapply(split(mydf, list(mydf$long, mydf$lat)), 
                          function(x) sapply(variable, 
                                             function(z) sd(x[,z], na.rm = TRUE))
                          )
tmp = as.data.frame(do.call(rbind, SD_each_position))
colnames(tmp) = paste(variable, "SD")
result = cbind(result, tmp)
result$long = as.character(result$long)
result$lat = as.character(result$lat)
#Adding column elevation to the dataframe result is just the same as adding it to mydf.
result$elevation = mapply(function(x, y) intlvtn_df[x, y], result$lat, result$long)
result[1:3, ]

#To plot on a map, we need to convert the long&lat to numeric class
result$long = -as.numeric(substring(result$long, 1, nchar(result$long)-1))
#Converting lat is more complicated, we need to assign positive value for N and negative value for S.
convert_lat = function(lat){
  N_or_S = substring(lat, nchar(lat), nchar(lat))
  if(N_or_S == "N"){
    return(as.numeric(substring(lat, 1, nchar(lat)-1)))
  }
  else{
    return(-as.numeric(substring(lat, 1, nchar(lat)-1)))
  }
}
result$lat = sapply(result$lat, convert_lat)

#Using funciton "cut" to divide pressure into 6 groups
pressure_col_cut = cut(result$`pressure mean`, c(673, 739, 804, 869, 933, 999,1000))
pressure_col = as.integer(pressure_col_cut)
result_split = split(result, pressure_col)
#Using heated colors to show the pressure on the map
par(mfrow = c(1,1))
map("world",mar = c(0,0,0,0), ylim = range(result$lat), xlim = range(result$long))
invisible(
  lapply( 1:length(result_split), 
          FUN = function(x){
            points(x = result_split[[ x ]]$long, y = result_split[[ x ]]$lat, 
                    pch = ".", cex = 33, col = heat.colors(36,alpha = 0.5)[6*x]  )
          }
  ) 
)


#Step4.5
#We have already added the column elevation to our dataframe result at Step4.3
#There is no obvious ouliers in result$elevation, so we can plot directly.
par(mfrow = c(1,1))
par(mar = c(5,5,5,5))
plot(result$`surftemp mean`, result$elevation, main = "average surftemp v.s elevation", xlab = "avg surface temperature", ylab = "elevation", cex = 0.5)

