#***********Assignment4************
#Huang Fang
library(stringr)
options(width = 70)
con = url("http://eeyore.ucdavis.edu/stat141/Data/vehicles.rda")
load(con)

rownames(vposts) = as.character(c(1:nrow(vposts)))


price_reg = "\\$([0-9][0-9,]*)"



convert_to_na = function(x){
  if(is.character(x) & length(x) == 0){
    return(NA)
  }
  else{
    return(x)
  }
}



#Method 1
price_position = gregexpr(price_reg2, vposts$body)
result = regmatches(vposts$body, price_position)
#convert character(0) to NA
result = lapply(result, convert_to_na)
price_result = lapply(result, function(x) 
                            gsub("\\$|,", "", x)
                  )

#Set names to the list, this will make convenience in our further analysis. 
names(price_result) = as.character(c(1:length(p_result)))

#Checking those with multiple result
len = sapply(price_result, length)
multi_price = price_result[len > 1]
#randomly choose some and check their "body".
set.seed(111)
multi_price_sample = sample(names(multi_price))[1:5]
cat(vposts$body[as.numeric(multi_price_sample)][1])
#We find that the maximum price is most likely to be the actual price.
price_result_num = lapply(price_result, as.numeric) 
price_result_num = sapply(price_result_num, max)

#NAs prevent us from mathcing p_result and vposts$price, since there is no 0 is vposts$price, so we can replace NA with 0.

table(!is.na(price_result_num) & !is.na(vposts$price) & price_result_num == vposts$price)
#We got 12061 matches and 22616 mismatches.


#(2)
#Note that the standardized format of VIN contains 17 characters and does not include "I", "O" and "Q".
#Our pattern: 17 numbers or letters after "VIN", with any thing between "VIN" and our 17 characters.
reg_vin = "([Vv][Ii][Nn])([^0-9A-Z])*([0-9A-Z]{17})"
vin_list = str_match_all(vposts$body, reg_vin)
vin = sapply(vin_list, function(x) x[4])
names(vin) = as.character(c(1:length(vposts$body)))
#check one

#check if all the vins' length is 17
vin_na_omit = na.omit(vin)
table(sapply(vin_na_omit, nchar) == 17)
#There are abnormal value, check the problem.
abnormal_vin = vin_na_omit[sapply(vin_na_omit, nchar) != 17]
abnormal_index = names(abnormal_vin)
vin_list[abnormal_index][1:3]
#We can find that this problem is caused by multiple "VIN" in the "body".
#For those with identical multiple "VIN", we just extract one of them, otherwise we extract all the distinctive "VIN".
#Design a function to do this.
get_vin = function(x){
  distinct_vin = unique(x[,4])
  result = paste(distinct_vin, collapse = " ")
  return(result)
}
vin_new = sapply(vin_list, get_vin)
table(sapply(na.omit(vin_new), nchar) < 17 & sapply(na.omit(vin_new), nchar) >=1)
#All correct now, add it to our "vposts"
vposts$vin = vin_new
table(vin_new != "")
#There are 6778 posts with VIN



#(c)
#We specified the pattern of a phone number like this
reg_phone = "\\(?[0-9]{3}[() -.]*[0-9]{3}[() -.]*[0-9]{4}"
phone_list = str_extract_all(vsposts$body, reg_phone)
#Same to the previous, there may be multiple phone numbers, if there are more than one distinctive phone numbers, we extract all of them
#and convert all of them into the standardized format (XXX) XXX-XXX, X denote numbers.
standard_phone = function(x){   #x is a vector
  if(is.character(x) & length(x) == 0){
    return(x)
  }
  else{
    phone_digits = str_extract_all(x, "[0-9]")  #Get 9 digits (the order is the same as the phone)
    result = sapply(1:length(x), function(i) 
      paste0(c("(", phone_digits[[i]][1:3], ") ", phone_digits[[i]][4:6], "-", phone_digits[[i]][7:(length(phone_digits[[i]]))]), collapse = "")
    )
    return(result) #return a vector of standardized phone number
  }
}

phone_list_standard = lapply(phone_list, standard_phone)
phone_list_standard_unique = sapply(phone_list_standard, function(x)
                                                            paste(unique(x), collapse = "  ")
                                    )
#check if our result is acceptable.
table(sapply(phone_list_standard_unique, nchar) < 3)
#There are 16549 posts with phone number.
#Check the posts we didn't find phone number.
no_phone = vposts$body[sapply(phone_list_standard_unique, nchar) < 3]
set.seed(123)
cat(sample(no_phone)[1:3])
#Suprisingly, we find a post with "503.575.six five three seven", this is the rare condition, so we just ingore this strange pattern.


#(4)

reg_email = ".*([A-z0-9]+@[A-z0-9]+\\.(com|net|org|edu|gov){1}).*"
a = gsub(reg_email, "\\1", vposts$body, ignore.case = TRUE)
a[a != vposts$body]




#(5)
#First find "year" in "description", if we can't find it, then do the searching in "body".
#Because the "year" is easy to be confused with "phone number" 

reg_year = ".*((20|19)[0-9]{2}).*" 
".*([0-9]{2}).*"
des_year = gsub(reg_year, "\\1", vposts$description)
#Check
table(is.na(des_year))
table(na.omit(des_year == vposts$year))
des_year[(des_year == vposts$description)] = NA
des_year = as.numeric(des_year)
vposts$des_year = des_year




#(6)

get_pattern = function(i){
  maker = vposts$maker[i]
  reg = paste0(".*?(", maker, ")[ -]*([^ ]+) ?.*")
  return(reg)
}
#convert all titles into lowercase.
vposts$header_new = tolower(vposts$header)

#iterate by developing a regular expression.
model = sapply(1:nrow(vposts), function(i) gsub(get_pattern(i), "\\2", vposts$header_new[i], ignore.case = TRUE))
#Set NA if we didn't find its model.
model[model == vposts$header_new] = NA
table(model != vposts$header_new)
#Check our "model" to see if we extract it correctly
vposts_with_model = vposts[!is.na(model), ]
#Aftering checking our result carefully, we find that we have a particular problem with "mercedes"
head(vposts_with_model[vposts_with_model$maker == "mercedes", c("header", "maker")])
#We should convert "mercedes-benz" in headers to "mercedes", and run our previous process again.
vposts$header_new = gsub("mercedes-benz", "mercedes", vposts$header_new)
#But before we run our algorithm again, we should check if there are some other problem.

#Check the posts which we can't find its model
vposts_with_nomodel = vposts[is.na(model), ]
vposts_with_nomodel[1:10,c("header","maker")]
#We find that there is particular problem with "chevrolet", in many posts' header, it is written as "chevy".
#e should convert "chevy" in headers to "chevrolet"
vposts$header_new = gsub("chevy", "chevrolet", vposts$header_new)

#Run our previous process again.
model = sapply(1:nrow(vposts), function(i) gsub(get_pattern(i), "\\2", vposts$header_new[i], ignore.case = TRUE))
#Set NA if we didn't find its model.
model[model == vposts$header_new] = NA
table(model != vposts$header_new)

#Now we can think that we have extract "model"s as much as possible, and there some misspell or simply we extract a wrong word.
#We are going to correct the misspellings and delete our "wrong word".

#It is a common sense that different makers should have different models, so we will split the "vposts" with "maker" and do our further analysis group by group.
vposts$model = model
vposts_split = split(vposts, vposts$maker)

#Take "acura" as a example
unique(vposts_split[["honda"]]$model)
table(vposts_split[["honda"]]$model)
#We find that there are some punctution in our model, it makes our analysis difficult, replace them with "".
vposts$model = gsub("[[:punct:]]", "", vposts$model)

vposts_split = split(vposts, vposts$maker)
unique(vposts_split[["honda"]]$model)
table(vposts_split[["honda"]]$model)

#We can get useful information from the frequency table.
#We notice that some models are really infrequency, they might be typo, abbreviation, or they are just actually correct, but this rarely happen
#After we find these infrequency "models", we use "grepl" and "agrel" function to find if they are similiar to some frequently occured "models".
#If we can't find a similar one, we think the model we extract is incorrect, and set it as NA, if we find a similar one, we change the name of this infrequent "model" into the similar frequenctly occured "model". 
#First, set our accept frequency as 1/(10*n), "n" denote the number of models in this maker.
#if agrepl(infreq_model, freq_model, nchar(infreq_model)/4)


fix_abnormal = function(vposts_maker){
  table_freq = table(vposts_maker$model)/sum(table(vposts_maker$model))
  total_model = names(table_freq)
  freq_model = names(table_freq[table_freq > 1/(10*length(table_freq))])
  infreq_model = names(table_freq[table_freq <= 1/(10*length(table_freq))])
  infreq_model_new = sapply(infreq_model, function(x)
                                              if_abnormal(x, freq_model)
                            )
  #change the value of infreq_model in vposts$split
  model_new = sapply(vposts_maker$model, function(x)
                                            replace_infreq(x, infreq_model_new)
                     )
  vposts_maker$model = model_new
  return(vposts_maker)
}


if_abnormal = function(infreq_model, freq_model){
  dist_infreq = adist(infreq_model, freq_model)
  if(any(dist_infreq < nchar(infreq_model)/4 )){
    min_index = which.min(dist_infreq)
    return(freq_model[min_index])
  }
  #else if(any(grepl(infreq_model, freq_model))){
  #  return(freq_model[grepl(infreq_model, freq_model))][1])
  #}
  #else if(any(sapply(freq_model, function(x) grepl(x, infreq_model) & nchar(infreq_model)!=0 & nchar(freq_model)!=0)))
  else{
    return(NA)
  }
}

replace_infreq = function(x, infreq_model_new){
  if(is.na(x)){
    
  }
  if(x %in% names(infreq_model_new)){
    return(infreq_model_new[x])
  }
  else{
  return(x)
  }
}

#Get result after correcting our "models"
vposts_split_after_fix = lapply(vposts_split, fix_abnormal) 
vposts_new = do.call(rbind, vposts_split_after_fix)
#“vposts_new” is our final result with models in it.

length(unique(vposts_new$model))
#We totally have 1062 models.

#We choose Toyota corolla and Chevrolet silverado.
#Do some data cleaning which I have done in Assignment 1.
vposts_new$price[vposts_new$price > quantile(vposts_new$price, 0.99, na.rm = TRUE) | vposts_new$price < quantile(vposts_new$price, 0.03, na.rm = TRUE)] = NA
vposts_new$age = 2015 - vposts_new$year
vposts_new$age[vposts_new$age < 0] = NA
vposts_new$odometer[vposts_new$odometer > quantile(vposts_new$odometer, 0.99, na.rm = TRUE)] = NA
common_condition = c("excellent", "fair", "good", "like new", "new", "used")
vposts_new$condition[!vposts_new$condition %in% common_condition] = NA
vposts_new$condition = factor(vposts_new$condition)
  

#Get our dataset for Toyota corolla and Chevrolet silverado.
vposts_new_split = split(vposts_new, vposts_new$maker)
vposts_toyota = vposts_new_split[["toyota"]]
vposts_toyota_corolla = vposts_toyota[!is.na(vposts_toyota$model) & vposts_toyota$model == "corolla", ]

vposts_chevy = vposts_new_split[["chevrolet"]]
vposts_chevy_silver = vposts_chevy[!is.na(vposts_chevy$model) & vposts_chevy$model == "silverado", ]

par(mfrow = c(2,1), cex.lab=0.5, cex.axis=0.8)
boxplot(vposts_toyota_corolla$price ~ vposts_toyota_corolla$city, main = "toyota corolla", cex = 0.8)
boxplot(vposts_chevy_silver$price ~ vposts_chevy_silver$city, main = "chevy silverado", cex = 0.8)
#The price of vehicles in the city of lasvegas is higher than other cities while the price in nyc is lower than others.
#For Toyota corolla.
data_toyota = vposts_toyota_corolla[,c("price", "odometer", "age", "condition")]
sapply(data_toyota, class)
pairs(~price+odometer+age+condition, data = data_toyota)
boxplot(vposts_toyota_corolla$price ~ vposts_toyota_corolla$condition, main = "toyota corolla", cex = 0.8)
#odometer shows linear realtionship with price, age shows curvilinear and different levels of conditions seems to have different price.
#So our model is $price = \{beta}_0 + \{beta}_1$
fit1 = lm(price ~ odometer + age + condition + I(age^2), data = data_toyota, na.action = na.omit)
summary(fit1)
par(mfrow = c(1,1))
plot(fit1,which=1, cex = 0.5)
plot(fit1,which=2, cex = 0.5)
solve(cor(d[,1:3]))

#Do the same to the data of Chevrolet silverado.
data_chevy = vposts_chevy_silver[,c("price", "odometer", "age", "condition")]
sapply(data_chevy, class)
pairs(~price+odometer+age+condition, data = data_chevy)
boxplot(data_chevy$price ~ data_chevy$condition, main = "toyota corolla", cex = 0.8)
#odometer shows linear realtionship with price, age shows curvilinear and different levels of conditions seems to have different price.
#So our model is $price = \{beta}_0 + \{beta}_1$
fit2 = lm(price ~ odometer + age + condition + I(age^2), data = data_chevy, na.action = na.omit)
summary(fit2)
par(mfrow = c(1,1))
plot(fit2,which=1, cex = 0.5)
plot(fit2,which=2, cex = 0.5)
solve(cor(d[,1:3]))




d = data_toyota[!is.na(data_toyota$price) & !is.na(data_toyota$odometer) & !is.na(data_toyota$condition),]
#
aa = vposts[!is.na(vposts$model) & vposts$model == "",]





