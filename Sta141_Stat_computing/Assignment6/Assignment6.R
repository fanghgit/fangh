#**********************Assignment6*********************
library(RCurl)
library(XML)
library(utils)

options(expressions=500000)
#*******Part1*********
#Process the first page.
initial_url = "http://stackoverflow.com/questions/tagged/r?sort=newest"
page_content = getURLContent(initial_url)
class(page_content)
nchar(page_content)
cat(substring(page_content,1,2000))

page = htmlParse(page_content)
class(page)


#The pattern of posts
#? if I want to extract posts with class contains question?
posts = getNodeSet(page, "//div[@class = ('question-summary')]")
#Exactly 15 posts

p = posts[[1]]
p1 = getNodeSet(p, ".//div[@class = 'user-details']//a", xmlValue)
xpathSApply(p, ".//div[@class = 'user-details']//a", xmlValue)


get_post = function(tag, n, url, i = 1, pre_data){
  #url1 = "http://stackoverflow.com/questions/tagged/r?page=1662&sort=newest&pagesize=50"
  if(missing(url)){
    url = paste0("http://stackoverflow.com/questions/tagged/", tag, "?page=1&sort=newest&pagesize=50")
  }
  #page_content = getURLContent(url, .mapUnicode = FALSE)
  page_content = getURL(url, .mapUnicode = FALSE)
  page = htmlParse(page_content, asText = TRUE)
  posts = getNodeSet(page, "//div[@class = ('question-summary')]")
  #tags
  tags = sapply(posts, function(x) xpathSApply(x, ".//div//a[@class = 'post-tag']", xmlValue))
  tags = sapply(tags, function(x) paste(x, collapse = "; "))
  tags = unname(tags)
  #Note that we could also use date_posts = xpathSApply(page, ".//div//a[@class = 'post-tag']")
  #The reason I use sapply, is that I want to make sure the date and user are extracted from the same post.
  #This thought is also applied in the following quesiton, so I will use sapply to our "posts"
  
  #id
  id = sapply(posts, function(x) gsub(".*?([0-9]+).*","\\1", xmlGetAttr(x, "id")))
  #user
  user = sapply(posts, function(x) xpathSApply(x, ".//div[@class = 'user-details']//a", xmlValue))
  #date
  date = sapply(posts, function(x) xpathSApply(x, ".//span[@class = 'relativetime']", function(y) xmlGetAttr(y,"title")))
  #title
  title = sapply(posts, function(x) xpathSApply(x, ".//div[@class = 'summary']//a[@class = 'question-hyperlink']", xmlValue))
  #votes
  votes = sapply(posts, function(x) xpathSApply(x, ".//div[@class = 'votes']//strong", xmlValue))
  #view
  views = sapply(posts, function(x) xpathSApply(x, ".//div[@class = 'views ' or @class = 'views supernova' or @class = 'views warm' or @class = 'views hot']", function(x) strsplit(xmlGetAttr(x, "title"), " ")[[1]][1]))
  #answer
  answers = sapply(posts, function(x) xpathSApply(x, ".//div[@class = ('status answered-accepted') or @class = ('status unanswered') or @class = ('status answered')]//strong", xmlValue))
  #reputation
  reputation = sapply(posts, function(x) xpathSApply(x, ".//span[@class = 'reputation-score']", xmlValue))
  #url
  url_head = "http://stackoverflow.com"
  url = sapply(posts, function(x) xpathSApply(x, ".//a[@class = 'question-hyperlink']", function(y) xmlGetAttr(y, "href")))
  url_final = paste0(url_head, url)
  #nest page
  next_url = get_next_page(page)
  data = data.frame(cbind(id, date, tags, title, url_final, views, votes, answers, user, reputation))
  data[] = lapply(data, as.character)
  #Release the memory, which can make my computation faster especially to the pages after 1000.
  rm(list = c("page_content", "page", "posts", "tags", "id", "user", "date", "title", "votes", "views", "answers", "reputation", "url_head", "url", "url_final"))
  if(!missing(pre_data)){
    data = rbind(pre_data, data)
  }
  
  i = i + 1
  print(i)
  if(missing(n)){
    if(length(next_url) == 0){
      return(data)
    }
    else{
      get_post(tag = tag, url = next_url, i = i, pre_data = data)
    }
  }
  else{
    if(i > n | length(next_url) == 0){
      return(data)
    }
    else{
      get_post(tag, n, next_url, i, data)
    }
  }
}

get_next_page = function(page){
  url_head = "http://stackoverflow.com"
  nextpage = xpathSApply(page, "//a[@rel = 'next']", function(x) xmlGetAttr(x, "href")) 
  if(length(nextpage) == 0){
    nextpage
  }
  else{
    paste0(url_head, nextpage)
  }
}

#all_post_500pages = get_post(tag = "r", n = 500)
#result = get_post("r")
#save(all_post_500pages, file = "~/all500pages.csv")
#write.csv(all_post_500pages, file = "~/all500pages.csv")
#all500pages = read.csv(file = "~/all500pages.csv")

#write.csv(result, file = "~/Allpages.csv")
#Allpages = read.csv(file = "~/Allpages.csv")
#Allpages = Allpages[,-1]
Allpages = get_post("r")
Allpages = Allpages[!duplicated(Allpages$qid),]
saveRDS(Allpages, file = "~/Allpages")



#********Part2************
load("~/academic/rQAs.rda")


get_info_eachpost = function(url){
  page_content = getURL(url, .mapUnicode = FALSE)
  page = htmlParse(page_content, asText = TRUE)
  page_question = xpathSApply(page, "//div[@class = 'question']")
  page_answer = xpathSApply(page, "//div[@class = 'answer accepted-answer' or @class = 'answer']")
  data_question = lapply(page_question, function(x) get_data_eachtype("question", x))
  data_answer = lapply(page_answer, function(x) get_data_eachtype("answer", x))
  data_comment_Q = lapply(page_question, function(x) get_comment("comment_Q", x))
  data_comment_A = lapply(page_answer, function(x) get_comment("comment_A", x))
  data_answer_full = do.call(rbind, data_answer)
  data_comment_Q_full = do.call(rbind, data_comment_Q)
  data_comment_A_full = do.call(rbind, data_comment_A)
  data = list(data_question[[1]], data_answer_full, data_comment_Q_full, data_comment_A_full)
  return(do.call(rbind, data))
  }


get_data_eachtype = function(type_given, page){
    user = xpathSApply(page, ".//div[@class = 'user-details']//a", xmlValue)
    userid = xpathSApply(page, ".//div[@class = 'user-gravatar32']//a", function(x) gsub("/.*?/(.*?)/.*", "\\1", xmlGetAttr(x, "href")))
    reputation = xpathSApply(page, ".//span[@class = 'reputation-score']", xmlValue)
    score = xpathSApply(page, ".//div[@class = 'vote']//span[@class = 'vote-count-post ']", xmlValue)
    text = as(getNodeSet(page, ".//div[@class = 'post-text']")[[1]], "character")
    if(type_given == "question"){
      date = xpathSApply(page, ".//td[@class = 'post-signature' or @class = 'post-signature owner']//text()[contains(., 'ask')]/..//span", function(x) gsub("([^A-z]*).*", "\\1", xmlGetAttr(x, "title")))
      id = xmlGetAttr(page, "data-questionid")
      qid = id
      parent = NA
    }
    else{
      date = xpathSApply(page, ".//td[@class = 'post-signature' or @class = 'post-signature owner']//text()[contains(., 'answer')]/..//span", function(x) gsub("([^A-z]*).*", "\\1", xmlGetAttr(x, "title")))
      id = xmlGetAttr(page, "data-answerid")
      parent = NA
      top_ancestor = xmlAncestors(page)[[1]]
      qid = xpathSApply(page, "//div[@class = 'question']", function(x) xmlGetAttr(x, "data-questionid"))
    }
    type = type_given
    data = data.frame(cbind(user, userid, date, reputation, score, text, id, type, parent, qid))
    data[] = lapply(data, as.character)
    return(data)
}


get_comment = function(type_given, page_given){
  page = getNodeSet(page_given, ".//tr[@class = 'comment ']")
  n = length(page)
  user = xmlSApply(page, function(x) xpathSApply(x, ".//a[@class = 'comment-user' or @class = 'comment-user owner']", xmlValue))
  userid = xmlSApply(page, function(x) xpathSApply(x, ".//a[@class = 'comment-user' or @class = 'comment-user owner']/@href", function(x) gsub("/.*?/(.*?)/.*", "\\1", x)))
  userid = unname(userid)
  date = xmlSApply(page, function(x) xpathSApply(x, ".//span[@class = 'comment-date']//span/@title", function(x) gsub("([^A-z]*).*", "\\1", x)))
  date = unname(date)
  reputation = xmlSApply(page, function(x) xpathSApply(x, ".//a[@class = 'comment-user' or @class = 'comment-user owner']/@title", function(x) gsub(".*?([0-9]+).*", "\\1", x)))
  reputation = unname(reputation)
  score = xmlSApply(page, function(x) xpathSApply(x, ".//td[@class = ' comment-score']", function(x) gsub(".*?([0-9]*).*", "\\1", xmlValue(x))))
  text = xmlSApply(page, function(x) as(getNodeSet(x, ".//span[@class = 'comment-copy']")[[1]], "character"))
  id = xmlSApply(page, function(x) gsub(".*?([0-9]+).*", "\\1", xmlGetAttr(x, "id")))
  top_ancestor = xmlAncestors(page_given)[[1]]
  qid = xpathSApply(top_ancestor, "//div[@class = 'question']", function(x) xmlGetAttr(x, "data-questionid"))
  qid = rep(qid, n)
  
  if(type_given == "comment_Q"){
    parent = qid
  }
  else{
    parent = rep(xmlGetAttr(page_given, "data-answerid"), n)
  }
  type = rep("comment", n)
  data = data.frame(cbind(user, userid, date, reputation, score, text, id, parent, type, qid))
  data[] = lapply(data, as.character)
  return(data)
}
#ramdonly test some urls
set.seed(10)
test_url = Allpages$url_final[sample(nrow(Allpages))[1:5]]

get_info_eachpost(test_url[1])





#****************Part3******************
load("~/rQAs.rda")
dim(rQAs)
#(1)
#Use "userid" to distinguish different persons
#If a userid answered the same question for many times, we will only count it as 1 time.
par(mfrow = c(2,1), cex.main = 0.9)
rQAs_split_user = split(rQAs, rQAs$userid)
#rQAs_split_type = split(rQAs, rQAs$type)
user_answer = lapply(rQAs_split_user, function(x) x[x$type == "answer", ])
user_answer_unqiue = sapply(user_answer, function(x) length(unique(x$qid)))

table_answer = table(user_answer_unqiue)
#To make our plot looks better, we need to exclude the largest number of answer.
table_answer = table_answer[-length(table_answer)]
plot(table_answer, type = 'h', lwd = 3, main = "Distribution of number each person answered (with 0)", xlab = "number of questions", ylab = "number of answers")
length(unique(rQAs$user))



#If we want to find the distribution without those who has 0 answers
rQAs_answer = rQAs[rQAs$type == "answer", ]
rQAs_answer_split = split(rQAs_answer, rQAs_answer$userid)
user_answer_unqiue = sapply(rQAs_answer_split, function(x) length(unique(x$qid)))

table_answer = table(user_answer_unqiue)
table_answer = table_answer[-length(table_answer)]
plot(table_answer, type = 'h', lwd = 3, main = "Distribution of number each person answered (without 0)", xlab = "number of questions", ylab = "number of answers")






#(2)
#merge
rQAs_new = rQAs
rQAs_new$url_final = rownames(rQAs_new) 
name = names(Allpages)
name[which(name == "id")] = "qid"
colnames(Allpages) = name
merge_result = merge(rQAs_new, Allpages[,c("qid","tags")], by = "qid")
names(merge_result)
#Randomly check if we get the correct dataframe.
#“http://stackoverflow.com/questions/32841873/how-to-get-the-particular-files-from-a-directory-in-r”
#"http://stackoverflow.com/questions/33338763/how-to-get-all-the-posible-permutations-of-a-sequence-in-a-list-in-r"
#All correct
merge_result$tags = as.character(merge_result$tags)
#Since there are many answers and comments in one question, so we need to use qid to split 'merge_result'
merge_result_split = split(merge_result, merge_result$qid)
all_tags = sapply(merge_result_split, function(x) unique(x$tags))
all_tags = strsplit(all_tags, "; ")
all_tags = unlist(all_tags)
sort(table(all_tags), decreasing = TRUE)[1:10]

#(3)
#If we use tag to find questions about ggplot, then there are 732 quesitons about ggplot
#Now, if we use text to find quesitons about ggplot.
#Find rQAs whose type is "question"
rQAs_question = rQAs[rQAs$type == "question", ]
table(grepl("ggplot", rQAs_question$text))
#So there are 956 questions with "ggplot" in text.

#We can also define quesitons about "ggplot" as the combination of these two, its text includes "ggplot" or its tags include "ggplot".
merge_result_question = merge_result[merge_result$type == "question", ]
table(grepl("ggplot", merge_result_question$text) | grepl("ggplot", merge_result_question$tags))
#Then there are 1009 qualified quesitons.



#Use title to define questions about "ggplot".
rQAs_new_question = rQAs_new[rQAs_new$type == "question", ]
table(grepl("ggplot", rQAs_new_question$url_final, ignore.case = TRUE))
#So there are 516 questions about


#(4)
#Using tags to find them. 
table(grepl("xml|html|web-scrapping", merge_result_question$tags))
#There are 1273 questions about 


#(5)
url = rQAs_new_question$url_final

Alltitles = gsub("http://stackoverflow.com/questions/([0-9]+)/", "", url)
title_str = strsplit(Alltitles, "-| ")
title_str_ = unlist(title_str)
title_str_all = paste(title_str_all, collapse = " ")

#However it is impossible for us to find all functions in R, because there are so many packages, and we cannot install all of them.
#Use the result of question2 "most common tags" to find our target packages and install them.
#They are "base" "ggplot"
#And I am going to recognize the function
#install.packages("ggplot2")
#install.packages("shiny")
#install.packages("dplyr")
#install.packages("knitr")
#install.packages("stringr")
#install.packages("stringi")
target_packages = list("base", "ggplot2", "shiny", "dplyr", "knitr", "stringr", "stringi")
invisible(
  lapply(target_packages, require, character.only = TRUE)
)

target_packages_name = paste0("package:", target_packages)
Allfunctions = lapply(target_packages_name, ls)
Allfunctions = unlist(Allfunctions)


#functions_in_title = Allfunctions[Allfunctions %in% tolower(title_str_all)]
#Method 1
punct = regmatches(Allfunctions, gregexpr("[[:punct:]]", Allfunctions))
punct = unique(unlist(punct))
punct = punct[punct != ">" & punct != "<"]
punct_reg = paste0("\\", punct)
punct_rep = paste0("\\\\", punct)

Allfunctions_new = Allfunctions
for(i in 1:length(punct_reg)){
  Allfunctions_new = gsub(punct_reg[i], punct_rep[i], Allfunctions_new)
  print(i)
  print(head(Allfunctions_new))
}


functions_in_title = lapply(Allfunctions_new, function(x) length(regmatches(title_str_all, gregexpr(x, title_str_all))[[1]]))
names(functions_in_title) = Allfunctions_new
func_list = unlist(functions_in_title)
sort(func_list, decreasing = T)[1:10]
#This result is not good, because the most common functions we find are "t", "a", "n", "c", "p"
#We need to change the pattern of our Allfunctions.
#The define our function as " function[ \\()]"
Allfunctions_pattern = paste0(" ", Allfunctions_new, "[ \\(]")
#And do the previous procedure again
functions_in_title = lapply(Allfunctions_pattern, function(x) length(regmatches(title_str_all, gregexpr(x, title_str_all))[[1]]))
names(functions_in_title) = Allfunctions
func_list = unlist(functions_in_title)
func_table = sort(func_list, decreasing = T)[1:10]
func_table
#this time we get a better result, but far from perfect, "a" is the most common one, and of course it shouldn't, but now it is different to distinguish function "a" and the word "a".
#So my final result is this:
pie(func_table, main = "distribution of R functions in title")

#Note we can also use is.function() , get() and try() to find our functions, should get approximately the same result.






#(6)
#??Accepted answer
unique(rQAs$type)
rQAs_new_ans_com = rQAs[rQAs$type == "answer" | rQAs$type == "comment", ]
rQAs_new_ans_com$text[1]

Allcodes = regmatches(rQAs_new_ans_com$text, gregexpr("<code>.*?</code>", rQAs_new_ans_com$text))
Allcodes = lapply(Allcodes, function(x) gsub("<code>|</code>", "", x))
Allcodes = unlist(Allcodes)


unique(rQAs$type)
rQAs_new_ans_com = rQAs[rQAs$type == "answer" | rQAs$type == "comment", ]
#Find all possible "punct" in R functions.
punct = regmatches(Allfunctions, gregexpr("[[:punct:]]", Allfunctions))
punct = unique(unlist(punct))
punct
#The pattern of function is like this "XXXX(...)" X is letter, number or the "punct" I have just find.
#However we have to remove "(" and from our pattern, because it can be confused with function"(").
punct = punct[punct != "("]
pattern_punct = paste0(punct, collapse = "\\")
pattern_punct = paste0("\\", pattern_punct)

pattern = paste0("([A-z0-9", pattern_punct, "]+)\\(")
#Fina all functions that follow our pattern
func_result = regmatches(Allcodes, gregexpr(pattern, Allcodes))
#Replace "(" with nothing
func_result = unlist(func_result)
func_result = gsub("\\(", "", func_result)
table_func = head(sort(table(func_result), decreasing = T), 10)
table_func
pie(table_func)
#We get a better result















