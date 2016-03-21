#*****************************Assignment 5*****************************
#Huang Fang 913439658
library(RSQLite)
library(combinat)
library(igraph)

conn = dbConnect(SQLite(), "/Users/fangh/Downloads/lean_imdbpy.db")
db = dbConnect(SQLite(), "/Users/fangh/Downloads/imdb_data")

dbListTables(conn)
cast_info = dbGetQuery(conn, "SELECT * FROM cast_info LIMIT 100")
role_type = dbGetQuery(conn, "SELECT * FROM role_type LIMIT 100")
name = dbGetQuery(conn, "SELECT * FROM name LIMIT 100")
title = dbGetQuery(conn, "SELECT * FROM title LIMIT 100")
movie_info = dbGetQuery(conn, "SELECT * FROM movie_info LIMIT 100")
info_type = dbGetQuery(conn, "SELECT * FROM info_type LIMIT 100")
aka_name = dbGetQuery(conn, "SELECT * FROM aka_name LIMIT 100")
person_info = dbGetQuery(conn, "SELECT * FROM person_info LIMIT 100")
movie_info = dbGetQuery(conn, "SELECT * FROM movie_info LIMIT 100")
aka_title = dbGetQuery(conn, "SELECT * FROM aka_title LIMIT 100")
kind_type = dbGetQuery(conn, "SELECT * FROM kind_type LIMIT 100")
movie_keyword = dbGetQuery(conn, "SELECT * FROM movie_keyword LIMIT 100")
keyword = dbGetQuery(conn, "SELECT * FROM keyword LIMIT 100")

dbactor = dbGetQuery(db, "SELECT * FROM actors LIMIT 100")
dbmovies = dbGetQuery(db, "SELECT * FROM movies LIMIT 100")
dbactin = dbGetQuery(db, "SELECT * FROM acted_in LIMIT 100")

#char_name = dbGetQuery(conn, "SELECT * FROM char_name LIMIT 100")

# [1] "aka_name"        "aka_title"       "cast_info"       "info_type"       "keyword"        
#[6] "kind_type"       "movie_info"      "movie_info_idx"  "movie_keyword"   "name"           
#[11] "person_info"     "role_type"       "sqlite_sequence" "title"   

sql = "CREATE TABLE movie_cast_name AS 
       SELECT title.title AS title, kind_id, production_year, person_id, movie_id, role_id, nr_order, name FROM title, cast_info, name 
       WHERE title.id = cast_info.movie_id AND cast_info.person_id = name.id AND kind_id = 1 AND (role_id = 1 OR role_id = 2)"
dbSendQuery(conn, sql)
#dbSendQuery(conn, "DROP TABLE movie_cast_name")



#Q1. How many actors? How many movies?
q1sql = "SELECT COUNT(DISTINCT person_id) FROM cast_info WHERE role_id = 1 OR role_id = 2 AND person_id <> 'NA'"
num_actors = dbGetQuery(conn, q1sql)
num_actors

q1sql = "SELECT COUNT(id) FROM title"
num_movies = dbGetQuery(conn, q1sql)
num_movies

#Q2. Year     ???
q2sql = "SELECT MIN(production_year) AS MIN, MAX(production_year) AS MAX FROM title"
year_range = dbGetQuery(conn, q2sql)
year_range

q2sql = "SELECT MIN(production_year) AS MIN, MAX(production_year) AS MAX FROM aka_title"
year_range = dbGetQuery(conn, q2sql)
year_range

series_years = dbGetQuery(conn, "SELECT series_years FROM title WHERE series_years <> 'NA'")
head(series_years)
pat = "[0-9]{4}"
all_year = regmatches(series_years[,1],gregexpr(pat, series_years[,1]))
all_year = unlist(all_year)
all_year = as.numeric(all_year)
year_range = c(min(all_year), max(all_year))
names(year_range) = c("MIN", "MAX")
year_range

#Q3.
#Method1, use SQL and R.
q3sql = "SELECT gender, COUNT(DISTINCT person_id) AS proportion FROM cast_info, name 
        WHERE cast_info.person_id = name.id AND (cast_info.role_id = 1 OR cast_info.role_id = 2) 
        GROUP BY name.gender"
actor_by_gender  = dbGetQuery(conn, q3sql)
actor_by_gender$proportion = actor_by_gender$proportion/sum(actor_by_gender$proportion)
actor_by_gender

#Method2, only use SQL
q3sql = "SELECT gender, COUNT(DISTINCT person_id)/(SELECT COUNT(DISTINCT person_id)*1.0 
        FROM cast_info, name WHERE cast_info.person_id = name.id AND 
        (cast_info.role_id = 1 OR cast_info.role_id = 2)) AS proportion FROM cast_info, name 
        WHERE cast_info.person_id = name.id AND (cast_info.role_id = 1 OR cast_info.role_id = 2) 
        GROUP BY name.gender"
dbGetQuery(conn, q3sql)
#We get the same result, however, in this way, the SQL is hard to read, and it takes more time on computation.


#Q4. 
dbGetQuery(conn, "SELECT * FROM kind_type")
#From table "kind_info", we can find that 1 denotes movie
#Method1, use SQL and R
q4sql = "SELECT kind_id, kind, COUNT(DISTINCT title.id) AS proportion FROM title, kind_type 
        where title.kind_id = kind_type.id 
        GROUP BY title.kind_id"
movie_by_kind = dbGetQuery(conn, q4sql)
movie_by_kind$proportion = movie_by_kind$proportion/sum(movie_by_kind$proportion)
movie_by_kind

#Method2, only use SQL
q4sql = "SELECT kind_id, kind, (COUNT(DISTINCT title.id)/(SELECT COUNT(DISTINCT title.id)*1.0 
        FROM title, kind_type WHERE title.kind_id = kind_type.id)) AS proportion 
        FROM title, kind_type 
        WHERE title.kind_id = kind_type.id 
        GROUP BY title.kind_id"
dbGetQuery(conn, q4sql)

#Q5.
#Whenever movie_info.info_type_id = info_type.id  AND  info_type.info = 'genres', then the value of movie_info.info is a particular value of genres.
q5sql = "SELECT COUNT(movie_info.id) FROM movie_info, info_type 
        WHERE movie_info.info_type_id = info_type.id AND  info_type.info = 'genres'"
dbGetQuery(conn, q5sql)
#2110545 movie_id with info_type.info = 'genres'

q5sql = "SELECT DISTINCT movie_info.info FROM movie_info, info_type 
        WHERE movie_info.info_type_id = info_type.id AND  info_type.info = 'genres'"
value_genres = dbGetQuery(conn, q5sql)
nrow(value_genres)
#There are 32 distinct movie_info.info with info.type = 'genres'
value_genres$info

#If we only want to find the distinct movie.info from movies, then we need to do a little change.
q5sql = "SELECT DISTINCT movie_info.info FROM movie_info, info_type, title 
        WHERE movie_info.info_type_id = info_type.id AND movie_info.movie_id = title.id AND 
        info_type.info = 'genres' AND title.kind_id = 1"
movie_info = dbGetQuery(conn, q5sql)
movie_info$info
#Now there are 28 distinct movie_info



#Q6. ???movie_id or id
#Only Count those genres with title.kind_id = 1 (only choose from movies)
q6sql = "SELECT movie_info.info, COUNT(movie_info.id) AS number FROM movie_info, title, info_type 
        WHERE movie_info.info_type_id = info_type.id AND  movie_info.movie_id = title.id 
        AND info_type.info = 'genres' AND title.kind_id = 1 
        GROUP BY movie_info.info 
        ORDER BY COUNT(movie_info.id) DESC LIMIT 10"
dbGetQuery(conn, q6sql)
#This is our 10 most common genres, the number is the number of movies


#Q7. 
q7sql = "SELECT DISTINCT movie_keyword.movie_id FROM movie_keyword, title, keyword 
        WHERE movie_keyword.movie_id = title.id AND movie_keyword.keyword_id = keyword.id 
        AND keyword.keyword = 'space' AND title.kind_id = 1"
movies_key_space = dbGetQuery(conn, q7sql)
nrow(movies_key_space)
#There are 401 movies with keyword 'sapce'.

#Find their distinct production_year
q7sql = "SELECT DISTINCT production_year FROM title WHERE id IN 
        (SELECT DISTINCT movie_keyword.movie_id FROM movie_keyword, title, keyword 
        WHERE movie_keyword.movie_id = title.id AND movie_keyword.keyword_id = keyword.id 
        AND keyword.keyword = 'space' AND title.kind_id = 1) 
        ORDER BY production_year ASC"
movie_year = dbGetQuery(conn, q7sql)
movie_year$production_year


q7sql = "SELECT movie_id, person_id, name, nr_order FROM cast_info, name 
        WHERE cast_info.person_id = name.id AND movie_id IN 
        (SELECT DISTINCT movie_keyword.movie_id FROM movie_keyword, title, keyword 
        WHERE movie_keyword.movie_id = title.id AND movie_keyword.keyword_id = keyword.id 
        AND keyword.keyword = 'space' AND title.kind_id = 1) AND (role_id = 1 OR role_id = 2) 
        AND (nr_order >=1 AND nr_order <=5) 
        ORDER BY movie_id, nr_order"
top5 = dbGetQuery(conn, q7sql)
head(top5)
dim(top5)
#However this method can not guarantee that the number of actors we select for each movie is no larger than 5, for example, a movie may have multiple nr_order = 1.
#So we have to check if this problem exist.
top5_split = split(top5, top5$movie_id)
table(sapply(top5_split, length) > 5)


#However this method is not perfect, for many movies we have nr_order = 0, and for some movies we have multiple actors with nr_order = 1, so the number of actors we extract for each movie could bu more than 5. 
#After checking a lot on google, I find it as a greatest-n-per-group problem.
movie_list = "SELECT DISTINCT movie_keyword.movie_id FROM movie_keyword, title, keyword 
              WHERE movie_keyword.movie_id = title.id AND movie_keyword.keyword_id = keyword.id 
              AND keyword.keyword = 'space' AND title.kind_id = 1"
top5_R = lapply(movie_list, function(x) 
            dbGetQuery(conn, paste("SELECT DISTINCT movie_id, person_id, name, nr_order 
            FROM cast_info, name 
            WHERE cast_info.person_id = name.id AND movie_id = ", x, 
            " AND (role_id = 1 OR role_id = 2) 
            ORDER BY nr_order DESC LIMIT 5")))
result = do.call(rbind, top5_R)


q7sql = "SELECT DISTINCT movie_id, person_id, name, nr_order FROM cast_info, name 
  WHERE cast_info.person_id = name.id AND movie_id IN (SELECT DISTINCT movie_keyword.movie_id FROM movie_keyword, title, keyword WHERE movie_keyword.movie_id = title.id AND movie_keyword.keyword_id = keyword.id AND keyword.keyword = 'space' AND title.kind_id = 1) AND nr_order <> 'NA'
  GROUP BY movie_id"
a = dbGetQuery(conn, q7sql)

#Q8
q8sql = "SELECT COUNT(DISTINCT movie_id) AS number, production_year AS year 
        FROM title, movie_info, info_type 
        WHERE movie_info.info_type_id = info_type.id AND title.id = movie_info.movie_id 
        AND movie_info.movie_id = title.id AND kind_id = 1 AND info_type.info = 'genres' 
        GROUP BY production_year 
        ORDER BY production_year"
year_num = dbGetQuery(conn, q8sql)
plot(year_num$year, year_num$number, main = "Number of movies", xlab = "Year", 
     ylab = "Number", type = "l", cex = 1)

#Group multiple columns.
q8sql = "SELECT movie_info.info, production_year, COUNT(DISTINCT movie_id) AS number 
        FROM title, movie_info, info_type 
        WHERE movie_info.info_type_id = info_type.id AND title.id = movie_info.movie_id 
        AND movie_info.movie_id = title.id AND info_type.info = 'genres' 
        GROUP BY movie_info.info, production_year"
group_year_info = dbGetQuery(conn, q8sql)
year_info_split = split(group_year_info, group_year_info$info)
par(mfrow = c(4,4), mar = c(1.9,1.9,1.9,1.9), cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
invisible(
  sapply(names(year_info_split), function(x)
        plot(year_info_split[[x]][["production_year"]], year_info_split[[x]][["number"]], 
        main = x, xlab = "year", ylab = "number", cex = 0.5))
)


#Q9.
q9sql = "SELECT person_id, name, COUNT(DISTINCT movie_id) AS number FROM movie_cast_name 
        GROUP BY person_id 
        ORDER BY COUNT(DISTINCT movie_id) DESC LIMIT 20"
dbGetQuery(conn, q9sql)



#R
#It will cost us a lot time on computation if we use the full table, so we will only extract the first 100,000 rows.
cast_info = dbReadTable(conn, "cast_info")
title = dbReadTable(conn, "title")
name = dbReadTable(conn, "name")
#Construct new columns "name", "year", "kind_id", "title" for cast_info
#Because the index of title,name is exactly the same as title$id, name$id.
cast_info$kind_id = title$kind_id[cast_info$movie_id]
cast_info$year = title$production_year[cast_info$movie_id]
cast_info$title = title$title[cast_info$movie_id]
cast_info$name = name$name[cast_info$person_id]
#Only include movies and actors
cast_info = cast_info[cast_info$kind_id == 1 & (cast_info$role_id == 1 | cast_info$role_id == 2),]
dim(cast_info)

person_movie = cast_info[, c("person_id","movie_id")]
person_movie = person_movie[!duplicated(person_movie), ]
dim(person_movie)
sort(table(person_movie$person_id), decreasing = TRUE)[1:20]




#Q10.
#Define the actors with top billing as those actors with nr_order =1, 2 or 3.
#Select top10.

q10sql = "SELECT person_id, name, COUNT(DISTINCT movie_id) AS number, 
          MAX(production_year) AS max_year, MIN(production_year) AS min_year 
          FROM movie_cast_name 
          WHERE nr_order >= 1 AND nr_order <= 3
          GROUP BY person_id
          ORDER BY COUNT(DISTINCT movie_id) DESC LIMIT 10"
dbGetQuery(conn, q10sql)


#To make our computation faster, we will use the previous top10 person_id. 
#q10sql = paste("SELECT person_id, name, MAX(production_year) as max_year, 
#          MIN(production_year) AS min_year FROM movie_cast_name 
#          WHERE nr_order >= 1 AND nr_order <= 3 AND person_id IN (", top10_person_id, ")
#          GROUP BY person_id")
#dbGetQuery(conn, q10sql)

#R
R_top10_billing = cast_info[!is.na(cast_info$nr_order) & cast_info$nr_order>=1 & cast_info$nr_order<=3,] 
#Because the id in title is exactly the same to its index, wo we can match R_top10_billing and title easily.
R_top10_billing$year = title$production_year[R_top10_billing$movie_id]
R_top10_billing = R_top10_billing[!duplicated(R_top10_billing[,c("person_id","movie_id")]),]
dim(R_top10_billing)
sort(table(R_top10_billing$person_id), decreasing = TRUE)[1:10]

top10 = sort(table(R_top10_billing$person_id), decreasing = TRUE)[1:10]
top10_id = names(top10)

top10_year = lapply(top10_id, function(x)
                    c(min(R_top10_billing[R_top10_billing$person_id == as.numeric(x),"year"]), 
                      max(R_top10_billing[R_top10_billing$person_id == as.numeric(x),"year"]))
      )
names(top10_year) = top10_id
top10_year




#Q11
q11sql = "SELECT person_id, name, production_year AS year, COUNT(DISTINCT movie_id) AS number 
          FROM movie_cast_name 
          GROUP BY person_id, production_year 
          ORDER BY COUNT(DISTINCT movie_id) DESC LIMIT 10"
top10 = dbGetQuery(conn, q11sql)
top10


#To get all the movies for each person and each year, we need to construct a temporary table, this will make my computation much more convinient.
dbSendQuery(conn, "CREATE TEMPORARY TABLE top10 AS 
                   SELECT person_id, name, production_year AS year, 
                   COUNT(DISTINCT movie_id) AS number 
                   FROM movie_cast_name 
                   GROUP BY person_id, production_year 
                   ORDER BY COUNT(DISTINCT movie_id) DESC LIMIT 10")


q11sql = "SELECT DISTINCT movie_cast_name.person_id, movie_cast_name.name, 
          production_year AS year, title FROM movie_cast_name, top10 
          WHERE movie_cast_name.person_id = top10.person_id 
          AND movie_cast_name.production_year = top10.year"
all_movies = dbGetQuery(conn, q11sql)
head(all_movies)
dim(all_movies)
sum(top10$number)

#Using R
cast_info_distinct = cast_info[!duplicated(cast_info[, c("person_id", "movie_id")]), ]
cast_info_distinct = cast_info_distinct[!is.na(cast_info_distinct$year),]
cast_info_distinct$id_year = paste("person_id:", as.character(cast_info_distinct$person_id), "name:", cast_info_distinct$name, "year:",as.character(cast_info_distinct$year))
sort(table(cast_info_distinct$id_year), decreasing = TRUE)[1:10]
#Their names are included in the previous table

top10 = sort(table(cast_info_distinct$id_year), decreasing = TRUE)[1:10]
top10_id_str = strsplit(names(top10)," ")
#Get their id
top10_id = sapply(top10_id_str, function(x) as.numeric(x[2]))
#Get their year
top10_year = sapply(top10_id_str, function(x) as.numeric(x[length(x)]))
result = cast_info_distinct[cast_info_distinct$id_year %in% names(top10), c("person_id", "name", "year", "title")]
result = result[order(result$person_id, result$year), ]
head(result)


#Q12
#Select the number of aliases who are movie actors. 
q12sql = "SELECT person_id, name, 
          COUNT(DISTINCT name) AS number_aliases 
          FROM aka_name 
          WHERE person_id IN 
          (SELECT DISTINCT person_id FROM movie_cast_name)
          GROUP BY person_id 
          ORDER BY COUNT(DISTINCT name) DESC LIMIT 10"
dbGetQuery(conn, q12sql)

#R
#
aka_name = dbReadTable(conn, "aka_name")
cast_info_full = dbReadTable(conn, "cast_info")
actor_id = unique(cast_info_full$person_id[cast_info_full$role_id %in% c(1,2)])

aka_name_actors = aka_name[aka_name$person_id %in% actor_id, ]
aka_name_actors$actual_name = name$name[aka_name_actors$person_id]
aka_name_actors = aka_name_actors[!duplicated(aka_name_actors[, c("person_id","name")]),]
dim(aka_name_actors)
sort(table(aka_name_actors$person_id), decreasing = TRUE)[1:10]





#Q13
#Find our lead actor first. 
q13sql = "SELECT person_id, name, COUNT(DISTINCT movie_id) FROM movie_cast_name 
          GROUP BY person_id 
          HAVING COUNT(DISTINCT movie_id) > 20 
          ORDER BY COUNT(DISTINCT movie_id) ASC LIMIT 10"
dbGetQuery(conn, q13sql)

#We choose the actor with person_id '5973' as our lead actor.
lead_id = 5973
lead_name = "Abraham-Kremer, Bruno"
#Find the actor who has been in the same movie with '5973', to limit the number of these actors, we will only choose those with nr_order < 3.
q13sql = "SELECT person_id, name, nr_order FROM movie_cast_name 
          WHERE movie_id IN 
          (SELECT DISTINCT movie_id FROM movie_cast_name 
          WHERE person_id = 5973) AND (person_id = 5973 OR nr_order < 3)"
net_person_id_list = dbGetQuery(conn, q13sql)
#Record the person_id we select
person_id_list = net_person_id_list$person_id
person_id_list = paste(person_id_list, collapse = ", ")
length(unique(net_person_id_list$person_id))
#We have 35 actors now
5973 %in% net_person_id_list$person_id

#Find the actors that has been in the same movie with those actors in our 'net_person_id_list', to limit the number of total actors in our network, we will only choose those with nr_order < 2
q13sql = paste("SELECT person_id, name, movie_id, nr_order FROM movie_cast_name 
          WHERE movie_id IN
          (SELECT DISTINCT movie_id FROM movie_cast_name
          WHERE person_id IN (", person_id_list, ")) 
          AND (person_id IN (", person_id_list, ") OR nr_order < 2)")

net_person_id_list_full = dbGetQuery(conn, q13sql)
length(unique(net_person_id_list_full$person_id))
actor_id = paste(net_person_id_list_full$person_id, collapse = ", ")

q13sql = paste("SELECT DISTINCT person_id, name, movie_id, nr_order FROM movie_cast_name 
              WHERE person_id IN (",actor_id, ")")
net_person_id_list_full = dbGetQuery(conn, q13sql)



#Now we finally get 674 actors in our network
n = length(unique(net_person_id_list_full$person_id))
net = matrix(0, n, n)
length(unique(net_person_id_list_full$name))
#the length of unique name is exactly the same as the length of id, so we can use name to set the matrix's colnames and rownames.
full_name_list = unique(net_person_id_list_full$name)
colnames(net) = full_name_list
rownames(net) = full_name_list
#Combination
comb_name = combn(full_name_list, 2)
comb_name = as.data.frame(t(comb_name))
colnames(comb_name) = c("actor1","actor2")
comb_name$actor1 = as.character(comb_name$actor1) 
comb_name$actor2 = as.character(comb_name$actor2) 
dim(comb_name)  #equals to choose(674,2)
head(comb_name)
#Construct a new column, if the actors in the 1st and 2nd have been in a same movie, then the value of the new column is TRUE, otherwise FALSE.
net_person_name_split = split(net_person_id_list_full, net_person_id_list_full$name)
#Design a function to do so
if_related = function(actor1, actor2, net_person_name_split){
  #Extract corresponding movie_id
  movie_actor1 = net_person_name_split[[actor1]][["movie_id"]]  
  movie_actor2 = net_person_name_split[[actor2]][["movie_id"]]
  if(length(intersect(movie_actor1, movie_actor2)) == 0){   #their intersection.
    return(FALSE)
  }
  else{
    return(TRUE)
  }
}

#"Amstutz, Roland" , "Clévenot, Philippe"


comb_name$relate = mapply(function(x, y) if_related(x, y, net_person_name_split), comb_name$actor1, comb_name$actor2)
head(comb_name)
#Update the network matrix
#Only get the related pairs
comb_name_relate = comb_name[comb_name$relate,]

#update_net = function(x, y, net){
#  result = net
#  result[x ,y] = 1
#  result[y, x] = 1
#  assign('net', result, envir = .GlobalEnv)
#}
#invisible(
#  mapply(function(x, y) update_net(x, y, net), comb_name_relate$actor1, comb_name_relate$actor2)
#)

#???
for(i in 1:nrow(comb_name_relate)){
  net[comb_name_relate$actor1[i],comb_name_relate$actor2[i]] = 1
  net[comb_name_relate$actor2[i],comb_name_relate$actor1[i]] = 1
}

#network graph, mainly from Google.
par(mfrow = c(1,1), mar = c(3,3,3,3), cex.main = 1.2)
g = graph.adjacency(net, mode = "undirected")
g = simplify(g)
V(g)$label = V(g)$name
V(g)$degree = degree(g)
#set.seed(10)
#layout1 = layout.fruchterman.reingold(g)

V(g)$label.cex = 1.2*V(g)$degree/max(V(g)$degree)+ 0.2
#Assign different colors to the lead actor and actors related to him/her.
color_vector = rep(rgb(0, 0.2, 0.2, 0.8), nrow(net))
lead_index = which(names(V(g)) == lead_name)
round2_name = unique(net_person_id_list$name)
round2_index = which(names(V(g)) %in% round2_name & names(V(g)) != lead_name)

color_vector[round2_index] = rgb(0.5, 0.5, 0, 0.8)
color_vector[lead_index] = rgb(0.7, 0.3, 0, 0.8)
V(g)$label.color = color_vector
V(g)$frame.color = NA
E(g)$color = rgb(0.5, 0.5, 0, 0.5)
E(g)$width = 0.3
vertex_size = 8*V(g)$degree/max(V(g)$degree) + 0.1
# plot the graph in layout1
#plot(g, layout=layout1, vertex.size = vertex_size, main = "Network Analysis")
plot(g, layout=layout.kamada.kawai, vertex.size = vertex_size, vertex.color = color_vector, main = "Network Analysis")
legend("topright", legend = c("lead actor", "2nd round actors", "3rd round actors"), col = c(rgb(0.7, 0.3, 0, 0.8), rgb(0.5, 0.5, 0, 0.8), rgb(0, 0, 0.2, 0.8)), 
       pch = 16, cex = 0.8)



#Q14.
#First we have the define the concept of "movie stars".
#We Define "movie stars" as those actors who has played a movie with nr_order < 5.
dbGetQuery(conn, "SELECT * FROM kind_type")

#kind_id = 2 equals to tv series.
#First get the person_id of all "movie stars", and then select tv series which has most of these "movie stars"
q14sql = "SELECT movie_id, title.title, COUNT(DISTINCT person_id) AS number
          FROM title, cast_info  
          WHERE title.id = cast_info.movie_id 
          AND kind_id = 2 AND person_id IN
          (SELECT DISTINCT person_id FROM movie_cast_name 
          WHERE nr_order < 5) 
          GROUP BY movie_id 
          ORDER BY COUNT(DISTINCT person_id) DESC LIMIT 10"
dbGetQuery(conn, q14sql)










