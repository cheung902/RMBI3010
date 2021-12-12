library(robotstxt)
library(xml2)
library(rvest)
library(purrr)
library(parallel)
library(tidyr)
library(dplyr)
library(stringr)
library(anytime)
library(splitstackshape)
library(stringi)
library(janitor)

# Approach
#####################################################
# I will start scrap the data from each page into a #
# dataframe, and then clean it and join together as #
# a final dataframe. However, the cleaning and the  #
# feature engineering might also exist in other part#
# through out the development.                      #
#                                                   #
#####################################################


# this is the function for merging the two database
fastmerge <- function(d1, d2) {
  d1.names <- names(d1)
  d2.names <- names(d2)
  
  # columns in d1 but not in d2
  d2.add <- setdiff(d1.names, d2.names)
  
  # columns in d2 but not in d1
  d1.add <- setdiff(d2.names, d1.names)
  
  # add blank columns to d2
  if(length(d2.add) > 0) {
    for(i in 1:length(d2.add)) {
      d2[d2.add[i]] <- "NA"
    }
  }
  
  # add blank columns to d1
  if(length(d1.add) > 0) {
    for(i in 1:length(d1.add)) {
      d1[d1.add[i]] <- "NA"
    }
  }
  
  return(rbind(d1, d2))
}

#checking robotstxt
paths_allowed("https://www.imdb.com/search/title/?groups=top_1000&sort=user_rating")
rtxt = robotstxt(domain = "imdb.com")



#---------------------------------------------------Start Scraping-------------------------------------------------
main = "https://www.imdb.com/search/title/?groups=top_1000&sort=user_rating"
main_page = read_html(main)

#url base for the summary page
url_base = "https://www.imdb.com/search/title/"
#url base for details in each movie
url_base_detail = "https://www.imdb.com"

# Each page contain 50 movies and the web address for each page changes with the number 51, 101, 151, 201 ........
page_no = seq(1,951,50)

# Here, I start to scrap the href of each movie in order to get into each movie's summary page to scrap more data.
#page 1 href of each movie
movie_href_1 = xml_attr(xml_find_all(main_page, "//*[@id='main']/div/div[3]/div/*/div[3]/h3/a"), "href")

#page 2 - page 20 href of each movie
movie_href_all = lapply(paste0(url_base,"?groups=top_1000&sort=user_rating,desc&start=",page_no , "&ref_=adv_nxt"),
              function(url){
                xml_attr(xml_find_all(read_html(url), "//*[@id='main']/ div/div[3]/div/*/div[3]/h3/a"), "href")
              })

#All movie href
movie_href_all= unlist(movie_href_all)

#movie_href_all
start <- proc.time()

# I first scrap the basic information in the first page which is the stars and genre of each movie
#movie stars
print("scraping star")
movie_star= lapply(paste0(url_base,"?groups=top_1000&sort=user_rating,desc&start=",page_no , "&ref_=adv_nxt"),
                     function(url){
                       list(
                         Stars = html_text(html_nodes(read_html(url),
                                                      xpath = "/html/body/div[3]/div/div[2]/div[3]/div[1]/div/div[3]/div/div/div[3]/p[3]"))
                         #,Genre = html_text(html_nodes(read_html(url), xpath = "/html/body/div[3]/div/div[2]/div[3]/div[1]/div/div[3]/div/div/div[3]/p[1]"))
                       )
                     })

col_name_stars = c("Stars")
df_movie_stars = data.frame(matrix(ncol = length(col_name_stars), nrow = 0))
colnames(df_movie_stars) = col_name_stars

for (i in movie_star){
  col_name_stars = c("Stars")
  value_stars = str_squish(gsub("[\r\n]", "", sub(".*:", "", matrix(unlist(i[1]),ncol=1,byrow=TRUE))))
  df_stars = data.frame(value_stars)
  colnames(df_stars) = col_name_stars
  #df_stars = spread(df_stars, col_name_stars, value_stars)
  df_movie_stars = fastmerge(df_movie_stars,df_stars)
}



#movie genre
print("scraping genre")
movie_genre = lapply(paste0(url_base,"?groups=top_1000&sort=user_rating,desc&start=",page_no , "&ref_=adv_nxt"),
                     function(url){
                       list(
                         Genre = html_text(html_nodes(read_html(url),
                                                      xpath = "/html/body/div[3]/div/div[2]/div[3]/div[1]/div/div[3]/div/div/div[3]/p[1]"))
                       )           
                     })
col_name_genre = c("Genre")
df_movie_genre = data.frame(matrix(ncol = length(col_name_genre), nrow = 0))
colnames(df_movie_genre) = col_name_genre

for (i in movie_genre){
  col_name_genre = c("Genre")
  value_genre = str_squish(matrix(unlist(i[1]),ncol=1,byrow=TRUE))
  df_genre = data.frame(value_genre)
  colnames(df_genre) = col_name_genre
  df_movie_genre = fastmerge(df_movie_genre,df_genre)
}

df_movie_genre$Genre = sub(".*min [|] ", "", df_genre$Genre) 



time_elapsed_series <- proc.time() - start

# After scraping the star and genre, I start scraping data on the movie's summary page
# There are also some sub-page after the summary page which is the - Full Cast Page, Rating Page, Award Page and Location Page.

# Movie summary page
print("scraping summary")
movie_summary = lapply(paste0(url_base_detail,movie_href_all),
                     function(url){
                       list(
                       detail = html_text(html_nodes(read_html(url), xpath = "//*[@id='titleDetails']/div")),
                       name = html_text(html_nodes(read_html(url), xpath = "//*[@id='title-overview-widget']/div[1]/div[2]/div/div[2]/div[2]/h1/text()"))[1]
                       )
                     })

col_name = str_squish(gsub("[\r\n]", "", sub("\\:.*", "", matrix(unlist(movie_summary[[1]][1]),ncol=1,byrow=TRUE))))
col_name = append(col_name, c("Name"))
df_movie_summary = data.frame(matrix(ncol = length(col_name), nrow = 0))
colnames(df_movie_summary) = col_name

for (i in movie_summary){
  col_name = str_squish(gsub("[\r\n]", "", sub("\\:.*", "", matrix(unlist(i[1]),ncol=1,byrow=TRUE))))
  col_name = append(col_name, c("Name"))
  value = str_squish(gsub("[\r\n]", "", sub(".*:", "", matrix(unlist(i[1]),ncol=1,byrow=TRUE))))
  value = append(value, str_squish(gsub("[\r\n]", "", sub(".*:", "", matrix(unlist(i[2]),ncol=1,byrow=TRUE)))))
  
  df = data.frame(col_name, value)
  df = spread(df, col_name, value)
  df_movie_summary = fastmerge(df_movie_summary,df)
}

#df_movie_summary cleaning
df_movie_summary$Budget = gsub("\\s*\\([^\\)]+\\)","", df_movie_summary$Budget)
df_movie_summary$`Also Known As` = gsub(" See more »", "", df_movie_summary$`Also Known As`) 
df_movie_summary$`Filming Locations` = gsub(" See more »", "", df_movie_summary$`Filming Locations`) 
df_movie_summary$`Production Co` = gsub(" See more »", "", df_movie_summary$`Production Co`) 
df_movie_summary$`Release Date` = gsub(" See more »", "", df_movie_summary$`Release Date`) 
df_movie_summary$Country = gsub(" | ", ",", df_movie_summary$Country, fixed = T) 
df_movie_summary$Color = gsub("|", ",", df_movie_summary$Color, fixed = T) 
df_movie_summary$Language = gsub(" | ", ",", df_movie_summary$Language, fixed = T) 
df_movie_summary$`Official Sites` = gsub(" | ", ",", df_movie_summary$`Official Sites`, fixed = T)
df_movie_summary$Runtime = gsub("|", ",", df_movie_summary$Runtime, fixed = T) 
df_movie_summary$`Sound Mix` = gsub("|", ",", df_movie_summary$`Sound Mix`, fixed = T) 
df_movie_summary$`Show more on IMDbPro »` = NULL

#page1 summary_url
summary_url = gsub('.{15}$', '', movie_href_1)
#all page summary_url
summary_url_all =  gsub('.{15}$', '', movie_href_all)

#get cast members
print("scraping cast")
cast = lapply(paste0(url_base_detail,summary_url_all,"fullcredits?ref_=tt_ql_dt_1"),
              function(url){
                list(
                title = html_text(html_nodes(read_html(url), xpath = "//*[@id='fullcredits_content']/h4")),
                staff = html_text(html_nodes(read_html(url), xpath = "//*[@id='fullcredits_content']/table")),
                cast_num = length(html_text(html_nodes(read_html(url), xpath = "/html/body/div[3]/div/div[2]/div[3]/div[1]/div[1]/div[2]/table/tbody/tr/td[1]/a")))
                )
              })

col_name_cast = str_squish(gsub("[\r\n]", "", sub("\\:.*", "", matrix(unlist(cast[[1]][1]),ncol=1,byrow=TRUE))))
col_name_cast[2] = "Writing Credits"
col_name_cast[3] = "Cast"
col_name_cast = append(col_name_cast, "Cast_num")
df_movie_cast = data.frame(matrix(ncol = length(col_name_cast), nrow = 0))
colnames(df_movie_cast) = col_name_cast
  
for (i in cast){
  col_name_cast = str_squish(gsub("[\r\n]", "", sub("\\:.*", "", matrix(unlist(i[1]),ncol=1,byrow=TRUE))))
  col_name_cast[2] = "Writing Credits"
  col_name_cast[3] = "Cast"
  col_name_cast = append(col_name_cast, "Cast_num")
  value = str_squish(gsub("[\r\n]", "",matrix(unlist(i[2]),ncol=1,byrow=TRUE)))
  #remove bracket and all the words in lowercase
  value = gsub("...", ",", gsub("\\b[a-z][^ ]*(\\s+)?","",gsub("\\s*\\([^\\)]+\\)", "", value)), fixed = T) 
  value = append(value, unlist(i[3]))
  
  df_cast = data.frame(col_name_cast, value)
  df_cast = spread(df_cast, col_name_cast, value)
  df_movie_cast = fastmerge(df_movie_cast,df_cast)
}


#get awards
print("scraping awards")
awards = lapply(paste0(url_base_detail,summary_url_all,"awards?ref_=tt_ql_op_1"),
              function(url){
                list(
                award_summary = html_text(html_nodes(read_html(url), xpath = "/html/body/div[3]/div/div[2]/div[3]/div[1]/div[1]/div/div[2]/div/div")),
                award_title = html_text(html_nodes(read_html(url), xpath = "/html/body/div[3]/div/div[2]/div[3]/div[1]/div[1]/div/h3")),
                award_detail = html_text(html_nodes(read_html(url), xpath = "/html/body/div[3]/div/div[2]/div[3]/div[1]/div[1]/div/table"))
                )
              })

col_name_award = c("award_summary", "award_title", "award_detail")
df_movie_award = data.frame(matrix(ncol = length(col_name_award), nrow = 0))
colnames(df_movie_award) = col_name_award

for (i in awards){
  col_name_award = c("award_summary", "award_title", "award_detail")
  value_award = gsub("win and", "", gsub("wins and ", "", gsub("Showing all ", "", gsub(" nominations", "", i[1]))))
  value_award = append(value_award, paste(gsub("\\s+"," ",matrix(unlist(i[2]),ncol=1,byrow=TRUE)), collapse = ','))
  value_award = append(value_award, paste(gsub("\\s+"," ",matrix(unlist(i[3]),ncol=1,byrow=TRUE)), collapse = ','))
  
  df_award = data.frame(col_name_award, value_award)
  df_award = spread(df_award, col_name_award, value_award)
  df_movie_award = fastmerge(df_movie_award,df_award)
}

#if movie do not have any awards -> character(0) -> NA
df_movie_award[df_movie_award=="character(0)"] <- NA
df_movie_award = df_movie_award %>% separate(award_summary,  c("wins", "nominations" ), " ")

#get filming locations
print("scraping locations")
locations = lapply(paste0(url_base_detail,summary_url_all,"locations?ref_=tt_ql_dt_5#filming_dates"),
                function(url){
                  list(
                    num_loc = html_text(html_nodes(read_html(url), xpath = "/html/body/div[3]/div/div[2]/div[3]/div[1]/section/div[2]")),
                    film_dates = html_text(html_nodes(read_html(url), xpath = "/html/body/div[3]/div/div[2]/div[3]/div[1]/section/section[2]/ul/li")),
                    prod_dates = html_text(html_nodes(read_html(url), xpath = "/html/body/div[3]/div/div[2]/div[3]/div[1]/section/section[3]/ul/li"))
                  )
                })

col_name_location = c("Locations", "Filming_Dates", "Production_Dates")
df_movie_location = data.frame(matrix(ncol = length(col_name_location), nrow = 0))
colnames(df_movie_location) = col_name_location

for (i in locations){
  col_name_location = c("Locations", "Filming_Dates", "Production_Dates")
  value_location = gsub(" items", "", gsub(" Showing all ", "", gsub("\\s+"," ",gsub("\n", " ", i[1]))))
  value_location = append(value_location, paste(gsub("\\s+"," ",matrix(unlist(i[2]),ncol=1,byrow=TRUE)), collapse = ','))
  value_location = append(value_location, paste(gsub("\\s+"," ",matrix(unlist(i[3]),ncol=1,byrow=TRUE)), collapse = ','))
  
  df_location = data.frame(col_name_location, value_location)
  df_location = spread(df_location, col_name_location, value_location)
  df_movie_location = fastmerge(df_movie_location,df_location)
}


df_movie_location$Filming_Days = 
  as.numeric(anydate(gsub("\\s+"," ",sub('.*-', '', df_movie_location$Filming_Dates))))-
  as.numeric(anydate(gsub("\\s+"," ",sub('-.*', '', df_movie_location$Filming_Dates[1]))))


#get rating
print("scraping rating")
rating = lapply(paste0(url_base_detail,summary_url_all,"ratings?ref_=tt_ql_op_4"),
                   function(url){
                     html_text(html_nodes(read_html(url), xpath = "/html/body/div[3]/div/div[2]/div[3]/div[1]/section/div/div[3]/div/table[2]"))

                   })
rating_cleaned = rating[!sapply(rating, identical, character(0))]

col_name_rating = c("All", "Males", "Females")
df_movie_rating = data.frame(matrix(ncol = length(col_name_rating), nrow = 0))
colnames(df_movie_rating) = col_name_rating

for (i in rating_cleaned){
  col_name_rating = c("All", "Males", "Females")
  value_rating = gsub("-", "NA NA",substring(sub(" Males.*", "", gsub("\\s+"," ",gsub("\n", " ", i))), 35))
  value_rating = append(value_rating, paste(substring(gsub("-", "NA NA", sub(" .*Males", "", sub(" Females.*", "", gsub("\\s+"," ",gsub("\n", " ", i))))),2), collapse = ','))
  value_rating = append(value_rating, paste( gsub('.{0,1}$', '', substring(gsub("-", "NA NA", sub(" .*Females", "", gsub("\\s+"," ",gsub("\n", " ", i)))),2)), collapse = ','))
  
  df_rating = data.frame(col_name_rating, value_rating)
  df_rating = spread(df_rating, col_name_rating, value_rating)
  df_movie_rating = fastmerge(df_movie_rating,df_rating)
}

df_movie_rating = df_movie_rating %>% separate(All,  c("rating_all_ages", "rating_all_ages_ppl", "rating_all_lower_18", "rating_all_lower_18_ppl",
                                     "rating_all_18_29", "rating_all_18_29_ppl", "rating_all_30_44", "rating_all_30_44_ppl", 
                                     "rating_all_up_45", "rating_all_up_45_ppl" ), " ")
df_movie_rating = df_movie_rating %>% separate(Males,  c("rating_males", "rating_males_ppl", "rating_males_lower_18", "rating_males_lower_18_ppl",
                                     "rating_males_18_29", "rating_males_18_29_ppl", "rating_males_30_44", "rating_males_30_44_ppl", 
                                     "rating_males_up_45", "rating_males_up_45_ppl" ), " ")
df_movie_rating = df_movie_rating %>% separate(Females,  c("rating_females", "rating_females_ppl", "rating_females_lower_18", "rating_females_lower_18_ppl",
                                     "rating_females_18_29", "rating_females_18_29_ppl", "rating_females_30_44", "rating_females_30_44_ppl", 
                                     "rating_females_up_45", "rating_females_up_45_ppl" ), " ")


#---------------------------------------------------Start Combining Dataframe-------------------------------------------------

#special case row 89 and 138 which have the same movie name
df_movie_summary[89,]$Name = "Drishyam (2013)"
df_movie_summary[138,]$Name = "Drishyam (2015)"

#Change Column Name
colnames(df_movie_cast)[31] = "Writer"

#Combine different dataframe
df_movie_stars$Name = df_movie_summary$Name
df_movie_genre$Name = df_movie_summary$Name
df_movie_award$Name = df_movie_summary$Name
df_movie_cast$Name = df_movie_summary$Name
df_movie_location$Name = df_movie_summary$Name

df_rating_name = df_movie_summary
df_rating_name = df_rating_name[-c(465, 466, 578), ]
df_movie_rating$Name = df_rating_name$Name

df_movie_summary$`Also Known As` = NULL
df_movie_summary$`Aspect Ratio` = NULL

movie_final = merge(df_movie_summary, df_movie_award, by="Name", all=TRUE)
movie_final = merge(movie_final, df_movie_stars, by="Name", all=TRUE)
movie_final = merge(movie_final, df_movie_genre, by="Name", all=TRUE)
movie_final = merge(movie_final, df_movie_cast, by="Name", all=TRUE)
movie_final = merge(movie_final, df_movie_location, by="Name", all=TRUE)
movie_final = merge(movie_final, df_movie_rating, by="Name", all=TRUE)

movie_final$Writer = gsub("&", "", movie_final$Writer )

movie_final = movie_final %>% cSplit(c("Language", "Production Co", "Sound Mix",
                                       "Filming Locations", "Color","Country",
                                       "Genre", "Stars", "Filming_Dates", "Runtime",
                                       "Directed by", "Writer"), ",")
#More Cleaning

#Date Feature
movie_final$Release_Year = stri_extract_last_regex(movie_final$`Release Date`, "\\d{4}")
movie_final$Release_Month = gsub("[^a-zA-Z]", "", gsub("\\s*\\([^\\)]+\\)","",as.character(movie_final$`Release Date`)))

#Budget Feature
movie_final$Exchange_rate = sub("^([[:alpha:]]*).*", "\\1", movie_final$Budget)
movie_final$Budget[movie_final$Budget=="NA"] = 0
movie_final$Budget = gsub("[^0-9.-]", "", movie_final$Budget)
movie_final$Budget = as.numeric(movie_final$Budget)
movie_final$Exchange_rate[movie_final$Exchange_rate=="NA"] = 1
movie_final$Exchange_rate[movie_final$Exchange_rate==""] = 1

movie_final$Exchange_rate[movie_final$Exchange_rate=="INR"] = 0.01354
movie_final$Exchange_rate[movie_final$Exchange_rate=="EUR"] = 1.21524
movie_final$Exchange_rate[movie_final$Exchange_rate=="FRF"] = 0.1736
movie_final$Exchange_rate[movie_final$Exchange_rate=="ESP"] = 0.007285
movie_final$Exchange_rate[movie_final$Exchange_rate=="KRW"] = 0.000917
movie_final$Exchange_rate[movie_final$Exchange_rate=="JPY"] = 0.00961
movie_final$Exchange_rate[movie_final$Exchange_rate=="RUR"] = 0.01337
movie_final$Exchange_rate[movie_final$Exchange_rate=="GBP"] = 1.345976
movie_final$Exchange_rate[movie_final$Exchange_rate=="CAD"] = 0.77447
movie_final$Exchange_rate[movie_final$Exchange_rate=="BRL"] = 0.193241
movie_final$Exchange_rate[movie_final$Exchange_rate=="DEM"] = 0.6197
movie_final$Exchange_rate[movie_final$Exchange_rate=="ITL"] = 0.000189
movie_final$Exchange_rate[movie_final$Exchange_rate=="TRL"] = 0.119793
movie_final$Exchange_rate[movie_final$Exchange_rate=="AUD"] = 0.74331
movie_final$Exchange_rate[movie_final$Exchange_rate=="SEK"] = 0.11804
movie_final$Exchange_rate[movie_final$Exchange_rate=="DKK"] = 0.16323

movie_final$Exchange_rate = as.numeric(movie_final$Exchange_rate)
movie_final$Budget = movie_final$Budget * movie_final$Exchange_rate
#1eur = 1936.27ITL
# 1 rub = 1000 rur
#1 EUR is equivalent to 166.386 ESP



#transform the column to numeric 
movie_final$`Cumulative Worldwide Gross` = substring(movie_final$`Cumulative Worldwide Gross`,2)
movie_final$`Cumulative Worldwide Gross` <- gsub(",", "", movie_final$`Cumulative Worldwide Gross`) 
movie_final$`Cumulative Worldwide Gross` = as.numeric(movie_final$`Cumulative Worldwide Gross`)

movie_final$Profitability = movie_final$`Cumulative Worldwide Gross`/movie_final$Budget

movie_final$rating_all_ages_ppl = as.numeric(gsub(",","",movie_final$rating_all_ages_ppl))
movie_final$rating_all_lower_18_ppl = as.numeric(gsub(",","",movie_final$rating_all_lower_18_ppl))
movie_final$rating_all_18_29_ppl = as.numeric(gsub(",","",movie_final$rating_all_18_29_ppl))
movie_final$rating_all_30_44_ppl = as.numeric(gsub(",","",movie_final$rating_all_30_44_ppl))
movie_final$rating_all_up_45_ppl = as.numeric(gsub(",","",movie_final$rating_all_up_45_ppl))

movie_final$rating_males_ppl = as.numeric(gsub(",","",movie_final$rating_males_ppl))
movie_final$rating_males_lower_18_ppl = as.numeric(gsub(",","",movie_final$rating_males_lower_18_ppl))
movie_final$rating_males_18_29_ppl = as.numeric(gsub(",","",movie_final$rating_males_18_29_ppl))
movie_final$rating_males_30_44_ppl = as.numeric(gsub(",","",movie_final$rating_males_30_44_ppl))
movie_final$rating_males_up_45_ppl = as.numeric(gsub(",","",movie_final$rating_males_up_45_ppl))

movie_final$rating_females_ppl = as.numeric(gsub(",","",movie_final$rating_females_ppl))
movie_final$rating_females_lower_18_ppl = as.numeric(gsub(",","",movie_final$rating_females_lower_18_ppl))
movie_final$rating_females_18_29_ppl = as.numeric(gsub(",","",movie_final$rating_females_18_29_ppl))
movie_final$rating_females_30_44_ppl = as.numeric(gsub(",","",movie_final$rating_females_30_44_ppl))
movie_final$rating_females_up_45_ppl = as.numeric(gsub(",","",movie_final$rating_females_up_45_ppl))

#remove redundant columns
movie_final <- remove_empty(movie_final)
write.csv(movie_final,"RMBI_3010_asg2.csv", row.names = FALSE)