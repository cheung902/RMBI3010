library(dplyr)
library(tidyr)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(shinythemes)
library(ggthemes)
library(DT)
library(maps)
library(data.table)
library(reshape2)
library(ggwordcloud)
library(shinyalert)

data = read.csv("RMBI_3010_asg2.csv")

#-----------------------------------------------Graph Generation------------------------------------------------

# Since the dataframe is large, I will extract the only useful columns to further creature the features.

#-----------------------------------------------Overview Page----------------------------------------------------

# top 20 - Unused
data_top20 = data[order(data$Cumulative.Worldwide.Gross, decreasing =T),][0:20,]
data_top20$Name = reorder(data_top20$Name, data_top20$Cumulative.Worldwide.Gross)

graph_top20 =   ggplotly(ggplot(data=data_top20, aes(x= Name, y=Cumulative.Worldwide.Gross), fill=Name) +
                           geom_bar(stat="identity", width=0.5) + 
                           scale_y_continuous(labels = scales::comma) +
                           labs(title = "Top 20 Movie With Highest Worldwide Gross",
                                caption = "Data source: IMDb", x = "Movie Name") +
                           coord_flip() +
                           theme(plot.title = element_text(color="red", size=14, face="bold.italic")))

# World map
WorldData <- map_data('world')
world_freq = as.data.frame(table(data$Country_01))
names(world_freq)[1] = "region"
worldSubset = inner_join(WorldData, world_freq, by = "region")

plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)
colnames(worldSubset)[5] = "Region"
colnames(worldSubset)[7] = "Moives"
world = ggplotly(ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group, label= Region)) + 
                    coord_fixed(1.3) +
                    geom_polygon(aes(fill = Moives)) +
                    scale_fill_distiller(palette ="RdBu", direction = -1) + # or direction=1
                    ggtitle("Top 1000 - Number of Movies Produced In Each Countries ") +
                    plain)

# Worldwide by year and month
data_worldwide_year = aggregate(x = data$Cumulative.Worldwide.Gross, by=list(Year=data$Release_Year), FUN=sum, na.rm = T)
colnames(data_worldwide_year)[2] = "Worldwide_Gross"
data_budget_year = aggregate(x = data$Budget, by=list(Year=data$Release_Year), FUN=sum, na.rm = T)
colnames(data_budget_year)[2] = "Budget"

data_worldwide_month = aggregate(x = data$Cumulative.Worldwide.Gross, by=list(Month=data$Release_Month), FUN=sum, na.rm = T)
data_worldwide_month = data_worldwide_month[-1,]
colnames(data_worldwide_month)[2] = "Worldwide_Gross"
data_budget_month = aggregate(x = data$Budget, by=list(Month=data$Release_Month), FUN=sum, na.rm = T)
data_budget_month = data_budget_month[-1,]
colnames(data_budget_month)[2] = "Budget"
data_worldwide_month = merge(data_worldwide_month,data_budget_month, by= "Month")
data_worldwide_month$Month = factor(data_worldwide_month$Month, 
                                    levels = c("January", "February", "March", "April", "May", "June",
                                               "July", "August", "September", "October", "November", "December"))

data_worldwide_year = merge(data_worldwide_year, data_budget_year, by = "Year")

graph_yr_worldgross = ggplotly(ggplot(data = gather(data_worldwide_year,key,value,Worldwide_Gross,Budget)
                                      ,aes(x = Year, y = value, colour = key)) + 
                                 geom_line() +
                                 scale_y_continuous(labels = scales::comma)+
                                 theme_hc() + scale_colour_hc())

graph_month_worldgross = ggplotly(ggplot(data = gather(data_worldwide_month,key, value, Worldwide_Gross,Budget)
                                         ,aes(x = Month, y = value, group = key, colour = key)) + 
                                    geom_line() +
                                    scale_y_continuous(labels = scales::comma)+
                                    theme_hc() + scale_colour_hc())

rating_over_years = select(data, Release_Year, rating_all_ages)
names(rating_over_years) = c("Year", "Rating")
rating_over_years = ggplotly(ggplot(rating_over_years, aes(x=Year, y=Rating, group = Year))+
                               coord_cartesian(ylim = c(7,10))
                             + geom_boxplot() + theme_hc())

#-----------------------------------------------Pouplarity Page----------------------------------------------------
#genre gross
data_genre = select(data, Budget, Cumulative.Worldwide.Gross, Release_Year,
                    rating_all_ages,rating_all_ages_ppl,rating_males_ppl,rating_females_ppl, Genre_01, Genre_02, Genre_03)
genre1_gross = aggregate(x = data_genre$Cumulative.Worldwide.Gross, by=list(Genre=data_genre$Genre_01, Year = data_genre$Release_Year), FUN=sum, na.rm = T)
genre2_gross = aggregate(x = data_genre$Cumulative.Worldwide.Gross, by=list(Genre=data_genre$Genre_02, Year = data_genre$Release_Year), FUN=sum, na.rm = T)
genre3_gross = aggregate(x = data_genre$Cumulative.Worldwide.Gross, by=list(Genre=data_genre$Genre_03, Year = data_genre$Release_Year), FUN=sum, na.rm = T)

genreAll_gross = merge(genre1_gross, genre2_gross, by=c("Genre","Year"), all=TRUE)
genreAll_gross = merge(genreAll_gross, genre3_gross, by=c("Genre","Year"), all=TRUE)
genreAll_gross$Worldwide_Gross = rowSums(genreAll_gross[,c("x.x", "x.y", "x")], na.rm = T)
genreAll_gross = genreAll_gross[,-c(3,4,5)]

graph_genre_worldgross = ggplotly(ggplot(data = genreAll_gross, 
                                         aes(x = Year, y = Worldwide_Gross , group = Genre, color = Genre)) + 
                                    geom_line() +
                                    scale_y_continuous(labels = scales::comma) +
                                    theme_hc())

#genre gross
genre1_gross = aggregate(x = data_genre$Cumulative.Worldwide.Gross, by=list(Genre=data_genre$Genre_01, Year = data_genre$Release_Year), FUN=mean, na.rm = T)
# genre2_gross = aggregate(x = data_genre$Cumulative.Worldwide.Gross, by=list(Genre=data_genre$Genre_02, Year = data_genre$Release_Year), FUN=mean, na.rm = T)
# genre3_gross = aggregate(x = data_genre$Cumulative.Worldwide.Gross, by=list(Genre=data_genre$Genre_03, Year = data_genre$Release_Year), FUN=mean, na.rm = T)
# 
# genreAll_gross = merge(genre1_gross, genre2_gross, by=c("Genre","Year"), all=TRUE)
# genreAll_gross = merge(genreAll_gross, genre3_gross, by=c("Genre","Year"), all=TRUE)
# genreAll_gross$Worldwide_Gross = rowSums(genreAll_gross[,c("x.x", "x.y", "x")], na.rm = T)
# genreAll_gross = genreAll_gross[,-c(3,4,5)]

colnames(genre1_gross)[3] = "Average_Worldwide_Gross"
graph_genre_worldgross = ggplotly(ggplot(data = genre1_gross, 
                                         aes(x = Year, y = Average_Worldwide_Gross , group = Genre, color = Genre)) + 
                                    geom_line() +
                                    scale_y_continuous(labels = scales::comma) +
                                    theme_hc() + scale_colour_hc())

rank_genre_gross = genre1_gross %>%
  group_by(Year) %>%
  mutate(Rank = order(order(Year, Average_Worldwide_Gross, decreasing=TRUE)))
rank_genre_gross = ggplotly(ggplot(rank_genre_gross, aes(x = Year, y = Rank, group = Genre)) +
  geom_line(aes(color = Genre, alpha = 1), size = 0.3) +
  geom_point(aes(color = Genre, alpha = 1), size = 1) +
  scale_y_reverse(breaks = 1:nrow(rank_genre_gross))+
    theme_hc() + scale_colour_hc())

#genre budget
genre1_budget = aggregate(x = data_genre$Budget, by=list(Genre=data_genre$Genre_01, Year = data_genre$Release_Year), FUN=mean, na.rm = T)
# genre2_budget = aggregate(x = data_genre$Budget, by=list(Genre=data_genre$Genre_02, Year = data_genre$Release_Year), FUN=mean, na.rm = T)
# genre3_budget = aggregate(x = data_genre$Budget, by=list(Genre=data_genre$Genre_03, Year = data_genre$Release_Year), FUN=mean, na.rm = T)
# 
# genreAll_budget = merge(genre1_budget, genre2_budget, by=c("Genre","Year"), all=TRUE)
# genreAll_budget = merge(genreAll_budget, genre3_budget, by=c("Genre","Year"), all=TRUE)
# genreAll_budget$Budget = rowSums(genreAll_budget[,c("x.x", "x.y", "x")], na.rm = T)
# genreAll_budget = genreAll_budget[,-c(3,4,5)]

colnames(genre1_budget)[3] = "Average_Budget"
graph_genre_budget = ggplotly(ggplot(data = genre1_budget, 
                                         aes(x = Year, y = Average_Budget , group = Genre, color = Genre)) + 
                                    geom_line() +
                                    scale_y_continuous(labels = scales::comma) +
                                    theme_hc() + scale_colour_hc())

rank_genre_budget = genre1_budget %>%
  group_by(Year) %>%
  mutate(Rank = order(order(Year, Average_Budget, decreasing=TRUE)))
rank_genre_budget = ggplotly(ggplot(rank_genre_budget, aes(x = Year, y = Rank, group = Genre)) +
                              geom_line(aes(color = Genre, alpha = 1), size = 0.3) +
                              geom_point(aes(color = Genre, alpha = 1), size = 1) +
                              scale_y_reverse(breaks = 1:nrow(rank_genre_budget))+
                              theme_hc() + scale_colour_hc())

#genre avg rating
genre1_rating_all_ages = aggregate(x = data_genre$rating_all_ages, by=list(Genre=data_genre$Genre_01, Year = data_genre$Release_Year), FUN=mean, na.rm = T)

colnames(genre1_rating_all_ages)[3] = "Average_Rating"
graph_genre1_rating_all_ages = ggplotly(ggplot(data = genre1_rating_all_ages,
                                     aes(x = Year, y = Average_Rating , group = Genre, color = Genre)) +
                                geom_line() +
                                scale_y_continuous(labels = scales::comma) +
                                theme_hc() + scale_colour_hc())

rank_genre_rating = genre1_rating_all_ages %>%
  group_by(Year) %>%
  mutate(Rank = order(order(Year, Average_Rating, decreasing=TRUE)))
rank_genre_rating = ggplotly(ggplot(rank_genre_rating, aes(x = Year, y = Rank, group = Genre)) +
                              geom_line(aes(color = Genre, alpha = 1), size = 0.3) +
                              geom_point(aes(color = Genre, alpha = 1), size = 1) +
                              scale_y_reverse(breaks = 1:nrow(rank_genre_rating))+
                              theme_hc() + scale_colour_hc())

#genre by gender
data_genre_rating = select(data,Genre_01, rating_all_ages, rating_all_ages_ppl, rating_all_lower_18, rating_all_lower_18_ppl,
                           rating_all_18_29, rating_all_18_29_ppl, rating_all_30_44, rating_all_30_44_ppl, 
                           rating_all_up_45, rating_all_up_45_ppl,
                           rating_males, rating_males_ppl, rating_males_lower_18, rating_males_lower_18_ppl,
                           rating_males_18_29, rating_males_18_29_ppl, rating_males_30_44, rating_males_30_44_ppl, 
                           rating_males_up_45, rating_males_up_45_ppl,
                           rating_females, rating_females_ppl, rating_females_lower_18, rating_females_lower_18_ppl,
                           rating_females_18_29, rating_females_18_29_ppl, rating_females_30_44, rating_females_30_44_ppl, 
                           rating_females_up_45, rating_females_up_45_ppl
                           )




#genre combination
genre_cbn = select(data, Genre_01, Genre_02, Genre_03)
mdt = as.data.table(genre_cbn)
genre_cbn = mdt[,.N, by=names(mdt)]
colnames(genre_cbn)[4] = "Frequency"
genre_cbn = genre_cbn[order(genre_cbn$Frequency, decreasing = T),]

genre_cbn_gross = select(data, Genre_01, Genre_02, Genre_03,Cumulative.Worldwide.Gross)
genre_cbn_gross = aggregate(x = genre_cbn_gross$Cumulative.Worldwide.Gross,
                            by=list(Genre_01=genre_cbn_gross$Genre_01, Genre_02=genre_cbn_gross$Genre_02,Genre_03=genre_cbn_gross$Genre_03),
                            FUN=mean, na.rm = T)
colnames(genre_cbn_gross)[4] = "Average Worldwide Gross"
genre_cbn_gross = genre_cbn_gross[order(genre_cbn_gross$`Average Worldwide Gross`, decreasing = T),]

#Genre by Gender and Ages
# All gender
rating_all_all = aggregate(x = data_genre_rating$rating_all_ages, by=list(data_genre_rating$Genre_01), FUN=mean, na.rm = T)
rating_all_all = rating_all_all[order(rating_all_all$x, decreasing = T ),]
names(rating_all_all) = c("Genre_01", "Rating")
row.names(rating_all_all) <- NULL

rating_all_lower_18 = aggregate(x = data_genre_rating$rating_all_lower_18, by=list(data_genre_rating$Genre_01), FUN=mean, na.rm = T)
rating_all_lower_18 = rating_all_lower_18[order(rating_all_lower_18$x, decreasing = T ),]
names(rating_all_lower_18) = c("Genre_01", "Rating")
row.names(rating_all_lower_18) <- NULL

rating_all_18_29 = aggregate(x = data_genre_rating$rating_all_18_29, by=list(data_genre_rating$Genre_01), FUN=mean, na.rm = T)
rating_all_18_29 = rating_all_18_29[order(rating_all_18_29$x, decreasing = T ),]
names(rating_all_18_29) = c("Genre_01", "Rating")
row.names(rating_all_18_29) <- NULL

rating_all_30_44 = aggregate(x = data_genre_rating$rating_all_30_44, by=list(data_genre_rating$Genre_01), FUN=mean, na.rm = T)
rating_all_30_44 = rating_all_30_44[order(rating_all_30_44$x, decreasing = T ),]
names(rating_all_30_44) = c("Genre_01", "Rating")
row.names(rating_all_30_44) <- NULL

rating_all_up_45 = aggregate(x = data_genre_rating$rating_all_up_45, by=list(data_genre_rating$Genre_01), FUN=mean, na.rm = T)
rating_all_up_45 = rating_all_up_45[order(rating_all_up_45$x, decreasing = T ),]
names(rating_all_up_45) = c("Genre_01", "Rating")
row.names(rating_all_up_45) <- NULL

#--------------------------------------------------
#Male
rating_males = aggregate(x = data_genre_rating$rating_males, by=list(data_genre_rating$Genre_01), FUN=mean, na.rm = T)
rating_males = rating_males[order(rating_males$x, decreasing = T ),]
names(rating_males) = c("Genre_01", "Rating")
row.names(rating_males) <- NULL

rating_males_lower_18 = aggregate(x = data_genre_rating$rating_males_lower_18, by=list(data_genre_rating$Genre_01), FUN=mean, na.rm = T)
rating_males_lower_18 = rating_males_lower_18[order(rating_males_lower_18$x, decreasing = T ),]
names(rating_males_lower_18) = c("Genre_01", "Rating")
row.names(rating_males_lower_18) <- NULL

rating_males_18_29 = aggregate(x = data_genre_rating$rating_males_18_29, by=list(data_genre_rating$Genre_01), FUN=mean, na.rm = T)
rating_males_18_29 = rating_males_18_29[order(rating_males_18_29$x, decreasing = T ),]
names(rating_males_18_29) = c("Genre_01", "Rating")
row.names(rating_males_18_29) <- NULL

rating_males_30_44 = aggregate(x = data_genre_rating$rating_males_30_44, by=list(data_genre_rating$Genre_01), FUN=mean, na.rm = T)
rating_males_30_44 = rating_males_30_44[order(rating_males_30_44$x, decreasing = T ),]
names(rating_males_30_44) = c("Genre_01", "Rating")
row.names(rating_males_30_44) <- NULL

rating_males_up_45 = aggregate(x = data_genre_rating$rating_males_up_45, by=list(data_genre_rating$Genre_01), FUN=mean, na.rm = T)
rating_males_up_45 = rating_males_up_45[order(rating_males_up_45$x, decreasing = T ),]
names(rating_males_up_45) = c("Genre_01", "Rating")
row.names(rating_males_up_45) <- NULL

#--------------------------------------------------
#Female
rating_females = aggregate(x = data_genre_rating$rating_females, by=list(data_genre_rating$Genre_01), FUN=mean, na.rm = T)
rating_females = rating_females[order(rating_females$x, decreasing = T ),]
names(rating_females) = c("Genre_01", "Rating")
row.names(rating_females) <- NULL

rating_females_lower_18 = aggregate(x = data_genre_rating$rating_females_lower_18, by=list(data_genre_rating$Genre_01), FUN=mean, na.rm = T)
rating_females_lower_18 = rating_females_lower_18[order(rating_females_lower_18$x, decreasing = T ),]
names(rating_females_lower_18) = c("Genre_01", "Rating")
row.names(rating_females_lower_18) <- NULL

rating_females_18_29 = aggregate(x = data_genre_rating$rating_females_18_29, by=list(data_genre_rating$Genre_01), FUN=mean, na.rm = T)
rating_females_18_29 = rating_females_18_29[order(rating_females_18_29$x, decreasing = T ),]
names(rating_females_18_29) = c("Genre_01", "Rating")
row.names(rating_females_18_29) <- NULL

rating_females_30_44 = aggregate(x = data_genre_rating$rating_females_30_44, by=list(data_genre_rating$Genre_01), FUN=mean, na.rm = T)
rating_females_30_44 = rating_females_30_44[order(rating_females_30_44$x, decreasing = T ),]
names(rating_females_30_44) = c("Genre_01", "Rating")
row.names(rating_females_30_44) <- NULL

rating_females_up_45 = aggregate(x = data_genre_rating$rating_females_up_45, by=list(data_genre_rating$Genre_01), FUN=mean, na.rm = T)
rating_females_up_45 = rating_females_up_45[order(rating_females_up_45$x, decreasing = T ),]
names(rating_females_up_45) = c("Genre_01", "Rating")
row.names(rating_females_up_45) <- NULL

data$Runtime_01 = as.numeric(gsub("([0-9]+).*$", "\\1", data$Runtime_01))
data$Locations = as.numeric(data$Locations)
rowSums(!is.na(select(data, Stars_01, Stars_02, Stars_03)))

corr_rating_budget = ggplotly(ggplot(data, aes(x = Budget, y = rating_all_ages)) + geom_point() + geom_smooth() + theme_hc() + scale_colour_hc())
corr_rating_castNum = ggplotly(ggplot(data, aes(x = Cast_num, y = rating_all_ages)) + geom_point() + geom_smooth() + theme_hc() + scale_colour_hc())
corr_rating_runtime = ggplotly(ggplot(data, aes(x = Runtime_01, y = rating_all_ages)) + geom_point() + geom_smooth() + theme_hc() + scale_colour_hc())
corr_rating_ppl = ggplotly(ggplot(data, aes(x = rating_all_ages_ppl, y = rating_all_ages)) + geom_point() + geom_smooth() + theme_hc() + scale_colour_hc())
corr_rating_locaion = ggplotly(ggplot(data, aes(x = Locations, y = rating_all_ages)) + geom_point() + geom_smooth() + theme_hc() + scale_colour_hc())

corr_gross_budget = ggplotly(ggplot(data, aes(x = Budget, y = Cumulative.Worldwide.Gross)) + geom_point() + geom_smooth() + theme_hc() + scale_colour_hc())
corr_gross_castNum = ggplotly(ggplot(data, aes(x = Cast_num, y = Cumulative.Worldwide.Gross)) + geom_point() + geom_smooth() + theme_hc() + scale_colour_hc())
corr_gross_runtime = ggplotly(ggplot(data, aes(x = Runtime_01, y = Cumulative.Worldwide.Gross)) + geom_point() + geom_smooth() + theme_hc() + scale_colour_hc())
corr_gross_ppl = ggplotly(ggplot(data, aes(x = rating_all_ages_ppl, y = Cumulative.Worldwide.Gross)) + geom_point() + geom_smooth() + theme_hc() + scale_colour_hc())
corr_gross_location = ggplotly(ggplot(data, aes(x = Locations, y = Cumulative.Worldwide.Gross)) + geom_point() + geom_smooth() + theme_hc() + scale_colour_hc())

ggplot(data, aes(x = Locations
                 , y = Cumulative.Worldwide.Gross)) + geom_point() + theme_hc() + scale_colour_hc()
#-------------------------------------
#Genre by People
#All
rating_all_ages_ppl = aggregate(x = data_genre_rating$rating_all_ages_ppl, by=list(data_genre_rating$Genre_01), FUN=sum, na.rm = T)
rating_all_ages_ppl = rating_all_ages_ppl[order(rating_all_ages_ppl$x, decreasing = T ),]
names(rating_all_ages_ppl) = c("Genre_01", "Number_of_People")
rating_all_ages_ppl$Genre_01 = reorder(rating_all_ages_ppl$Genre_01, rating_all_ages_ppl$Number_of_People)
rating_all_ages_ppl = ggplotly(ggplot(data = rating_all_ages_ppl, aes(x = Genre_01, y = Number_of_People, fill = Genre_01))+
                                 geom_bar(stat = "identity") + scale_y_continuous(labels = scales::comma))

rating_all_lower_18_ppl = aggregate(x = data_genre_rating$rating_all_lower_18_ppl, by=list(data_genre_rating$Genre_01), FUN=sum, na.rm = T)
rating_all_lower_18_ppl = rating_all_lower_18_ppl[order(rating_all_lower_18_ppl$x, decreasing = T ),]
names(rating_all_lower_18_ppl) = c("Genre_01", "Number_of_People")
rating_all_lower_18_ppl$Genre_01 = reorder(rating_all_lower_18_ppl$Genre_01, rating_all_lower_18_ppl$Number_of_People)
rating_all_lower_18_ppl = ggplotly(ggplot(data = rating_all_lower_18_ppl, aes(x = Genre_01, y = Number_of_People, fill = Genre_01))+
                                 geom_bar(stat = "identity") + scale_y_continuous(labels = scales::comma))

rating_all_18_29_ppl = aggregate(x = data_genre_rating$rating_all_18_29_ppl, by=list(data_genre_rating$Genre_01), FUN=sum, na.rm = T)
rating_all_18_29_ppl = rating_all_18_29_ppl[order(rating_all_18_29_ppl$x, decreasing = T ),]
names(rating_all_18_29_ppl) = c("Genre_01", "Number_of_People")
rating_all_18_29_ppl$Genre_01 = reorder(rating_all_18_29_ppl$Genre_01, rating_all_18_29_ppl$Number_of_People)
rating_all_18_29_ppl = ggplotly(ggplot(data = rating_all_18_29_ppl, aes(x = Genre_01, y = Number_of_People, fill = Genre_01))+
                                 geom_bar(stat = "identity") + scale_y_continuous(labels = scales::comma))

rating_all_30_44_ppl = aggregate(x = data_genre_rating$rating_all_30_44_ppl, by=list(data_genre_rating$Genre_01), FUN=sum, na.rm = T)
rating_all_30_44_ppl = rating_all_30_44_ppl[order(rating_all_30_44_ppl$x, decreasing = T ),]
names(rating_all_30_44_ppl) = c("Genre_01", "Number_of_People")
rating_all_30_44_ppl$Genre_01 = reorder(rating_all_30_44_ppl$Genre_01, rating_all_30_44_ppl$Number_of_People)
rating_all_30_44_ppl = ggplotly(ggplot(data = rating_all_30_44_ppl, aes(x = Genre_01, y = Number_of_People, fill = Genre_01))+
                                 geom_bar(stat = "identity") + scale_y_continuous(labels = scales::comma))

rating_all_up_45_ppl = aggregate(x = data_genre_rating$rating_all_up_45_ppl, by=list(data_genre_rating$Genre_01), FUN=sum, na.rm = T)
rating_all_up_45_ppl = rating_all_up_45_ppl[order(rating_all_up_45_ppl$x, decreasing = T ),]
names(rating_all_up_45_ppl) = c("Genre_01", "Number_of_People")
rating_all_up_45_ppl$Genre_01 = reorder(rating_all_up_45_ppl$Genre_01, rating_all_up_45_ppl$Number_of_People)
rating_all_up_45_ppl = ggplotly(ggplot(data = rating_all_up_45_ppl, aes(x = Genre_01, y = Number_of_People, fill = Genre_01))+
                                 geom_bar(stat = "identity") + scale_y_continuous(labels = scales::comma))

#--------------------------------------
#Male
rating_males_ppl = aggregate(x = data_genre_rating$rating_males_ppl, by=list(data_genre_rating$Genre_01), FUN=sum, na.rm = T)
rating_males_ppl = rating_males_ppl[order(rating_males_ppl$x, decreasing = T ),]
names(rating_males_ppl) = c("Genre_01", "Number_of_People")
rating_males_ppl$Genre_01 = reorder(rating_males_ppl$Genre_01, rating_males_ppl$Number_of_People)
rating_males_ppl = ggplotly(ggplot(data = rating_males_ppl, aes(x = Genre_01, y = Number_of_People, fill = Genre_01))+
                                 geom_bar(stat = "identity") + scale_y_continuous(labels = scales::comma))

rating_males_lower_18_ppl = aggregate(x = data_genre_rating$rating_males_lower_18_ppl, by=list(data_genre_rating$Genre_01), FUN=sum, na.rm = T)
rating_males_lower_18_ppl = rating_males_lower_18_ppl[order(rating_males_lower_18_ppl$x, decreasing = T ),]
names(rating_males_lower_18_ppl) = c("Genre_01", "Number_of_People")
rating_males_lower_18_ppl$Genre_01 = reorder(rating_males_lower_18_ppl$Genre_01, rating_males_lower_18_ppl$Number_of_People)
rating_males_lower_18_ppl = ggplotly(ggplot(data = rating_males_lower_18_ppl, aes(x = Genre_01, y = Number_of_People, fill = Genre_01))+
                              geom_bar(stat = "identity") + scale_y_continuous(labels = scales::comma))

rating_males_18_29_ppl = aggregate(x = data_genre_rating$rating_males_18_29_ppl, by=list(data_genre_rating$Genre_01), FUN=sum, na.rm = T)
rating_males_18_29_ppl = rating_males_18_29_ppl[order(rating_males_18_29_ppl$x, decreasing = T ),]
names(rating_males_18_29_ppl) = c("Genre_01", "Number_of_People")
rating_males_18_29_ppl$Genre_01 = reorder(rating_males_18_29_ppl$Genre_01, rating_males_18_29_ppl$Number_of_People)
rating_males_18_29_ppl = ggplotly(ggplot(data = rating_males_18_29_ppl, aes(x = Genre_01, y = Number_of_People, fill = Genre_01))+
                              geom_bar(stat = "identity") + scale_y_continuous(labels = scales::comma))

rating_males_30_44_ppl = aggregate(x = data_genre_rating$rating_males_30_44_ppl, by=list(data_genre_rating$Genre_01), FUN=sum, na.rm = T)
rating_males_30_44_ppl = rating_males_30_44_ppl[order(rating_males_30_44_ppl$x, decreasing = T ),]
names(rating_males_30_44_ppl) = c("Genre_01", "Number_of_People")
rating_males_30_44_ppl$Genre_01 = reorder(rating_males_30_44_ppl$Genre_01, rating_males_30_44_ppl$Number_of_People)
rating_males_30_44_ppl = ggplotly(ggplot(data = rating_males_30_44_ppl, aes(x = Genre_01, y = Number_of_People, fill = Genre_01))+
                              geom_bar(stat = "identity") + scale_y_continuous(labels = scales::comma))

rating_males_up_45_ppl = aggregate(x = data_genre_rating$rating_males_up_45_ppl, by=list(data_genre_rating$Genre_01), FUN=sum, na.rm = T)
rating_males_up_45_ppl = rating_males_up_45_ppl[order(rating_males_up_45_ppl$x, decreasing = T ),]
names(rating_males_up_45_ppl) = c("Genre_01", "Number_of_People")
rating_males_up_45_ppl$Genre_01 = reorder(rating_males_up_45_ppl$Genre_01, rating_males_up_45_ppl$Number_of_People)
rating_males_up_45_ppl = ggplotly(ggplot(data = rating_males_up_45_ppl, aes(x = Genre_01, y = Number_of_People, fill = Genre_01))+
                              geom_bar(stat = "identity") + scale_y_continuous(labels = scales::comma))
#--------------------------
#Female
rating_females_ppl = aggregate(x = data_genre_rating$rating_females_ppl, by=list(data_genre_rating$Genre_01), FUN=sum, na.rm = T)
rating_females_ppl = rating_females_ppl[order(rating_females_ppl$x, decreasing = T ),]
names(rating_females_ppl) = c("Genre_01", "Number_of_People")
rating_females_ppl$Genre_01 = reorder(rating_females_ppl$Genre_01, rating_females_ppl$Number_of_People)
rating_females_ppl = ggplotly(ggplot(data = rating_females_ppl, aes(x = Genre_01, y = Number_of_People, fill = Genre_01))+
                                    geom_bar(stat = "identity") + scale_y_continuous(labels = scales::comma))

rating_females_lower_18_ppl = aggregate(x = data_genre_rating$rating_females_lower_18_ppl, by=list(data_genre_rating$Genre_01), FUN=sum, na.rm = T)
rating_females_lower_18_ppl = rating_females_lower_18_ppl[order(rating_females_lower_18_ppl$x, decreasing = T ),]
names(rating_females_lower_18_ppl) = c("Genre_01", "Number_of_People")
rating_females_lower_18_ppl$Genre_01 = reorder(rating_females_lower_18_ppl$Genre_01, rating_females_lower_18_ppl$Number_of_People)
rating_females_lower_18_ppl = ggplotly(ggplot(data = rating_females_lower_18_ppl, aes(x = Genre_01, y = Number_of_People, fill = Genre_01))+
                                    geom_bar(stat = "identity") + scale_y_continuous(labels = scales::comma))

rating_females_18_29_ppl = aggregate(x = data_genre_rating$rating_females_18_29_ppl, by=list(data_genre_rating$Genre_01), FUN=sum, na.rm = T)
rating_females_18_29_ppl = rating_females_18_29_ppl[order(rating_females_18_29_ppl$x, decreasing = T ),]
names(rating_females_18_29_ppl) = c("Genre_01", "Number_of_People")
rating_females_18_29_ppl$Genre_01 = reorder(rating_females_18_29_ppl$Genre_01, rating_females_18_29_ppl$Number_of_People)
rating_females_18_29_ppl = ggplotly(ggplot(data = rating_females_18_29_ppl, aes(x = Genre_01, y = Number_of_People, fill = Genre_01))+
                                    geom_bar(stat = "identity") + scale_y_continuous(labels = scales::comma))

rating_females_30_44_ppl = aggregate(x = data_genre_rating$rating_females_30_44_ppl, by=list(data_genre_rating$Genre_01), FUN=sum, na.rm = T)
rating_females_30_44_ppl = rating_females_30_44_ppl[order(rating_females_30_44_ppl$x, decreasing = T ),]
names(rating_females_30_44_ppl) = c("Genre_01", "Number_of_People")
rating_females_30_44_ppl$Genre_01 = reorder(rating_females_30_44_ppl$Genre_01, rating_females_30_44_ppl$Number_of_People)
rating_females_30_44_ppl = ggplotly(ggplot(data = rating_females_30_44_ppl, aes(x = Genre_01, y = Number_of_People, fill = Genre_01))+
                                    geom_bar(stat = "identity") + scale_y_continuous(labels = scales::comma))

rating_females_up_45_ppl = aggregate(x = data_genre_rating$rating_females_up_45_ppl, by=list(data_genre_rating$Genre_01), FUN=sum, na.rm = T)
rating_females_up_45_ppl = rating_females_up_45_ppl[order(rating_females_up_45_ppl$x, decreasing = T ),]
names(rating_females_up_45_ppl) = c("Genre_01", "Number_of_People")
rating_females_up_45_ppl$Genre_01 = reorder(rating_females_up_45_ppl$Genre_01, rating_females_up_45_ppl$Number_of_People)
rating_females_up_45_ppl = ggplotly(ggplot(data = rating_females_up_45_ppl, aes(x = Genre_01, y = Number_of_People, fill = Genre_01))+
                                    geom_bar(stat = "identity") + scale_y_continuous(labels = scales::comma))

#-----------------------------------------------Cast Page-------------------------------------------------------
# Here I will divide the page development in to three parts - Director/Stars/Writer

#Director
numOfMovies_director_01 = select(data, Directed.by_01)
mdt = as.data.table(numOfMovies_director_01)
numOfMovies_director_01 = mdt[,.N, by=names(mdt)]
names(numOfMovies_director_01) = c("Director", "Movies_Number_01")

numOfMovies_director_02 = select(data, Directed.by_02)
mdt = as.data.table(numOfMovies_director_02)
numOfMovies_director_02 = mdt[,.N, by=names(mdt)]
names(numOfMovies_director_02) = c("Director", "Movies_Number_02")

numOfMovies_director_03 = select(data, Directed.by_03)
mdt = as.data.table(numOfMovies_director_03)
numOfMovies_director_03 = mdt[,.N, by=names(mdt)]
names(numOfMovies_director_03) = c("Director", "Movies_Number_03")

numOfMovies_director_04 = select(data, Directed.by_04)
mdt = as.data.table(numOfMovies_director_04)
numOfMovies_director_04 = mdt[,.N, by=names(mdt)]
names(numOfMovies_director_04) = c("Director", "Movies_Number_04")

numOfMovies_director = merge(numOfMovies_director_01,merge(numOfMovies_director_02,merge(numOfMovies_director_03,numOfMovies_director_04, by="Director", all=T), by="Director", all=T), by="Director", all=T)
numOfMovies_director$Movies_Number = rowSums(numOfMovies_director[,c("Movies_Number_01","Movies_Number_02","Movies_Number_03","Movies_Number_04")], na.rm = T)
numOfMovies_director = numOfMovies_director[-1,-c(2,3,4,5)]

numOfMovies_director = numOfMovies_director[numOfMovies_director$Movies_Number>5,]
numOfMovies_director$Director = reorder(numOfMovies_director$Director, numOfMovies_director$Movies_Number)
numOfMovies_director = ggplotly(ggplot(data = numOfMovies_director, aes(x = Director, y = Movies_Number, fill = Director))+
                                      geom_bar(stat = "identity") + scale_y_continuous(labels = scales::comma) + coord_flip())

score_ranking_director = select(data, Directed.by_01, rating_all_ages)
score_ranking_director = aggregate(score_ranking_director$rating_all_ages, by=list(score_ranking_director$Directed.by_01), FUN=mean, na.rm = T)
names(score_ranking_director) = c("Director", "Rating")
score_ranking_director$Director = reorder(score_ranking_director$Director, score_ranking_director$Rating)
score_ranking_director = score_ranking_director[order(score_ranking_director$Rating, decreasing =T),]
score_ranking_director = score_ranking_director[1:20,]
score_ranking_director = ggplotly(ggplot(data = score_ranking_director, aes(x = Director, y = Rating, fill = Director))+
                                    geom_bar(stat = "identity") + scale_y_continuous(labels = scales::comma) + coord_flip())

#Stars
numOfMovies_star_01 = select(data, Stars_01)
mdt = as.data.table(numOfMovies_star_01)
numOfMovies_star_01 = mdt[,.N, by=names(mdt)]
names(numOfMovies_star_01) = c("Star", "Movies_Number_01")

numOfMovies_star_02 = select(data, Stars_02)
mdt = as.data.table(numOfMovies_star_02)
numOfMovies_star_02 = mdt[,.N, by=names(mdt)]
names(numOfMovies_star_02) = c("Star", "Movies_Number_02")

numOfMovies_star_03 = select(data, Stars_03)
mdt = as.data.table(numOfMovies_star_03)
numOfMovies_star_03 = mdt[,.N, by=names(mdt)]
names(numOfMovies_star_03) = c("Star", "Movies_Number_03")

numOfMovies_star = merge(numOfMovies_star_01,merge(numOfMovies_star_02,numOfMovies_star_03, by="Star", all=T), by="Star", all=T)
numOfMovies_star$Movies_Number = rowSums(numOfMovies_star[,c("Movies_Number_01","Movies_Number_02","Movies_Number_03")], na.rm = T)
numOfMovies_star = numOfMovies_star[-1,-c(2,3,4)]
numOfMovies_star = numOfMovies_star[numOfMovies_star$Movies_Number>5,]
numOfMovies_star$Star = reorder(numOfMovies_star$Star, numOfMovies_star$Movies_Number)
numOfMovies_star = ggplotly(ggplot(data = numOfMovies_star, aes(x = Star, y = Movies_Number, fill = Star))+
                                  geom_bar(stat = "identity") + scale_y_continuous(labels = scales::comma) + coord_flip())


score_ranking_star_01 = select(data, Stars_01, rating_all_ages)
score_ranking_star_02 = select(data, Stars_02, rating_all_ages)
score_ranking_star_03 = select(data, Stars_03, rating_all_ages)
score_ranking_star_01 = aggregate(score_ranking_star_01$rating_all_ages, by = list(score_ranking_star_01$Stars_01), FUN = mean)
score_ranking_star_02 = aggregate(score_ranking_star_02$rating_all_ages, by = list(score_ranking_star_02$Stars_02), FUN = mean)
score_ranking_star_03 = aggregate(score_ranking_star_03$rating_all_ages, by = list(score_ranking_star_03$Stars_03), FUN = mean)
names(score_ranking_star_01) = c("Star", "Rating_01")
names(score_ranking_star_02) = c("Star", "Rating_02")
names(score_ranking_star_03) = c("Star", "Rating_03")
score_ranking_star = merge(score_ranking_star_02,score_ranking_star_03, by = "Star", all = T)
score_ranking_star = merge(score_ranking_star,score_ranking_star_01, by = "Star", all = T)
score_ranking_star$Rating = rowMeans(score_ranking_star[,c("Rating_01", "Rating_02", "Rating_03")], na.rm = T)
score_ranking_star$Star = reorder(score_ranking_star$Star, score_ranking_star$Rating)
score_ranking_star = score_ranking_star[order(score_ranking_star$Rating, decreasing =T),]
score_ranking_star = score_ranking_star[1:20,]
score_ranking_star = ggplotly(ggplot(data = score_ranking_star, aes(x = Star, y = Rating, fill = Star))+
                                    geom_bar(stat = "identity") + scale_y_continuous(labels = scales::comma) + coord_flip())

# Writing Credits
numOfMovies_WC_01 = select(data, Writer_01)
mdt = as.data.table(numOfMovies_WC_01)
numOfMovies_WC_01 = mdt[,.N, by=names(mdt)]
names(numOfMovies_WC_01) = c("Writer", "Movies_Number_01")

numOfMovies_WC_02 = select(data, Writer_02)
mdt = as.data.table(numOfMovies_WC_02)
numOfMovies_WC_02 = mdt[,.N, by=names(mdt)]
names(numOfMovies_WC_02) = c("Writer", "Movies_Number_02")

numOfMovies_WC_03 = select(data, Writer_03)
mdt = as.data.table(numOfMovies_WC_03)
numOfMovies_WC_03 = mdt[,.N, by=names(mdt)]
names(numOfMovies_WC_03) = c("Writer", "Movies_Number_03")

numOfMovies_writer = merge(numOfMovies_WC_01,merge(numOfMovies_WC_02,numOfMovies_WC_03, by="Writer", all=T), by="Writer", all=T)
numOfMovies_writer$Movies_Number = rowSums(numOfMovies_writer[,c("Movies_Number_01","Movies_Number_02","Movies_Number_03")], na.rm = T)
numOfMovies_writer = numOfMovies_writer[-1,-c(2,3,4)]
numOfMovies_writer = numOfMovies_writer[numOfMovies_writer$Movies_Number>5,]
numOfMovies_writer$Writer = reorder(numOfMovies_writer$Writer, numOfMovies_writer$Movies_Number)
numOfMovies_writer = ggplotly(ggplot(data = numOfMovies_writer, aes(x = Writer, y = Movies_Number, fill = Writer))+
                              geom_bar(stat = "identity") + scale_y_continuous(labels = scales::comma) + coord_flip())

score_ranking_writer_01 = select(data, Writer_01, rating_all_ages)
score_ranking_writer_02 = select(data, Writer_02, rating_all_ages)
score_ranking_writer_03 = select(data, Writer_03, rating_all_ages)
score_ranking_writer_01 = aggregate(score_ranking_writer_01$rating_all_ages, by = list(score_ranking_writer_01$Writer_01), FUN = mean)
score_ranking_writer_02 = aggregate(score_ranking_writer_02$rating_all_ages, by = list(score_ranking_writer_02$Writer_02), FUN = mean)
score_ranking_writer_03 = aggregate(score_ranking_writer_03$rating_all_ages, by = list(score_ranking_writer_03$Writer_03), FUN = mean)
names(score_ranking_writer_01) = c("Writer", "Rating_01")
names(score_ranking_writer_02) = c("Writer", "Rating_02")
names(score_ranking_writer_03) = c("Writer", "Rating_03")
score_ranking_writer = merge(score_ranking_writer_02,score_ranking_writer_03, by = "Writer", all = T)
score_ranking_writer = merge(score_ranking_writer,score_ranking_writer_01, by = "Writer", all = T)
score_ranking_writer$Rating = rowMeans(score_ranking_writer[,c("Rating_01", "Rating_02", "Rating_03")], na.rm = T)
score_ranking_writer$Writer = reorder(score_ranking_writer$Writer, score_ranking_writer$Rating)
score_ranking_writer = score_ranking_writer[order(score_ranking_writer$Rating, decreasing =T),]
score_ranking_writer = score_ranking_writer[1:20,]
score_ranking_writer = ggplotly(ggplot(data = score_ranking_writer, aes(x = Writer, y = Rating, fill = Writer))+
                                geom_bar(stat = "identity") + scale_y_continuous(labels = scales::comma) 
                                + coord_flip() + scale_colour_hc())

star_01 = select(data, Genre_01, Stars_01, Cumulative.Worldwide.Gross)
star_02 = select(data, Genre_01, Stars_02, Cumulative.Worldwide.Gross)
star_03 = select(data, Genre_01, Stars_03, Cumulative.Worldwide.Gross)
star_04 = select(data, Genre_01, Stars_04, Cumulative.Worldwide.Gross)
names(star_01) = c("Genre", "Star", "Worldwide_Gross")
names(star_01) = c("Genre", "Star", "Worldwide_Gross")
names(star_01) = c("Genre", "Star", "Worldwide_Gross")
names(star_01) = c("Genre", "Star", "Worldwide_Gross")

star = bind_rows(star_01,star_02,star_03,star_04)
star = aggregate(star$Worldwide_Gross, by=list(star$Genre, star$Star), FUN=mean, na.rm = T)
names(star) = c("Genre", "Star", "Worldwide_Gross")
genre_by_cast = select(data, Genre_01, Directed.by_01,Writer_01, Cumulative.Worldwide.Gross)
names(genre_by_cast) = c("Genre", "Director", "Writer", "Worldwide_Gross")

#-----------------------------------------------------------------
#Production House Analysis - Unused since not enough time to develop
prod = select(data, Cumulative.Worldwide.Gross, Production.Co_01, Production.Co_02, Production.Co_03, Production.Co_04, Release_Year)
prod_01 = select(prod, Production.Co_01, Cumulative.Worldwide.Gross, Release_Year)
prod_01 = aggregate(prod_01$Cumulative.Worldwide.Gross, by = list(prod_01$Production.Co_01, prod_01$Release_Year), FUN = sum, na.rm = T)

prod_02 = select(prod, Production.Co_02, Cumulative.Worldwide.Gross, Release_Year)
prod_02 = aggregate(prod_02$Cumulative.Worldwide.Gross, by = list(prod_02$Production.Co_02, prod_02$Release_Year), FUN = sum, na.rm = T)

prod_03 = select(prod, Production.Co_03, Cumulative.Worldwide.Gross, Release_Year)
prod_03 = aggregate(prod_03$Cumulative.Worldwide.Gross, by = list(prod_03$Production.Co_03, prod_03$Release_Year), FUN = sum, na.rm = T)

prod_04 = select(prod, Production.Co_04, Cumulative.Worldwide.Gross, Release_Year)
prod_04 = aggregate(prod_04$Cumulative.Worldwide.Gross, by = list(prod_04$Production.Co_04, prod_04$Release_Year), FUN = sum, na.rm = T)

prod = bind_rows(prod_01, prod_02, prod_03, prod_04)


prod_name_01 = select(data, Name, Production.Co_01)
prod_name_02 = select(data, Name, Production.Co_02)
prod_name_03 = select(data, Name, Production.Co_03)
prod_name_04 = select(data, Name, Production.Co_04)
names(prod_name_01) = c("Name", "Production_House")
names(prod_name_02) = c("Name", "Production_House")
names(prod_name_03) = c("Name", "Production_House")
names(prod_name_04) = c("Name", "Production_House")
prod_name = bind_rows(prod_name_01, prod_name_02, prod_name_03, prod_name_04)
prod_name = na.omit(prod_name)

ui = dashboardPage(
  dashboardHeader(title = "Movie Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "Overview", icon = icon("dashboard")),
      menuItem("Popularity", tabName = "Popularity", icon = icon("fire-alt")),
      menuItem("Cast", tabName = "Cast", icon = icon("users")),
      menuItem("Analysis", tabName = "Analysis", icon = icon("chart-bar")),
      menuItem("Movie Detail", tabName = "movie_detail", icon = icon("film")),
      sidebarSearchForm(textId = "searchMovie", buttonId = "searchButton",
                        label = "Input Movie's Name", icon = icon("search"))
    )
  ),
  
  dashboardBody(
    
    tabItems(
      # Overview tab's content
      tabItem(tabName = "Overview",
              fluidRow(box(title = strong("Produced Movies' Number In Each Countries"),plotlyOutput("worldMap"), width = "100%")),
              fluidRow(tabBox(title = strong("Market Overview"), id = "Tab_Worldwide_Gross", 
                              tabPanel("Worldwide Gross and Budget Over Years", graph_yr_worldgross), 
                              tabPanel("Worldwide Gross and Budget Over Months", graph_month_worldgross)),
                       tabBox(title = strong("Ranking"), id = "Tab_Rank",
                              tabPanel("By Worldwide Gross",
                                       numericInput(inputId = "rank_wg",h3("Top Rank"),value = 10) ,
                                       dataTableOutput("tableWorldwide")),
                              tabPanel("By Budget",
                                       numericInput(inputId = "rank_budget",h3("Top Rank"),value = 10) ,
                                       dataTableOutput("tableBudget"))

                              )
                       )
              ),
      # Popularity tab's content
      tabItem(tabName = "Popularity",
              fluidRow(tabBox(title = strong("Popularity Among Genders and Ages"), id = "Box_Genre_Genders_Ages",
                              tabPanel("Average rating",
                                       fluidRow(
                                         column(width = 6, selectInput("select", inputId = "Genders_rating", label = h3("Gender"),
                                                choices = list("All" = 1, "Male" = 2, "Female" = 3),
                                                )),
                                         column(width = 6, selectInput("select", inputId = "Ages_rating", label = h3("Ages"),
                                                choices = list("ALL" = 1, "Under 18" = 2, "18 - 29" = 3,
                                                               "30 - 44" = 4, "45 And Above" = 5),
                                                ))),
                                       fluidRow(dataTableOutput("Gender_Ages_Rating"))
                                       ),
                              tabPanel("Numbers Of People Voted In The Genre",
                                       fluidRow(
                                         column(width = 6, selectInput("select", inputId = "Genders_ppl", label = h3("Gender"),
                                                                       choices = list("All" = 1, "Male" = 2, "Female" = 3),
                                         )),
                                         column(width = 6, selectInput("select", inputId = "Ages_ppl", label = h3("Ages"),
                                                                       choices = list("ALL" = 1, "Under 18" = 2, "18 - 29" = 3,
                                                                                      "30 - 44" = 4, "45 And Above" = 5),
                                         ))),
                                       fluidRow(plotlyOutput("Gender_Ages_PPl"))
                              )),
                       tabBox(title = strong("Top Movie Genres Combinations"), id = "Tab_Genre_Cbn",
                              tabPanel("By Frequency", dataTableOutput("movie_combination")),
                              tabPanel("By Average WorldeGross", dataTableOutput("movie_combination_gross"))
                               )),
              fluidRow(tabBox(title = strong("Genre Rank Over Years"), id = "Tab_Genre",
                              tabPanel("Ranking By Average Worldwide Gross", rank_genre_gross),
                              tabPanel("Rank By Average Budget", rank_genre_budget),
                              tabPanel("Rank By Average Rating", rank_genre_rating),
                              tabPanel("Average Worldwide Gross Over Years", graph_genre_worldgross),
                              tabPanel("Average Budget Over Years", graph_genre_budget),
                              tabPanel("Average Rating Over Years", graph_genre1_rating_all_ages),
                              tabPanel("Top Performers For Each Genres", 
                                       fluidRow(
                                         column(3, selectInput("genre_select",inputId = "genre_select",label = h3("Choose a genre:"),
                                                                 choices = genre_by_cast$Genre)),
                                         column(3, selectInput("cast_select", inputId = "cast_select", label = h3("Choose a type of cast"),
                                                               choices = list("Director", "Star", "Writer")))),
                                       fluidRow( plotOutput("wordCloud_Genre"))
                                       ),
                              width = "100%"
                       ))),
      #Cast Tab
      tabItem(tabName = "Cast",
              fluidRow(tabBox(title = strong("Movie Numbers Ranking"), id = "Movie_Numbers_Ranking",
                              tabPanel("Star", numOfMovies_star),
                              tabPanel("Director", numOfMovies_director),
                              tabPanel("Writer", numOfMovies_writer)
                              ),
                       tabBox(title = strong("IMDB Rating Ranking"), id = "Rating_Ranking",
                              tabPanel("Star", score_ranking_star),
                              tabPanel("Director", score_ranking_director),
                              tabPanel("Writer", score_ranking_writer)
                              )
                       )),
      #Rating Tab
      tabItem(tabName = "Analysis",
              fluidRow(tabBox(title = strong("Correlation Analysis - Rating"), id = "corr_rating",
                              tabPanel("Movie Budget", corr_rating_budget),
                              tabPanel("Number of Cast", corr_rating_castNum),
                              tabPanel("Movie Runtime", corr_rating_runtime),
                              tabPanel("Number of People Voted", corr_rating_ppl),
                              tabPanel("Total Filming Locations", corr_rating_locaion)
                              ),
                       tabBox(title = strong("Correlation Analysis - Worldwide Gross"), id = "corr_worldwide_gross",
                              tabPanel("Movie Budget", corr_gross_budget),
                              tabPanel("Number of Cast", corr_gross_castNum),
                              tabPanel("Movie Runtime", corr_gross_runtime),
                              tabPanel("Number of People Voted", corr_gross_ppl),
                              tabPanel("Total Filming Locations", corr_gross_location)
                       )
                       ),
              fluidRow(box(strong("Rating Boxplot Over Years"), rating_over_years, width = "100%"))
              ),
      #Detail tab
      tabItem(tabName = "movie_detail",
              useShinyalert(),
              uiOutput("movie_name"),
              fluidRow(
                 valueBoxOutput("md_rating"),
                 valueBoxOutput("md_worldwide_gross"),
                 valueBoxOutput("md_budget"),
                 
              ),
              fluidRow(
                valueBoxOutput("md_award"),
                valueBoxOutput("md_nomination"),
                valueBoxOutput("md_castNum")
              ),
              fluidRow(
                plotlyOutput("prod_house")
              )
            )
    )
  )
)

server = function(input, output){
    
    #Overview
    output$worldMap = renderPlotly({world})
    output$Tab_Worldwide_GrossSelected = renderText({
      input$Tab_WorldWide_Gross
    })
    
    filt_df_wg = reactive(data[order(data$Cumulative.Worldwide.Gross, decreasing =T),][1: input$rank_wg, c("Name","Cumulative.Worldwide.Gross")])
    output$tableWorldwide = renderDataTable({
      datatable(filt_df_wg())
    }) 
    
    filt_df_budget = reactive(data[order(data$Budget, decreasing =T),][1: input$rank_budget, c("Name","Budget")])
    output$tableBudget = renderDataTable({
      datatable(filt_df_budget())
    }) 
    
    #Popularity
    output$movie_combination = renderDataTable(datatable(genre_cbn))
    output$movie_combination_gross = renderDataTable(datatable(genre_cbn_gross))
    output$Tab_GenreSelected = renderText({
      input$Tab_Genre
    })
    output$graph_genre_worldgross = renderPlotly({graph_genre_worldgross})
    
    output$Gender_Ages_Rating = renderDataTable({
      if (input$Genders_rating == 1 & input$Ages_rating == 1){
        datatable(rating_all_all)
      }
      else if (input$Genders_rating == 1 & input$Ages_rating == 2){
        datatable(rating_all_lower_18)
      }
      else if (input$Genders_rating == 1 & input$Ages_rating == 3){
        datatable(rating_all_18_29)
      }
      else if (input$Genders_rating == 1 & input$Ages_rating == 4){
        datatable(rating_all_30_44)
      }
      else if (input$Genders_rating == 1 & input$Ages_rating == 5){
        datatable(rating_all_up_45)
      }
      else if (input$Genders_rating == 2 & input$Ages_rating == 1){
        datatable(rating_males)
      }
      else if (input$Genders_rating == 2 & input$Ages_rating == 2){
        datatable(rating_males_lower_18)
      }
      else if (input$Genders_rating == 2 & input$Ages_rating == 3){
        datatable(rating_males_18_29)
      }
      else if (input$Genders_rating == 2 & input$Ages_rating == 4){
        datatable(rating_males_30_44)
      }
      else if (input$Genders_rating == 2 & input$Ages_rating == 5){
        datatable(rating_males_up_45)
      }
      else if (input$Genders_rating == 3 & input$Ages_rating == 1){
        datatable(rating_females)
      }
      else if (input$Genders_rating == 3 & input$Ages_rating == 2){
        datatable(rating_females_lower_18)
      }
      else if (input$Genders_rating == 3 & input$Ages_rating == 3){
        datatable(rating_females_18_29)
      }
      else if (input$Genders_rating == 3 & input$Ages_rating == 4){
        datatable(rating_females_30_44)
      }
      else if (input$Genders_rating == 3 & input$Ages_rating == 5){
        datatable(rating_females_up_45)
      }
    })
    
    output$Gender_Ages_PPl = renderPlotly({
      if (input$Genders_ppl == 1 & input$Ages_ppl == 1){
        return(rating_all_ages_ppl)
      }
      else if (input$Genders_ppl == 1 & input$Ages_ppl == 2){
        rating_all_lower_18_ppl
      }
      else if (input$Genders_ppl == 1 & input$Ages_ppl == 3){
        rating_all_18_29_ppl
      }
      else if (input$Genders_ppl == 1 & input$Ages_ppl == 4){
        rating_all_30_44_ppl
      }
      else if (input$Genders_ppl == 1 & input$Ages_ppl == 5){
        rating_all_up_45_ppl
      }
      else if (input$Genders_ppl == 2 & input$Ages_ppl == 1){
        rating_males_ppl
      }
      else if (input$Genders_ppl == 2 & input$Ages_ppl == 2){
        rating_males_lower_18_ppl
      }
      else if (input$Genders_ppl == 2 & input$Ages_ppl == 3){
        rating_males_18_29_ppl
      }
      else if (input$Genders_ppl == 2 & input$Ages_ppl == 4){
        rating_males_30_44_ppl
      }
      else if (input$Genders_ppl == 2 & input$Ages_ppl == 5){
        rating_males_up_45_ppl
      }
      else if (input$Genders_ppl == 3 & input$Ages_ppl == 1){
        rating_females_ppl
      }
      else if (input$Genders_ppl == 3 & input$Ages_ppl == 2){
        rating_females_lower_18_ppl
      }
      else if (input$Genders_ppl == 3 & input$Ages_ppl == 3){
        rating_females_18_29_ppl
      }
      else if (input$Genders_ppl == 3 & input$Ages_ppl == 4){
        rating_females_30_44_ppl
      }
      else if (input$Genders_ppl == 3 & input$Ages_ppl == 5){
        rating_females_up_45_ppl
      }
    })

    wordCloud = reactive(subset(genre_by_cast, Genre==input$genre_select))
    
    set.seed(42)
    output$wordCloud_Genre = renderPlot({
      if (input$cast_select == "Director"){
      temp = wordCloud()[,c("Director","Worldwide_Gross")]
      temp = temp[order(temp$Worldwide_Gross,decreasing = T),]
      ggplot(temp,
             aes(label = Director,
                 size = Worldwide_Gross,
                 color = factor(sample.int(10,nrow(temp),replace = T))
                 )
             ) +
        geom_text_wordcloud_area(eccentricity = 1, grid_size = 1) +
        scale_color_brewer(palette = "Paired", direction = -1)+
        scale_size_area(max_size = 15)+
        theme_minimal()
      }
      else if (input$cast_select == "Writer"){
        temp = wordCloud()[,c("Writer","Worldwide_Gross")]
        temp = temp[order(temp$Worldwide_Gross,decreasing = T),]
        ggplot(temp,
               aes(label = Writer,
                   size = Worldwide_Gross,
                   color = factor(sample.int(10,nrow(temp),replace = T))
               )
        ) +
          geom_text_wordcloud_area(eccentricity = 1, grid_size = 1) +
          scale_color_brewer(palette = "Paired", direction = -1)+
          scale_size_area(max_size = 15)+
          theme_minimal()
      }
      else if (input$cast_select == "Star"){
        temp = subset(star, Genre==input$genre_select)[,c("Star","Worldwide_Gross")]
        temp = temp[order(temp$Worldwide_Gross,decreasing = T),]
        ggplot(temp,
               aes(label = Star,
                   size = Worldwide_Gross,
                   color = factor(sample.int(10,nrow(temp),replace = T))
               )
        ) +
          geom_text_wordcloud_area(eccentricity = 1, grid_size = 1) +
          scale_color_brewer(palette = "Paired", direction = -1)+
          scale_size_area(max_size = 15)+
          theme_minimal()
      }
    })
    
    observeEvent(input$searchButton, {
      if (input$searchMovie %in% data$Name){
        output$movie_name = renderUI(h2(input$searchMovie))
        
        rowNum = which(grepl(input$searchMovie, data$Name))
        
        output$md_rating = renderValueBox({
          valueBox(data[rowNum, "rating_all_ages"], "IMDB Rating", icon = icon("star"),color = "yellow")
        })
        output$md_worldwide_gross = renderValueBox({
          valueBox(format(data[rowNum, "Cumulative.Worldwide.Gross"],scientific = F, big.mark=","), "Worldwide Gross", icon = icon("dollar-sign"),color = "blue")
        })
        output$md_budget = renderValueBox({
          valueBox(format(data[rowNum, "Budget"],scientific = F, big.mark=","), "Budget", icon = icon("coins"),color = "green")
        })
        output$md_castNum = renderValueBox({
          valueBox(format(data[rowNum, "Cast_num"],scientific = F, big.mark=","), "Number Of Cast", icon = icon("people-carry"),color = "red")
        })
        output$md_award = renderValueBox({
          valueBox(format(data[rowNum, "wins"],scientific = F, big.mark=","), "Award Wins", icon = icon("award"),color = "orange")
        })
        output$md_nomination = renderValueBox({
          valueBox(format(data[rowNum, "nominations"],scientific = F, big.mark=","), "Award Nominations", icon = icon("flag"),color = "olive")
        })
        
        output$prod_house = renderPlotly({
          
        })
        
      }
      else{
        shinyalert("Oops!", "The Movie You Searched Not Existed !", type = "error")
      }

    })
}
#subset(genre_by_cast, Genre=="Comedy")[order(-subset(genre_by_cast, Genre=="Comedy")$Rating),]

shinyApp(ui = ui, server = server)
