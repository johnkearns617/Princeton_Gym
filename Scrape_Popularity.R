# PopularTimes
# John Kearns
# 2023-02-10

# install packages
remotes::install_github("JosiahParry/populartimes")
library(populartimes)
library(tidyverse)

# set folder
folder = paste0("")

# set key
Sys.setenv("GOOGLE_KEY" = "AIzaSyDkgsVgd1ev7BaPNgxt0igVB63UgLVY5Go")

manch_bars1 <- get_popular_times("ChIJPcXb6sbmw4kR5PJFmmFUetM")


# save data in df
# gym_popular = data.frame(time=as.character(Sys.time()),hour=lubridate::hour(Sys.time()),current_popularity=manch_bars1$current_popularity,
#                          expected_popularity = (manch_bars1$popular_times[[1]] %>% 
#                            filter(day_of_week==lubridate::wday(Sys.time())&hour==lubridate::hour(Sys.time())))$popularity[1])

gym_popular = read.csv(paste0(folder,"gym_popular.csv"))[,-1]

gym_popular = rbind(gym_popular,data.frame(time=as.character(Sys.time()),hour=lubridate::hour(Sys.time()),current_popularity=manch_bars1$current_popularity,
                         expected_popularity = (manch_bars1$popular_times[[1]] %>%
                           filter(day_of_week==lubridate::wday(Sys.time())&hour==lubridate::hour(Sys.time())))$popularity[1]))

write.csv(gym_popular,paste0(folder,"gym_popular.csv"))

plot1 = ggplot() +
  geom_bar(data=tail(gym_popular,1),aes(x=hour,y=current_popularity,fill="Live Popularity"),stat='identity') +
  geom_bar(data=manch_bars1$popular_times[[1]] %>% filter(day_of_week==lubridate::wday(Sys.time())),aes(x=hour,y=popularity,fill="Expected Popularity"),stat='identity',alpha=.5,width=.7) +
  labs(x='Hour',y='Popularity Level',caption=paste0(Sys.Date(),"\n\n100 denotes the gym is as busy as it gets during the week, or worse.")) +
  ggthemes::theme_fivethirtyeight() +
  ggthemes::scale_fill_fivethirtyeight() +
  theme(plot.caption = element_text(hjust = 0),axis.title = element_text(), axis.title.x = element_text(),legend.position="bottom") +
  lims(y=c(0,100)) +
  xlab("Hour")

current_popular = tail(gym_popular,1)
do_i_go = case_when(
  current_popular$current_popularity>=current_popular$expected_popularity&current_popular$expected_popularity>=50~"DO NOT GO\nMore crowded than usual, and that was already bad", # eventually change 50 when I have the data
  current_popular$current_popularity>=60~"DO NOT GO\nIt's a zoo",
  current_popular$current_popularity>=current_popular$expected_popularity&current_popular$expected_popularity<50&current_popular$current_popularity>=50~"DO NOT GO\nIt usually isn't crowded, but there are kids in there",
  current_popular$current_popularity<current_popular$expected_popularity&current_popular$expected_popularity>=50&current_popular$current_popularity>=50~"IT'S A MAYBE\nIt's usually crowded at this time, you can risk it",
  current_popular$current_popularity<current_popular$expected_popularity&current_popular$expected_popularity>=50&current_popular$current_popularity<50~"GO\nIt's much emptier than usual",
  current_popular$current_popularity<50~"GO\nIt's empty"
  )

color = case_when(
  grepl("DO NOT",do_i_go)~"red",
  grepl("MAYBE",do_i_go)~"yellow",
  TRUE~"green"
)

save.image(paste0(folder,"data.RData"))


  

