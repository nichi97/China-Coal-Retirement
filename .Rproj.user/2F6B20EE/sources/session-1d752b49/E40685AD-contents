library(tidyverse)
library(plotly)

retired_units <- read_csv("./processed_data/retired_china_units.csv")

#' # Basic Descriptive Questions 

#' ## When did most retirement happens? 

#' ### Pure counts
time_hist <- 
  ggplot(retired_units, aes(RETIRED)) +
    geom_histogram(binwidth = 1)

ggplotly(time_hist)

retire_yr_cnt <- 
retired_units %>% group_by(RETIRED) %>% summarize(retire_cnt = n()) %>% 
  arrange(desc(retire_cnt)) %>% top_n(10)
retire_yr_cnt

#' If we are looking at pure number of plants that are retired, the top five 
#' years are 2007, 2008, 2009, 2018, and 2020. 
#' I am wondering what happened from 2007 to 2009? 

#' ### Generation Capacity 
cap_time_hist <- 
  ggplot(retired_units, aes(x = RETIRED, weight = `Capacity (MW)`)) +
    geom_histogram(binwidth = 1)

ggplotly(cap_time_hist)

retire_yr_cap <- 
retired_units %>% group_by(RETIRED) %>% 
  summarize(retire_cap_mw = sum(`Capacity (MW)`)) 

retire_yr_cap %>% 
  arrange(desc(retire_cap_mw)) %>% top_n(10)

#' This is so interesting. So if we rank by retirement capacity, then 
#' the top five are 2020, 2018, 2009, 2007, 2010. I suppose if we organize 
#' retirement into "waves", we can see the following phases:
#' Phase 1: ~ 2006: Barely any retirement is observed. 
#' Phase 2: 2007 ~ 2010: a lot of smaller plants are retired 
#' Phase 3: 2011 ~ 2013: Some retirements are happening, but there are 
#'          relatively few coal-fired power plants retirement going on
#' Phase 4: 2014 ~ 2020: Ups and Downs, but reaching a definitive peak at 2020
#' Phase 5: 2021 ~ : Dropped to pre-2007 level. Is this a data issue or is there
#'          actually some kind of pattern going on here? 

#' ### Shares of retired generation capacity 

#' This is slightly more ambitious. I will calculate the percentage of retired
#' capacity with respect to the total capacity at that year 

cumu_cap <- read_csv("./processed_data/cumulative_cap.csv")
retire_cumu_df <- left_join(retire_yr_cap, cumu_cap, by = c("RETIRED" = "Year"))

retire_cumu_df <- 
  retire_cumu_df %>% 
  mutate(retire_perc = retire_cap_mw / cumu_cap_mw * 100)

retire_share_plot <- 
  ggplot(retire_cumu_df, aes(x = RETIRED, y=retire_perc)) + 
    geom_point() + 
    geom_path()

ggplotly(retire_share_plot)

#' This is again, very exciting! 
#' So it seems that phase 2, 2007 ~ 2010, has the highest coal plant 
#' retirement rate. Another very intriguing drop happened in 2021, where 
#' a very significant drop in retirement rate can be observed compared to 2020.





