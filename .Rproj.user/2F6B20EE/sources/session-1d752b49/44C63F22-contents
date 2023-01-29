#' 1_data_wrangle.R
#' This file does a very simple job -- clean the data. In this specific case, 
#' the data set is very easy to clean. All I did was to filter so that I have 
#' a data set that is left with only retired plants in China 

library(sets)
library(hash)
library(tidyverse)

coal_units <- read_csv("./raw_data/coal_units.csv")

# filter out units whose start year and retire year are both unknown
# can't work with these for this project.
china_units <- coal_units %>% 
  filter(Country == "China") %>% 
  filter(!is.na(Year) | !is.na(RETIRED))


#' Map years that looks like "1980's" to "1985"
#' This choice is a bit arbitrary, but it is the best I can do. 
#' Note that I could've just removed these data points too -- there are a 
#' total of 30 of them. But I think it is better to preserve them. Since 
#' there are only 30 of them, this really shouldn't matter that much. 

# map fuzzy years like "1980's"to "1985"
china_units <- china_units %>% 
  mutate(Year = if_else(str_detect(Year, "'s"),
                        str_c(str_sub(Year, 1, 3), "5"),
                        Year)) %>% 
  mutate(Year = as.numeric(Year))

#' ## Retirement units in China. 
#' Output: retired_china_units.csv
retired_china_units <-   
china_units %>% 
  filter(!is.na(RETIRED))

write_csv(retired_china_units, "./processed_data/retired_china_units.csv")



#' ## Cumulative coal capacity in China by year
#' Output: cumulative_cap.csv: cumulative coal gen cap over years

# convert year-capacity into named vector, acting like dict essentially 
new_cap <- china_units %>% 
  group_by(Year) %>% 
  summarize(new_cap = sum(`Capacity (MW)`))
new_cap_yr <- as.set(new_cap$Year)
new_cap_vct <- new_cap$new_cap
names(new_cap_vct) <- new_cap$Year

retire_cap <- china_units %>% 
  group_by(RETIRED) %>% 
  summarize(retired_cap = sum(`Capacity (MW)`))
retire_cap_yr <- as.set(retire_cap$RETIRED)
retire_cap_vct <- retire_cap$retired_cap
names(retire_cap_vct) <- retire_cap$RETIRED

# construct needed inputs & outputs vessel for looping 
yr_min <- min(new_cap$Year, retire_cap$RETIRED, na.rm=T)
# for some reason, `Year` can be larger than current year. 
# the `min()` is to ensure that we don't go into the future
yr_max <- min(max(new_cap$Year, retire_cap$RETIRED, na.rm=T),
              as.numeric(format(Sys.Date(), "%Y"))) 
yr_seq <- yr_min:yr_max
cumu_cap_vct <- vector(mode="numeric", length = length(yr_seq))
names(cumu_cap_vct) <- as.character(yr_seq)

# Cap_i = Cap_{i-1} + New_i - Retire_i
for (yr in yr_seq){
  chr_yr <- as.character(yr)
  # Cap_{i-1}
  prev_cap <- ifelse(yr == yr_min, 0, cumu_cap_vct[as.character(yr-1)])
  # if not in the new/retire yr set, that year don't have increment /decrements 
  # New_i
  new_cap_curr <- ifelse(yr %in% new_cap_yr,  new_cap_vct[chr_yr], 0)
  # Retire_i
  retire_cap_curr <-  ifelse(yr %in% retire_cap_yr,  retire_cap_vct[chr_yr], 0)
  cumu_cap_vct[chr_yr] <- prev_cap + new_cap_curr - retire_cap_curr
}

cumu_cap_df <- tibble(
  Year = names(cumu_cap_vct) %>% as.numeric(),
  cumu_cap_mw = cumu_cap_vct
)
write_csv(cumu_cap_df, "./processed_data/cumulative_cap.csv")

#' ## Exploratory analysis: what are plants whose `Year` is `NA`? 
#' No output produced 

na_yr_units <- 
  china_units %>% filter(is.na(Year))

na_yr_units %>% pull(Status) %>% unique()

operating_na_yr <- 
  na_yr_units %>% filter(Status == "operating")

#' There is a total of 91 plants that are operational and yet don't have a 
#' operation start year. 91 plants is not a whole lot I have to say... 
#' I just have to work with what I have. 
