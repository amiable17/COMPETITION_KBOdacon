require(tidyverse)
require(dplyr)

# install.packages('readx1')

Pre_Season_Batter <- read_csv("C:\\Users\\JOO\\Desktop\\DS_JOO\\DACON\\KBO_batter_2019_1\\data\\Pre_Season_Batter.csv", col_types = cols())
Regular_Season_Batter <- read_csv("C:\\Users\\JOO\\Desktop\\DS_JOO\\DACON\\KBO_batter_2019_1\\data\\Regular_Season_Batter.csv", col_types = cols())
Regular_Season_Batter_day <- read_csv("C:\\Users\\JOO\\Desktop\\DS_JOO\\DACON\\KBO_batter_2019_1\\data\\Regular_Season_Batter_Day_by_Day.csv", col_types = cols())

a <- Pre_Season_Batter %>% 
  select(`height/weight`)

Pre_Season_Batter <- Pre_Season_Batter %>% 
  mutate(height = substr(`height/weight`, 1, 3),
         weight = substr(`height/weight`, 7, length(`height/weight`)-2),
         hw_ratio = height / weight)

# 
Pre_Season_Batter <- Pre_Season_Batter %>% 
  mutate(height = as.numeric(substr(`height/weight`, 1, 3)),
         weight = as.numeric(substr(`height/weight`, 7, (nchar(`height/weight`)-2))),
         hw_ratio = height / weight
  )

# start_year = 데뷔년도(데이터에 처음 등장한 year), n_year = 해당 연도의 연차(연도 기준)
Pre_Season_Batter <- Pre_Season_Batter %>%
  group_by(batter_id) %>% 
  mutate(start_year = first(year),
         n_year = year - start_year + 1)

test <- strsplit(Pre_Season_Batter$career, "-")
regex(test,)
test[[2]][-regexpr(pattern = "초$", test)]