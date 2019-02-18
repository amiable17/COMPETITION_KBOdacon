## 0. package
pck_list <- c('tidyverse')
sapply(pck_list, require, character.only = T)

## 1. Load the data
pre_season_batter <- read_csv('data/pre_season_batter.csv')
regular_season_batter <- read_csv('data/Regular_Season_Batter.csv')
regular_season_batter_dbd <- read_csv('data/Regular_Season_Batter_Day_by_Day.csv')
submission <- read_csv('data/submission.csv')

str(pre_season_batter)

## 2. 데이터 변수 작은 단위로 쪼개서 펼치기
pre_season_batter$avg

pre_season_batter <- pre_season_batter %>%
  mutate(
    # height/weight: 키(height), 체중(weight), 체중/키 비율(hw_ratio : 같은 kg도 키에 따라 다른 체격으로 볼 수 있음)
    height = as.numeric(gsub('cm','',unlist(strsplit(`height/weight`, '/'))[1])),
    weight = as.numeric(gsub('kg','',unlist(strsplit(`height/weight`, '/'))[2])),
    hw_ratio = weight / height,
    
    # born_year, born_month, born_day
    born_year = as.numeric(substr(year_born,1,4)),
    born_month = as.numeric(substr(year_born,7,8)),
    born_day = as.numeric(substr(year_born,11,12))
    
    # position
  )

pre_season_batter %>% 
  transmute(year_born,
            )

# start_year = 데뷔년도(데이터에 처음 등장한 year), n_year = 해당 연도의 연차(연도 기준)
pre_season_batter <- pre_season_batter %>%
  group_by(batter_id) %>% 
  mutate(start_year = first(year),
         n_year = year - start_year + 1)
