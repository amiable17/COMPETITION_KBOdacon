## 0. package
pck_list <- c('tidyverse','mlr')
# install.packages(pck_list)

sapply(pck_list, require, character.only = T)

## 1. Load the data
pre_season_batter <- read_csv('data/pre_season_batter.csv')
regular_season_batter <- read_csv('data/Regular_Season_Batter.csv')
regular_season_batter_dbd <- read_csv('data/Regular_Season_Batter_Day_by_Day.csv')
submission <- read_csv('data/submission.csv')

## 2. 데이터 변수 작은 단위로 쪼개서 펼치기
## 2-1. pre_season_batter
str(pre_season_batter)
for(i in 1:length(names(pre_season_batter))){
  if(i %in% c(10, 11, 24)) next
  
  var <- names(pre_season_batter)[i]
  var <- eval(parse(text = paste0('pre_season_batter$',var)))
  if(mode(var) == 'character'){
    cat('\n\n\n variable : ', names(pre_season_batter)[i], '\n')
    print(table(pre_season_batter[,i], useNA = "ifany"))
    unq_val <- unlist(unique(pre_season_batter[,i]))
    cat('\n (length:', length(unq_val), ')')
  }
}

pre_season_batter <- pre_season_batter %>%
  group_by(batter_id) %>% 
  mutate(
    # avg 숫자형으로 변경
    avg = as.numeric(ifelse(avg == '-', '', avg)),
    
    # year_born: born_year, born_month, born_day
    born_year = as.numeric(substr(year_born,1,4)),
    born_month = as.numeric(substr(year_born,7,8)),
    born_day = as.numeric(substr(year_born,11,12)),
    
    # position: position_1, position_2(투), postion(타)
    position_1 = unlist(strsplit(position,'\\('))[1],
    position_2 = substr(unlist(strsplit(position,'\\('))[2],1,2),
    position_3 = substr(unlist(strsplit(position,'\\('))[2],3,4),
    
    # starting_salary: 단위 없애기
    starting_salary = gsub('만원','',starting_salary),
    starting_salary = gsub('달러','000',starting_salary),
    starting_salary = as.numeric(starting_salary),
    
    # height/weight: 키(height), 체중(weight), 체중/키 비율(hw_ratio : 같은 kg도 키에 따라 다른 체격으로 볼 수 있음)
    height = as.numeric(gsub('cm','',unlist(strsplit(`height/weight`, '/'))[1])),
    weight = as.numeric(gsub('kg','',unlist(strsplit(`height/weight`, '/'))[2])),
    hw_ratio = weight / height,
    
    # start_year = 데뷔년도(데이터에 처음 등장한 year), n_year = 해당 연도의 연차(연도 기준), age = 해당 연도의 만나이
    start_year = first(year),
    n_year = year - start_year + 1,
    age = year - born_year
  ) %>% 
  select(-one_of('height/weight','year_born','position'))


## 2-2. regular_season_batter
str(regular_season_batter)
for(i in 1:length(names(regular_season_batter))){
  if(i %in% c(10, 11, 24)) next
  
  var <- names(regular_season_batter)[i]
  var <- eval(parse(text = paste0('regular_season_batter$',var)))
  if(mode(var) == 'character'){
    cat('\n\n\n variable : ', names(regular_season_batter)[i], '\n')
    print(table(regular_season_batter[,i], useNA = "ifany"))
    unq_val <- unlist(unique(regular_season_batter[,i]))
    cat('\n (length:', length(unq_val), ')')
  }
}

regular_season_batter <- regular_season_batter %>%
  group_by(batter_id) %>% 
  mutate(
    # avg 숫자형으로 변경
    avg = as.numeric(ifelse(avg == '-', '', avg)),
    
    # year_born: born_year, born_month, born_day
    born_year = as.numeric(substr(year_born,1,4)),
    born_month = as.numeric(substr(year_born,7,8)),
    born_day = as.numeric(substr(year_born,11,12)),
    
    # position: position_1, position_2(투), postion(타)
    position_1 = unlist(strsplit(position,'\\('))[1],
    position_2 = substr(unlist(strsplit(position,'\\('))[2],1,2),
    position_3 = substr(unlist(strsplit(position,'\\('))[2],3,4),
    
    # starting_salary: 단위 없애기
    starting_salary = gsub('만원','',starting_salary),
    starting_salary = gsub('달러','000',starting_salary),
    starting_salary = as.numeric(starting_salary),
    
    # height/weight: 키(height), 체중(weight), 체중/키 비율(hw_ratio : 같은 kg도 키에 따라 다른 체격으로 볼 수 있음)
    height = as.numeric(gsub('cm','',unlist(strsplit(`height/weight`, '/'))[1])),
    weight = as.numeric(gsub('kg','',unlist(strsplit(`height/weight`, '/'))[2])),
    hw_ratio = weight / height,
    
    # start_year = 데뷔년도(데이터에 처음 등장한 year), n_year = 해당 연도의 연차(연도 기준), age = 해당 연도의 만나이
    start_year = first(year),
    n_year = year - start_year + 1,
    age = year - born_year
  ) %>% 
  select(-one_of('height/weight','year_born','position'))


## 2-3. regular_season_batter_dbd
str(regular_season_batter_dbd)
for(i in 1:length(names(regular_season_batter_dbd))){
  if(i %in% c(9,10)) next
  
  var <- names(regular_season_batter_dbd)[i]
  var <- eval(parse(text = paste0('regular_season_batter_dbd$',var)))
  if(mode(var) == 'character'){
    cat('\n\n\n variable : ', names(regular_season_batter_dbd)[i], '\n')
    print(table(regular_season_batter_dbd[,i], useNA = "ifany"))
    unq_val <- unlist(unique(regular_season_batter_dbd[,i]))
    cat('\n (length:', length(unq_val), ')')
  }
}

regular_season_batter_dbd <- regular_season_batter_dbd %>%
  group_by(batter_id) %>% 
  mutate(
    # avg 숫자형으로 변경
    avg1 = as.numeric(ifelse(avg1 == '-', '', avg1)),
    
    # date값을 month와 day로 분리
    date = as.character(sprintf('%2.02f',date)),
    date_month = unlist(strsplit(date, '\\.'))[1],
    date_day = unlist(strsplit(date, '\\.'))[2]
    
  )

write_csv(pre_season_batter, 'data/Pre_Season_Batter_2.csv')
write_csv(regular_season_batter, 'data/Regular_Season_Batter_2.csv')
write_csv(regular_season_batter_dbd, 'data/Regular_Season_Batter_Day_by_Day_2.csv')



# modeling
# model1 : 타격변수들()을 이용하여 PCA + 비타격 변수 > 마트 구성
# R

# 1. 시계열적 요소가 있는지 확인
