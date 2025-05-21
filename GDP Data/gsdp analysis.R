library(tidyverse)
library(janitor)


view(final_statewise_gsdp)

"final_statewise_gsdp.csv" %>% 
  read.csv() %>% 
  rename("sector"= "item") ->statewise_gsdp

statewise_gsdp %>% 
  pull(sector) %>% 
  unique()


#1. for every financial yr, which sector has performed well
statewise_gsdp %>% 
  group_by(year,sector) %>% 
  summarise(total_gsdp=sum(gsdp,na.rm=T)) ->df

  df %>% 
  group_by(year) %>% 
 arrange(desc(total_gsdp)) %>% 
    slice_max(order_by=total_gsdp,n=1)
  
  


#2. for every financial yr, which sector has performed least
  statewise_gsdp %>% 
    group_by(year,sector) %>% 
    summarise(total_gsdp=sum(gsdp,na.rm=T)) ->df
  
  df %>% 
    group_by(year) %>% 
    arrange(total_gsdp) %>% 
    slice(1)
  
  
#3. for every financial yr, which state has performed well
  statewise_gsdp %>% 
    group_by(year,state) %>% 
    summarise(total_gsdp=sum(gsdp,na.rm=T)) ->df
  
  df %>% 
    group_by(year) %>% 
    arrange(desc(total_gsdp)) %>% 
    slice(1)
  
  
#4. for every financial yr, which state has performed least
  statewise_gsdp %>% 
    group_by(year,state) %>% 
    summarise(total_gsdp=sum(gsdp,na.rm=T)) ->df
  
  df %>% 
    group_by(year) %>% 
    arrange(total_gsdp) %>% 
    slice(1)
  
  
#5. top 5 performing state in manfacturing
  
  
  statewise_gsdp %>% 
    filter(sector=="Manufacturing") %>% 
    group_by(state) %>%
    summarise(total_gsdp = sum(gsdp, na.rm = TRUE), .groups = "drop") %>% 
    arrange(desc(total_gsdp)) %>%
    slice(1:5)
  
  
  
  
#6. top 5 performing state in construction
  statewise_gsdp %>% 
    filter(sector=="Construction") %>% 
    group_by(state) %>%
    summarise(total_gsdp = sum(gsdp, na.rm = TRUE), .groups = "drop") %>% 
    arrange(desc(total_gsdp)) %>%
    slice(1:5)
  
  
  
#7. for every financial 2016-2017 , for every state get top performing sector
  statewise_gsdp %>% 
    filter(year=="2016-17") %>% 
    group_by(state,sector) %>% 
    summarise(total_gsdp=sum(gsdp,na.rm=T)) ->df
  
  df %>% 
    group_by(state) %>% 
    arrange(desc(total_gsdp)) %>% 
    slice(1)
  
  
  
#8. for every financial 2016-2017 , for every state get top 5 performing sector
  statewise_gsdp %>% 
    filter(year=="2016-17") %>% 
    group_by(state,sector) %>% 
    summarise(total_gsdp=sum(gsdp,na.rm=T)) ->df
  
  df %>% 
    group_by(state) %>% 
    arrange(desc(total_gsdp)) %>% 
    slice(1:5)
  
#9. how many states are performing well in manufacturing, (if manufacturing is in top 3)
  statewise_gsdp %>% 
     group_by(state,sector) %>%
    summarise(total_gsdp = sum(gsdp, na.rm = TRUE), .groups = "drop") %>% 
    group_by(state) %>%
    arrange(desc(total_gsdp)) %>%
    slice(1:3) %>% 
    filter(sector=="Manufacturing") -> top_manufacturing_states
nrow(top_manufacturing_states)
    
  
  
#10. what is the GROSS GSDP of Karnataka for all financial years
statewise_gsdp %>% 
  filter(state == "Karnataka") %>% 
  group_by(year) %>% 
  summarise(total_gsdp = sum(gsdp,na.rm=T)) 

