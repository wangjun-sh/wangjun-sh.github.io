

library(tidyverse)
library(lubridate)
library(stringi)
library(stringr)


testdat <- c("2017/11/11","2016/UN/UN","2015/UK/UK","UNK/UK/UK")
# remove(testdat)
# validate yyyy/mm/dd
str_detect(testdat, regex("((19|20)[0-9][0-9])/(0?[1-9]|1[012])/(0?[1-9]|[12][0-9]|3[01])"))


dat <- data.frame(testdat) %>%
  mutate(validdate=str_detect(testdat, regex("((19|20)[0-9][0-9])/(0?[1-9]|1[012])/(0?[1-9]|[12][0-9]|3[01])")))  %>%
  separate(testdat,c("yyyy","mm","dd"),sep="/",remove=FALSE)  %>%
  
  mutate(yyyy=if_else(yyyy %in% c("UN","UK","UNK","NA",""), "", yyyy))  %>%
  mutate(mm=if_else(mm %in% c("UN","UK",""), "", mm))  %>%
  mutate(dd=if_else(dd %in% c("UN","UK",""), "", dd))  %>%
  
  mutate(testdtc=stri_join(yyyy,mm,dd,sep="-"))  %>%
  mutate(testdtc=if_else(str_detect(testdtc,regex("--$")),stri_replace_all_regex(testdtc,regex("--$"),""),testdtc))  %>%
  
  mutate(testdat_yyyy=str_split_fixed(testdat,"/",3)[,1]) %>%
  mutate(testdat_mm=str_split_fixed(testdat,"/",3)[,2]) %>%
  mutate(testdat_dd=str_split_fixed(testdat,"/",3)[,3]) %>%
  
  mutate(testdat_yyyy=if_else(testdat_yyyy %in% c("UN","UK","UNK","NA",""), "", testdat_yyyy))  %>%
  mutate(testdat_mm=if_else(testdat_mm %in% c("UN","UK",""), "", testdat_mm))  %>%
  mutate(testdat_dd=if_else(testdat_dd %in% c("UN","UK",""), "", testdat_dd))  %>%
  
  mutate(testdt=if_else(validdate,as.Date(testdtc),NULL))
  

#str_detect("2011--",regex("--$"))
#str_detect("2011-11--",regex("--$"))

#stri_replace_all_regex("2011--",regex("--$"),"")
#stri_replace_all_regex("2011-11--",regex("--$"),"")


