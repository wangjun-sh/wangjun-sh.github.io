

library(tidyverse)
library(haven)
library(htmlTable)
library(lazyeval)


adsl <- read_xpt("./AdaM/adsl.xpt")
adsl_total <- filter(adsl,SAFFL=="Y") %>%
  mutate(TRT01PN=99,TRT01P="Total")

adsl <- bind_rows(adsl, adsl_total)

# data wrangling
# https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf

## table header with big N
tableheader <- filter(adsl, SAFFL=="Y") %>%
  select(TRT01PN, TRT01P) %>%
  group_by(TRT01PN, TRT01P) %>%
  count() %>%
  # mutate(trtlabel=paste0(TRT01P,"\n (N=", n,")"))  ##\n not working in html table
  mutate(trtlabel=paste0(TRT01P,"<br/> (N=", n,")"))


## mean
adsl_AGE <- filter(adsl,SAFFL=="Y") %>%
  select(TRT01PN, TRT01P, AGE) %>%
  arrange(TRT01PN, TRT01P) %>%
  group_by(TRT01PN, TRT01P) %>%
  summarise(n=n(),mean=mean(AGE),median=median(AGE),std=sd(AGE),min=min(AGE),max=max(AGE)) %>%
  gather(key="statc",value="valn",n,mean,median,std,min,max) %>%
  mutate(varorder=1, varlabel="Age (Years)") %>%
  mutate(statlabel = factor(statc,levels=c("n","mean","std","median","min","max"),labels=c("n","Mean","STD","Median","Min","Max"))) %>%
  arrange(TRT01PN, TRT01P, statlabel) %>%
  mutate(valc=if_else(statlabel=="Mean",sprintf("%5.2 f", valn),
                     if_else(statlabel=="STD",sprintf("%6.3 f", valn), sprintf("%3. f",valn))))

# https://thomasleeper.com/Rcourse/Tutorials/numeric-printing.html
# https://www.r-bloggers.com/number-formatting/
# https://www.rdocumentation.org/packages/base/versions/3.5.1/topics/format
# format(c(6.0, 13.1), nsmall = 2)
# sprintf("%5.1f", pi)

# adsl_AGE$statlabel <- factor(adsl_AGE$statc,levels=c("n","mean","std","median","min","max"),labels=c("n","Mean","STD","Median","Min","Max"))
# alternative to SAS format
# https://dplyr.tidyverse.org/reference/recode.html
adsl_AGE$statn <- recode(adsl_AGE$statc,"n"=1,"mean"=2,"std"=3,"median"=4,"min"=5,"max"=6)


## cat
adsl_SEX <- filter(adsl,SAFFL=="Y") %>%
  select(TRT01PN, TRT01P, SEX) %>%
  arrange(TRT01PN, TRT01P) %>%
  group_by(TRT01PN, TRT01P) %>%
  count(SEX) %>%
  mutate(statc=SEX, valn=n, varorder=2, varlabel="Sex") %>%
  select(-SEX,-n) %>%
  mutate(statlabel = factor(statc,levels=c("M","F"),labels=c("Male","Female"))) %>%
  arrange(TRT01PN, TRT01P, statlabel) %>%
  mutate(valc=sprintf("%4. f", valn))

# adsl_SEX$statlabel <- factor(adsl_SEX$statc,levels=c("M","F"),labels=c("Male","Female"))

# alternative to SAS format
# https://dplyr.tidyverse.org/reference/recode.html
adsl_SEX$statn <- recode(adsl_SEX$statc,"M"=1,"F"=2)


source("./getmean.r")
source("./getcat2.r")

ddmean <- getMean(adsl, 'SAFFL=="Y"',tmtvar=list("TRT01PN", "TRT01P"), dptvar="AGE", dptvarlabel="Age (Years)")
ddcat <- getCat(adsl, 'SAFFL=="Y"',tmtvar=list("TRT01PN", "TRT01P"),
                dptvar="SEX", dptvarlabel="Sex",
                dptvarvalueorder=list("M"=1,"F"=2),
                dptvarvaluelabel=list("M"="Male","F"="Female"))


# tlfdb <-bind_rows(adsl_AGE, adsl_SEX) %>%
tlfdb <-bind_rows(ddmean, ddcat) %>%
  arrange(varorder, TRT01PN, TRT01P) %>%
  select(-TRT01P,-valn) %>%
  spread(TRT01PN, valc, sep="_") %>%
  arrange(varorder,varlabel,statn) %>%
  group_by(varorder,varlabel) 


varrows <- group_by(tlfdb,varorder, varlabel) %>%
  count(varlabel)


tlfoutput <- ungroup(tlfdb)%>%
  select(-varorder,-varlabel,-statn,-statc) %>%
  mutate(statlabel=paste0("&nbsp;&nbsp;&nbsp;&nbsp;",statlabel))


# refer to https://cran.r-project.org/web/packages/htmlTable/vignettes/tables.html
# alternative https://haozhu233.github.io/kableExtra/awesome_table_in_html.html
htmlTable(tlfoutput,
          header =  c("Statistics",tableheader$trtlabel),
          rnames=FALSE,
          align="lccc",
          align.header="lccc",
          rgroup = varrows$varlabel,
          n.rgroup = varrows$n,
          padding.rgroup = "&nbsp;&nbsp;&nbsp;&nbsp;",
          col.rgroup = c("none", "#EFEFF0"),
          caption="Basic table with both column spanners (groups) and row groups",
          tfoot="&dagger; A table footer commment")




