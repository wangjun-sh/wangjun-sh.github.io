

library(tidyverse)
library(haven)
library(htmlTable)
library(lazyeval)


source("./anainit.r")
source("./getmean.r")
source("./getcat2.r")


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


anainit()

ddmean <- getMean(adsl, 'SAFFL=="Y"',tmtvar=list("TRT01PN", "TRT01P"), dptvar="AGE", dptvarlabel="Age (Years)")
ddcat_sex <- getCat(adsl, 'SAFFL=="Y"',tmtvar=list("TRT01PN", "TRT01P"),
                    dptvar="SEX", dptvarlabel="Sex",
                    dptvarvalueorder=list("M"=1,"F"=2),
                    dptvarvaluelabel=list("M"="Male","F"="Female"))

race <- adsl %>% 
  filter(SAFFL=="Y") %>%
  select(RACE,RACEN) %>%
  distinct()

# https://www.dummies.com/programming/r/how-to-name-the-values-in-your-vectors-in-r/
racename <- race$RACEN
names(racename)  <- race$RACE


ddcat_race <- getCat(adsl, 'SAFFL=="Y"',tmtvar=list("TRT01PN", "TRT01P"),
                    dptvar="RACE", dptvarlabel="Race",
                    dptvarvalueorder=as.list(racename))


tlfdb <-bind_rows(ddmean, ddcat_sex, ddcat_race) %>%
  arrange(varorder, TRT01PN, TRT01P) %>%
  select(-TRT01P,-valn) %>%
  spread(TRT01PN, valc, sep="_",fill=0) %>%   # missing value filled with 0
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
          caption="Table 14.1 Demographics, Safety Population",
          tfoot="&dagger; A table footer commment")




