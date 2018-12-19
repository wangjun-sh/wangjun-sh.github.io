


library(lazyeval)


getCat <- function(df,whr,tmtvar,dptvar,dptvarlabel){
  
  # https://github.com/r-lib/rlang/issues/116
  df_cat <- filter(df,eval(parse(text=whr))) %>%
    select_(.dots=append(tmtvar, dptvar)) %>%
    arrange_(.dots=tmtvar) %>%
    group_by_(.dots=tmtvar) %>%
    count_(dptvar) %>%
    mutate(statc=!!sym(dptvar), valn=n, varorder=2, varlabel=dptvarlabel) %>%
    select(-!!dptvar,-n) %>%
    mutate(statlabel = factor(statc,levels=c("M","F"),labels=c("Male","Female"))) %>%
    arrange_(.dots=append(tmtvar, "statlabel")) %>%
    mutate(valc=sprintf("%4. f", valn))

  
  df_cat$statn <- recode(df_cat$statc,"M"=1,"F"=2)
  
  return(df_cat)

}

ddcat <- getCat(adsl, 'SAFFL=="Y"',tmtvar=list("TRT01PN", "TRT01P"), dptvar="SEX", dptvarlabel="Sex")

