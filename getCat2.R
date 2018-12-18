


# library(lazyeval)


getCat <- function(df,whr,tmtvar,dptvar,dptvarlabel,dptvarvaluelist=NULL,dptvarvaluelabel=NULL,dptvarvalueorder=NULL){
  
  if (!exists("varorder_",envir = .GlobalEnv)) {
    .GlobalEnv$varorder_ <<- 0
  }
  
  varorder_ <<-  varorder_ + 1
  
  # https://github.com/r-lib/rlang/issues/116
  df_cat <- filter(df,eval(parse(text=whr))) %>%
    select_(.dots=append(tmtvar, dptvar)) %>%
    arrange_(.dots=tmtvar) %>%
    group_by_(.dots=tmtvar) %>%
    count_(dptvar) %>%
    mutate(statc=!!sym(dptvar), valn=n, varorder=varorder_, varlabel=dptvarlabel) %>%
    select(-!!dptvar,-n) %>%
    # mutate(statlabel = factor(statc,levels=c("M","F"),labels=c("Male","Female"))) %>%
    # arrange_(.dots=append(tmtvar, "statlabel")) %>%
    mutate(valc=sprintf("%4. f", valn))
  
  
  df_cat$statn <- recode(df_cat$statc,!!!dptvarvalueorder)
  
  if (!is.null(dptvarvaluelabel)) {
    df_cat$statlabel <- recode(df_cat$statc,!!!dptvarvaluelabel)  
  } else {
    df_cat$statlabel <- df_cat$statc
  }
  
  
  df_cat <- df_cat %>%
    arrange_(.dots=append(tmtvar,list("statn","statlabel")))
  
  return(df_cat)
  
}

# ddcat <- getCat(adsl, 'SAFFL=="Y"',tmtvar=list("TRT01PN", "TRT01P"), 
#                 dptvar="SEX", dptvarlabel="Sex",
#                 dptvarvalueorder=list("M"=1,"F"=2),
#                 dptvarvaluelabel=list("M"="Male","F"="Female"))

