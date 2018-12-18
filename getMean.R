


# library(lazyeval)


getMean <- function(df,whr,tmtvar,dptvar,dptvarlabel){
  
  if (!exists("varorder_",envir = .GlobalEnv)) {
    .GlobalEnv$varorder_ <<- 0
  }
  
  varorder_ <<-  varorder_ + 1
  
  # https://www.r-bloggers.com/data-frame-columns-as-arguments-to-dplyr-functions/
  df_mean <- filter(df,eval(parse(text=whr))) %>%
    select_(.dots=append(tmtvar, dptvar)) %>%
    arrange_(.dots=tmtvar) %>%
    group_by_(.dots=tmtvar) %>%
    summarise_(n=~n(),
              mean=interp(~mean(x),x=as.name(dptvar)),
              median=interp(~median(x),x=as.name(dptvar)),
              std=interp(~sd(x),x=as.name(dptvar)),
              min=interp(~min(x),x=as.name(dptvar)),
              max=interp(~max(x),x=as.name(dptvar))) %>%
    gather(key="statc",value="valn",n,mean,median,std,min,max) %>%
    mutate(varorder=varorder_, varlabel=dptvarlabel) %>%
    mutate(statlabel = factor(statc,levels=c("n","mean","std","median","min","max"),labels=c("n","Mean","STD","Median","Min","Max"))) %>%
    arrange_(.dots=append(tmtvar, "statlabel")) %>%
    mutate(valc=if_else(statlabel=="Mean",sprintf("%5.2 f", valn),
                        if_else(statlabel=="STD",sprintf("%6.3 f", valn), 
                                sprintf("%3. f",valn))))

  df_mean$statn <- recode(df_mean$statc,"n"=1,"mean"=2,"std"=3,"median"=4,"min"=5,"max"=6)
  
  return(df_mean)
}


# ddmean <- getMean(adsl, 'SAFFL=="Y"',tmtvar=list("TRT01PN", "TRT01P"), dptvar="AGE", dptvarlabel="Age (Years)")


