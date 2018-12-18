

library(knitr)
library(markdown)
library(rmarkdown)
library(tidyverse)
library(haven)

dm <- read_xpt("./SDTM/dm.xpt")
  
usubjid <- dm %>%
  filter(!(tolower(ARM) %in% c("screen failure","not assigned","not treated"))) %>%
  select(USUBJID) %>%
  distinct()

# https://www.reed.edu/data-at-reed/software/R/markdown_multiple_reports.html
for (subject in unique(usubjid$USUBJID)){
  rmarkdown::render('./patient_profile_2_report.Rmd',  # file 2
                    output_file =  paste("patient_profile_report_", subject, ".html", sep=''), 
                    output_dir = './patientprofile_reports')
}



