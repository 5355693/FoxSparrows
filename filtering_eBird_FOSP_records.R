ebAllFosp <- fread("/Users/johnlloyd/Downloads/ebd_foxspa_relNov-2017/ebd_foxspa_relNov-2017.txt",
                     header = T)
ebAllFosp.reduced <- ebAllFosp %>%
  filter(STATE_PROVINCE == "Maine"|STATE_PROVINCE == "New Hampshire"|
           STATE_PROVINCE == "New York"|STATE_PROVINCE == "Vermont"|
           STATE_PROVINCE == "New Brunswick"|STATE_PROVINCE == "Nova Scotia"|
           STATE_PROVINCE == "Quebec")
library(lubridate)
ebAllFosp.reduced$MONTH = month(sample.file.reduced$`OBSERVATION DATE`)
ebAllFosp.reduced$YEAR = year(sample.file.reduced$`OBSERVATION DATE`)

ebAllFosp.reduced <-
  ebAllFosp.reduced%>%
  filter(MONTH == 6|MONTH == 7)
write.csv(ebAllFosp.reduced,file = "ebdFOSP_Jun_Jul")