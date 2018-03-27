#install.packages("data.table")
library(data.table)
library(dplyr)
library(tidyr)
library(sp)
library(ggplot2)
library(rgeos)
library(maptools)
library(raster)
library(maps)
library(rgdal)
##First read in checklists from the eBird Basic Data set, which contains all Fox Sparrow records,
##including from incomplete checklists.
ebAllFosp <- fread("/Users/johnlloyd/GitHub/FoxSparrows/ebd_foxspa_relNov-2017.txt",
                   header = T)
ebAllFosp.reduced <- ebAllFosp %>%
  filter(STATE_PROVINCE == "Maine"|STATE_PROVINCE == "New Hampshire"|
           STATE_PROVINCE == "New York"|STATE_PROVINCE == "Vermont"|
           STATE_PROVINCE == "New Brunswick"|STATE_PROVINCE == "Nova Scotia"|
           STATE_PROVINCE == "Quebec")
library(lubridate)
ebAllFosp.reduced$MONTH = month(ebAllFosp.reduced$`OBSERVATION DATE`)
ebAllFosp.reduced$YEAR = year(ebAllFosp.reduced$`OBSERVATION DATE`)

ebAllFosp.reduced <-
  ebAllFosp.reduced%>%
  filter(MONTH == 6|MONTH == 7)
write.csv(ebAllFosp.reduced,file = "ebdFOSP_Jun_Jul.csv")

#Map them
data("wrld_simpl")
plot(wrld_simpl, xlim = c(-80,-60), ylim = c(42,50), axes = TRUE, col = "light yellow")
points(ebAllFosp.reduced$LONGITUDE, ebAllFosp.reduced$LATITUDE, col = "orange", pch = 20,
       cex = 0.5)

#browseVignettes("data.table")
sample.file <- fread("/Users/johnlloyd/Downloads/ERD2016SS/2002/checklists.csv",sep = ",", nrows = 100,
                     header = T)
match("Passerella_iliaca",names(sample.file)) #2960
match("Setophaga_striata",names(sample.file)) #3884
match("Catharus_bicknelli",names(sample.file)) #754

#Read in checklists w/columns of interest

checklists.2002 <- fread("/Users/johnlloyd/eBirdComplete/Checklists2002&Before.csv",
                         sep = ",", select = c(1:19, 2960, 3884, 754))

#Filter out US records of interest
checklists.2002.US <- checklists.2002 %>%
  #filter(Passerella_iliaca > 0) %>%
  filter(STATE_PROVINCE == "Maine"|STATE_PROVINCE == "New Hampshire") %>%
  filter(COUNTY == "Aroostook"|COUNTY == "Penobscot"|COUNTY == "Piscataquis"|COUNTY == "Somerset"|
           COUNTY == "Franklin"|COUNTY == "Oxford"|COUNTY == "Coos"|COUNTY == "Grafton") %>%
  filter(MONTH == 6|MONTH == 7)
#Filter out Canada records of interest
checklists.2002.CA <- checklists.2002 %>%
  #filter(Passerella_iliaca > 0) %>%
  filter(STATE_PROVINCE == "New Brunswick"|STATE_PROVINCE == "Nova Scotia"|STATE_PROVINCE == "Quebec")%>%
  filter(MONTH == 6|MONTH == 7)

#Bind into a single data frame
checklists.2002.US.CA <- bind_rows(checklists.2002.US, checklists.2002.CA)

#Remove intermediates
rm(checklists.2002,checklists.2002.CA,checklists.2002.US)

#2016
start.time <- Sys.time()
checklists.2016 <- fread("/Users/johnlloyd/eBirdComplete/Checklists2016.csv",
                         sep = ",", select = c(1:19, 2960, 3884, 754))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken #10.36184 mins

checklists.2016.US <- checklists.2016 %>%
  #filter(Passerella_iliaca > 0) %>%
  filter(STATE_PROVINCE == "Maine"|STATE_PROVINCE == "New Hampshire") %>%
  filter(COUNTY == "Aroostook"|COUNTY == "Penobscot"|COUNTY == "Piscataquis"|COUNTY == "Somerset"|
           COUNTY == "Franklin"|COUNTY == "Oxford"|COUNTY == "Coos"|COUNTY == "Grafton") %>%
  filter(MONTH == 6|MONTH == 7)
#Filter out Canada records of interest
checklists.2016.CA <- checklists.2016 %>%
  #filter(Passerella_iliaca > 0) %>%
  filter(STATE_PROVINCE == "New Brunswick"|STATE_PROVINCE == "Nova Scotia"|STATE_PROVINCE == "Quebec")%>%
  filter(MONTH == 6|MONTH == 7)

#Bind into a single data frame
checklists.2016.US.CA <- bind_rows(checklists.2016.US, checklists.2016.CA)

#Remove intermediates
rm(checklists.2016,checklists.2016.CA,checklists.2016.US)

#2015
start.time <- Sys.time()
checklists.2015 <- fread("/Users/johnlloyd/eBirdComplete/Checklists2015.csv",
                         sep = ",", select = c(1:19, 2960, 3884, 754))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken #9.43676

checklists.2015.US <- checklists.2015 %>%
  #filter(Passerella_iliaca > 0) %>%
  filter(STATE_PROVINCE == "Maine"|STATE_PROVINCE == "New Hampshire") %>%
  filter(COUNTY == "Aroostook"|COUNTY == "Penobscot"|COUNTY == "Piscataquis"|COUNTY == "Somerset"|
           COUNTY == "Franklin"|COUNTY == "Oxford"|COUNTY == "Coos"|COUNTY == "Grafton") %>%
  filter(MONTH == 6|MONTH == 7)
#Filter out Canada records of interest
checklists.2015.CA <- checklists.2015 %>%
  #filter(Passerella_iliaca > 0) %>%
  filter(STATE_PROVINCE == "New Brunswick"|STATE_PROVINCE == "Nova Scotia"|STATE_PROVINCE == "Quebec")%>%
  filter(MONTH == 6|MONTH == 7)

#Bind into a single data frame
checklists.2015.US.CA <- bind_rows(checklists.2015.US, checklists.2015.CA)

#Remove intermediates
rm(checklists.2015,checklists.2015.CA,checklists.2015.US)

#2014
start.time <- Sys.time()
checklists.2014 <- fread("/Users/johnlloyd/eBirdComplete/Checklists2014.csv",
                         sep = ",", select = c(1:19, 2960, 3884, 754))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken #7.112371

checklists.2014.US <- checklists.2014 %>%
  #filter(Passerella_iliaca > 0) %>%
  filter(STATE_PROVINCE == "Maine"|STATE_PROVINCE == "New Hampshire") %>%
  filter(COUNTY == "Aroostook"|COUNTY == "Penobscot"|COUNTY == "Piscataquis"|COUNTY == "Somerset"|
           COUNTY == "Franklin"|COUNTY == "Oxford"|COUNTY == "Coos"|COUNTY == "Grafton") %>%
  filter(MONTH == 6|MONTH == 7)
#Filter out Canada records of interest
checklists.2014.CA <- checklists.2014 %>%
  #filter(Passerella_iliaca > 0) %>%
  filter(STATE_PROVINCE == "New Brunswick"|STATE_PROVINCE == "Nova Scotia"|STATE_PROVINCE == "Quebec")%>%
  filter(MONTH == 6|MONTH == 7)

#Bind into a single data frame
checklists.2014.US.CA <- bind_rows(checklists.2014.US, checklists.2014.CA)

#Remove intermediates
rm(checklists.2014,checklists.2014.CA,checklists.2014.US)

#2013
start.time <- Sys.time()
checklists.2013 <- fread("/Users/johnlloyd/eBirdComplete/Checklists2013.csv",
                         sep = ",", select = c(1:19, 2960, 3884, 754))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken #5.296967

checklists.2013.US <- checklists.2013 %>%
  #filter(Passerella_iliaca > 0) %>%
  filter(STATE_PROVINCE == "Maine"|STATE_PROVINCE == "New Hampshire") %>%
  filter(COUNTY == "Aroostook"|COUNTY == "Penobscot"|COUNTY == "Piscataquis"|COUNTY == "Somerset"|
           COUNTY == "Franklin"|COUNTY == "Oxford"|COUNTY == "Coos"|COUNTY == "Grafton") %>%
  filter(MONTH == 6|MONTH == 7)
#Filter out Canada records of interest
checklists.2013.CA <- checklists.2013 %>%
  #filter(Passerella_iliaca > 0) %>%
  filter(STATE_PROVINCE == "New Brunswick"|STATE_PROVINCE == "Nova Scotia"|STATE_PROVINCE == "Quebec")%>%
  filter(MONTH == 6|MONTH == 7)

#Bind into a single data frame
checklists.2013.US.CA <- bind_rows(checklists.2013.US, checklists.2013.CA)

#Remove intermediates
rm(checklists.2013,checklists.2013.CA,checklists.2013.US)

#2012
start.time <- Sys.time()
checklists.2012 <- fread("/Users/johnlloyd/eBirdComplete/Checklists2012.csv",
                         sep = ",", select = c(1:19, 2960, 3884, 754))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken #4.13244

checklists.2012.US <- checklists.2012 %>%
  #filter(Passerella_iliaca > 0) %>%
  filter(STATE_PROVINCE == "Maine"|STATE_PROVINCE == "New Hampshire") %>%
  filter(COUNTY == "Aroostook"|COUNTY == "Penobscot"|COUNTY == "Piscataquis"|COUNTY == "Somerset"|
           COUNTY == "Franklin"|COUNTY == "Oxford"|COUNTY == "Coos"|COUNTY == "Grafton") %>%
  filter(MONTH == 6|MONTH == 7)
#Filter out Canada records of interest
checklists.2012.CA <- checklists.2012 %>%
  #filter(Passerella_iliaca > 0) %>%
  filter(STATE_PROVINCE == "New Brunswick"|STATE_PROVINCE == "Nova Scotia"|STATE_PROVINCE == "Quebec")%>%
  filter(MONTH == 6|MONTH == 7)

#Bind into a single data frame
checklists.2012.US.CA <- bind_rows(checklists.2012.US, checklists.2012.CA)

#Remove intermediates
rm(checklists.2012,checklists.2012.CA,checklists.2012.US)

#2011
start.time <- Sys.time()
checklists.2011 <- fread("/Users/johnlloyd/eBirdComplete/Checklists2011.csv",
                         sep = ",", select = c(1:19, 2960, 3884, 754))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken #2.593969 mins

checklists.2011.US <- checklists.2011 %>%
  #filter(Passerella_iliaca > 0) %>%
  filter(STATE_PROVINCE == "Maine"|STATE_PROVINCE == "New Hampshire") %>%
  filter(COUNTY == "Aroostook"|COUNTY == "Penobscot"|COUNTY == "Piscataquis"|COUNTY == "Somerset"|
           COUNTY == "Franklin"|COUNTY == "Oxford"|COUNTY == "Coos"|COUNTY == "Grafton") %>%
  filter(MONTH == 6|MONTH == 7)
#Filter out Canada records of interest
checklists.2011.CA <- checklists.2011 %>%
  #filter(Passerella_iliaca > 0) %>%
  filter(STATE_PROVINCE == "New Brunswick"|STATE_PROVINCE == "Nova Scotia"|STATE_PROVINCE == "Quebec")%>%
  filter(MONTH == 6|MONTH == 7)

#Bind into a single data frame
checklists.2011.US.CA <- bind_rows(checklists.2011.US, checklists.2011.CA)

#Remove intermediates
rm(checklists.2011,checklists.2011.CA,checklists.2011.US)

#2010
start.time <- Sys.time()
checklists.2010 <- fread("/Users/johnlloyd/eBirdComplete/Checklists2010.csv",
                         sep = ",", select = c(1:19, 2960, 3884, 754))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken #1.842453 mins

checklists.2010.US <- checklists.2010 %>%
  #filter(Passerella_iliaca > 0) %>%
  filter(STATE_PROVINCE == "Maine"|STATE_PROVINCE == "New Hampshire") %>%
  filter(COUNTY == "Aroostook"|COUNTY == "Penobscot"|COUNTY == "Piscataquis"|COUNTY == "Somerset"|
           COUNTY == "Franklin"|COUNTY == "Oxford"|COUNTY == "Coos"|COUNTY == "Grafton") %>%
  filter(MONTH == 6|MONTH == 7)
#Filter out Canada records of interest
checklists.2010.CA <- checklists.2010 %>%
  #filter(Passerella_iliaca > 0) %>%
  filter(STATE_PROVINCE == "New Brunswick"|STATE_PROVINCE == "Nova Scotia"|STATE_PROVINCE == "Quebec")%>%
  filter(MONTH == 6|MONTH == 7)

#Bind into a single data frame
checklists.2010.US.CA <- bind_rows(checklists.2010.US, checklists.2010.CA)

#Remove intermediates
rm(checklists.2010,checklists.2010.CA,checklists.2010.US)

#2009
start.time <- Sys.time()
checklists.2009 <- fread("/Users/johnlloyd/eBirdComplete/Checklists2009.csv",
                         sep = ",", select = c(1:19, 2960, 3884, 754))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken #50.58869 secs

checklists.2009.US <- checklists.2009 %>%
  #filter(Passerella_iliaca > 0) %>%
  filter(STATE_PROVINCE == "Maine"|STATE_PROVINCE == "New Hampshire") %>%
  filter(COUNTY == "Aroostook"|COUNTY == "Penobscot"|COUNTY == "Piscataquis"|COUNTY == "Somerset"|
           COUNTY == "Franklin"|COUNTY == "Oxford"|COUNTY == "Coos"|COUNTY == "Grafton") %>%
  filter(MONTH == 6|MONTH == 7)
#Filter out Canada records of interest
checklists.2009.CA <- checklists.2009 %>%
  #filter(Passerella_iliaca > 0) %>%
  filter(STATE_PROVINCE == "New Brunswick"|STATE_PROVINCE == "Nova Scotia"|STATE_PROVINCE == "Quebec")%>%
  filter(MONTH == 6|MONTH == 7)

#Bind into a single data frame
checklists.2009.US.CA <- bind_rows(checklists.2009.US, checklists.2009.CA)

#Remove intermediates
rm(checklists.2009,checklists.2009.CA,checklists.2009.US)

#2008
start.time <- Sys.time()
checklists.2008 <- fread("/Users/johnlloyd/eBirdComplete/Checklists2008.csv",
                         sep = ",", select = c(1:19, 2960, 3884, 754))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken #23.9995 secs

checklists.2008.US <- checklists.2008 %>%
  #filter(Passerella_iliaca > 0) %>%
  filter(STATE_PROVINCE == "Maine"|STATE_PROVINCE == "New Hampshire") %>%
  filter(COUNTY == "Aroostook"|COUNTY == "Penobscot"|COUNTY == "Piscataquis"|COUNTY == "Somerset"|
           COUNTY == "Franklin"|COUNTY == "Oxford"|COUNTY == "Coos"|COUNTY == "Grafton") %>%
  filter(MONTH == 6|MONTH == 7)
#Filter out Canada records of interest
checklists.2008.CA <- checklists.2008 %>%
  #filter(Passerella_iliaca > 0) %>%
  filter(STATE_PROVINCE == "New Brunswick"|STATE_PROVINCE == "Nova Scotia"|STATE_PROVINCE == "Quebec")%>%
  filter(MONTH == 6|MONTH == 7)

#Bind into a single data frame
checklists.2008.US.CA <- bind_rows(checklists.2008.US, checklists.2008.CA)

#Remove intermediates
rm(checklists.2008,checklists.2008.CA,checklists.2008.US)

#2007
start.time <- Sys.time()
checklists.2007 <- fread("/Users/johnlloyd/eBirdComplete/Checklists2007.csv",
                         sep = ",", select = c(1:19, 2960, 3884, 754))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken #20.62224 secs

checklists.2007.US <- checklists.2007 %>%
  #filter(Passerella_iliaca > 0) %>%
  filter(STATE_PROVINCE == "Maine"|STATE_PROVINCE == "New Hampshire") %>%
  filter(COUNTY == "Aroostook"|COUNTY == "Penobscot"|COUNTY == "Piscataquis"|COUNTY == "Somerset"|
           COUNTY == "Franklin"|COUNTY == "Oxford"|COUNTY == "Coos"|COUNTY == "Grafton") %>%
  filter(MONTH == 6|MONTH == 7)
#Filter out Canada records of interest
checklists.2007.CA <- checklists.2007 %>%
  #filter(Passerella_iliaca > 0) %>%
  filter(STATE_PROVINCE == "New Brunswick"|STATE_PROVINCE == "Nova Scotia"|STATE_PROVINCE == "Quebec")%>%
  filter(MONTH == 6|MONTH == 7)

#Bind into a single data frame
checklists.2007.US.CA <- bind_rows(checklists.2007.US, checklists.2007.CA)

#Remove intermediates
rm(checklists.2007,checklists.2007.CA,checklists.2007.US)

#2006
start.time <- Sys.time()
checklists.2006 <- fread("/Users/johnlloyd/eBirdComplete/Checklists2006.csv",
                         sep = ",", select = c(1:19, 2960, 3884, 754))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken #15.77527 secs

checklists.2006.US <- checklists.2006 %>%
  #filter(Passerella_iliaca > 0) %>%
  filter(STATE_PROVINCE == "Maine"|STATE_PROVINCE == "New Hampshire") %>%
  filter(COUNTY == "Aroostook"|COUNTY == "Penobscot"|COUNTY == "Piscataquis"|COUNTY == "Somerset"|
           COUNTY == "Franklin"|COUNTY == "Oxford"|COUNTY == "Coos"|COUNTY == "Grafton") %>%
  filter(MONTH == 6|MONTH == 7)
#Filter out Canada records of interest
checklists.2006.CA <- checklists.2006 %>%
  #filter(Passerella_iliaca > 0) %>%
  filter(STATE_PROVINCE == "New Brunswick"|STATE_PROVINCE == "Nova Scotia"|STATE_PROVINCE == "Quebec")%>%
  filter(MONTH == 6|MONTH == 7)

#Bind into a single data frame
checklists.2006.US.CA <- bind_rows(checklists.2006.US, checklists.2006.CA)

#Remove intermediates
rm(checklists.2006,checklists.2006.CA,checklists.2006.US)

#2005
start.time <- Sys.time()
checklists.2005 <- fread("/Users/johnlloyd/eBirdComplete/Checklists2005.csv",
                         sep = ",", select = c(1:19, 2960, 3884, 754))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken #8.5831 secs

checklists.2005.US <- checklists.2005 %>%
  #filter(Passerella_iliaca > 0) %>%
  filter(STATE_PROVINCE == "Maine"|STATE_PROVINCE == "New Hampshire") %>%
  filter(COUNTY == "Aroostook"|COUNTY == "Penobscot"|COUNTY == "Piscataquis"|COUNTY == "Somerset"|
           COUNTY == "Franklin"|COUNTY == "Oxford"|COUNTY == "Coos"|COUNTY == "Grafton") %>%
  filter(MONTH == 6|MONTH == 7)
#Filter out Canada records of interest
checklists.2005.CA <- checklists.2005 %>%
  #filter(Passerella_iliaca > 0) %>%
  filter(STATE_PROVINCE == "New Brunswick"|STATE_PROVINCE == "Nova Scotia"|STATE_PROVINCE == "Quebec")%>%
  filter(MONTH == 6|MONTH == 7)

#Bind into a single data frame
checklists.2005.US.CA <- bind_rows(checklists.2005.US, checklists.2005.CA)

#Remove intermediates
rm(checklists.2005,checklists.2005.CA,checklists.2005.US)

#2004
start.time <- Sys.time()
checklists.2004 <- fread("/Users/johnlloyd/eBirdComplete/Checklists2004.csv",
                         sep = ",", select = c(1:19, 2960, 3884, 754))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken #6.170227 secs

checklists.2004.US <- checklists.2004 %>%
  #filter(Passerella_iliaca > 0) %>%
  filter(STATE_PROVINCE == "Maine"|STATE_PROVINCE == "New Hampshire") %>%
  filter(COUNTY == "Aroostook"|COUNTY == "Penobscot"|COUNTY == "Piscataquis"|COUNTY == "Somerset"|
           COUNTY == "Franklin"|COUNTY == "Oxford"|COUNTY == "Coos"|COUNTY == "Grafton") %>%
  filter(MONTH == 6|MONTH == 7)
#Filter out Canada records of interest
checklists.2004.CA <- checklists.2004 %>%
  #filter(Passerella_iliaca > 0) %>%
  filter(STATE_PROVINCE == "New Brunswick"|STATE_PROVINCE == "Nova Scotia"|STATE_PROVINCE == "Quebec")%>%
  filter(MONTH == 6|MONTH == 7)

#Bind into a single data frame
checklists.2004.US.CA <- bind_rows(checklists.2004.US, checklists.2004.CA)

#Remove intermediates
rm(checklists.2004,checklists.2004.CA,checklists.2004.US)

#2003
start.time <- Sys.time()
checklists.2003 <- fread("/Users/johnlloyd/eBirdComplete/Checklists2003.csv",
                         sep = ",", select = c(1:19, 2960, 3884, 754))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken #7.322242 secs

checklists.2003.US <- checklists.2003 %>%
  #filter(Passerella_iliaca > 0) %>%
  filter(STATE_PROVINCE == "Maine"|STATE_PROVINCE == "New Hampshire") %>%
  filter(COUNTY == "Aroostook"|COUNTY == "Penobscot"|COUNTY == "Piscataquis"|COUNTY == "Somerset"|
           COUNTY == "Franklin"|COUNTY == "Oxford"|COUNTY == "Coos"|COUNTY == "Grafton") %>%
  filter(MONTH == 6|MONTH == 7)
#Filter out Canada records of interest
checklists.2003.CA <- checklists.2003 %>%
  #filter(Passerella_iliaca > 0) %>%
  filter(STATE_PROVINCE == "New Brunswick"|STATE_PROVINCE == "Nova Scotia"|STATE_PROVINCE == "Quebec")%>%
  filter(MONTH == 6|MONTH == 7)

#Bind into a single data frame
checklists.2003.US.CA <- bind_rows(checklists.2003.US, checklists.2003.CA)

#Remove intermediates
rm(checklists.2003,checklists.2003.CA,checklists.2003.US)

#Bind them all together
checklists.all <- rbind(checklists.2002.US.CA,checklists.2003.US.CA,checklists.2004.US.CA,
                        checklists.2005.US.CA, checklists.2006.US.CA, checklists.2007.US.CA,
                        checklists.2008.US.CA, checklists.2009.US.CA, checklists.2010.US.CA,
                        checklists.2011.US.CA, checklists.2012.US.CA, checklists.2013.US.CA,
                        checklists.2014.US.CA, checklists.2015.US.CA, checklists.2016.US.CA)

#Remove individual years
rm(checklists.2002.US.CA,checklists.2003.US.CA,checklists.2004.US.CA,
   checklists.2005.US.CA, checklists.2006.US.CA, checklists.2007.US.CA,
   checklists.2008.US.CA, checklists.2009.US.CA, checklists.2010.US.CA,
   checklists.2011.US.CA, checklists.2012.US.CA, checklists.2013.US.CA,
   checklists.2014.US.CA, checklists.2015.US.CA, checklists.2016.US.CA)
rm(end.time, start.time, time.taken)

#Create binary P/A variable
checklists.all$FOSPp <- checklists.all$Passerella_iliaca
checklists.all$FOSPp[checklists.all$FOSPp>0] <- 1
checklists.all$FOSPp <- as.numeric(checklists.all$FOSPp)
checklists.all$BITHp <- checklists.all$Catharus_bicknelli
checklists.all$BITHp[checklists.all$BITHp>0] <- 1
checklists.all$BITHp <- as.numeric(checklists.all$BITHp)
checklists.all$BLPWp <- checklists.all$Setophaga_striata
checklists.all$BLPWp[checklists.all$BLPWp>0] <- 1
checklists.all$BLPWp <- as.numeric(checklists.all$BLPWp)

#Exploring data
#Get rid of the New York counts that slipped in (Franklin County)
checklists.all <- checklists.all %>%
  filter(STATE_PROVINCE != "New York" & STATE_PROVINCE != "Vermont")
unique(checklists.all$STATE_PROVINCE)
unique(checklists.all$PRIMARY_CHECKLIST_FLAG) #flag is "?" when duplicate
unique(checklists.all$COUNT_TYPE)
## We have the following
## "P62" "P22" "P21" "P23" "P35" "P39" "P48" "P40" "P60"
## P62 = Historical, could be any kind of count
## P21 = Stationary
## P22 = Traveling
## P23 = Area
## P35 = Yard Count
## P39 = Loon Watch
## P48 = Random
## P40 = My Yard eBird
## P60 = Pelagic

length(checklists.all$COUNT_TYPE[checklists.all$COUNT_TYPE == "P62"]) #2408
length(checklists.all$COUNT_TYPE[checklists.all$COUNT_TYPE == "P22"]) #56026
length(checklists.all$COUNT_TYPE[checklists.all$COUNT_TYPE == "P21"]) #30812
length(checklists.all$COUNT_TYPE[checklists.all$COUNT_TYPE == "P23"]) #1034
length(checklists.all$COUNT_TYPE[checklists.all$COUNT_TYPE == "P35"]) #1
length(checklists.all$COUNT_TYPE[checklists.all$COUNT_TYPE == "P39"]) #3
length(checklists.all$COUNT_TYPE[checklists.all$COUNT_TYPE == "P48"]) #20
length(checklists.all$COUNT_TYPE[checklists.all$COUNT_TYPE == "P60"]) #13
length(checklists.all$COUNT_TYPE[checklists.all$COUNT_TYPE == "P40"]) #6

## Will want to remove count types incompatible with observing Fox Sparrows: P35, P39, P40, or P60
checklists.all$PRIMARY_CHECKLIST_FLAG <- as.factor(checklists.all$PRIMARY_CHECKLIST_FLAG)
checklists.all$COUNT_TYPE <- as.factor(checklists.all$COUNT_TYPE)

checklists.all$COUNT_TYPE <- as.factor(checklists.all$COUNT_TYPE)
  checklists.all <- checklists.all %>%
  filter(COUNT_TYPE %in% c("P62","P22","P23","P48", "P21")) %>%
    droplevels()


## Now split out the duplicate records generated by the group checklist option.
first.checklists <- checklists.all %>%
  filter(PRIMARY_CHECKLIST_FLAG == "1")

second.checklists <- checklists.all %>%
  filter(PRIMARY_CHECKLIST_FLAG == "?")

## Lump any secondary group checklists with more than 1 additional list per group,
## and calculate the maximum value of the FOSPp variable (presence/absence of FOSP).
## This should identify any group checklists edited by individual members to add an 
## observation not seen by the rest of the group.
second.checklists <- 
  second.checklists %>%
  group_by(GROUP_ID) %>%
  summarize(FOSPmax = max(FOSPp))

## Join the two lists.
joined.checklist <- inner_join(second.checklists,first.checklists, by = "GROUP_ID")

## Calculate a difference between the maximum FOSP count (1 = present, 0 = absent) from
## the secondary checklists in the group and the maximum FOSP count from the primary checklist.
joined.checklist <- joined.checklist %>%
  mutate(difference = FOSPmax - FOSPp)

## If the values match, "difference" should equal zero.
## Here, we find 3 cases where the value = -1. This indicates
## that the primary checklist reported FOSP and the secondary 
## lists did not (presumably only 1 observer in the group saw the bird).
## So, we can safely remove non-primary checklists without affecting the 
## incidence of FOSP detections.
summary(joined.checklist$difference)

## Clean up
rm(first.checklists,second.checklists,joined.checklist)

## Reduce the checklists to only non-duplicates:
checklists.unique <- checklists.all %>%
  filter(PRIMARY_CHECKLIST_FLAG == 1)
checklists.unique$year.factor <- as.factor(checklists.unique$YEAR)

## Write this file to a CSV. Then open in QGIS and create shapefile. Then join with the grid cell shapefile
## to create a new file that has a grid cell for each record. Write this shapefile to a .csv called
## "uniqueChecklistsGridded.csv". Filter this file to remove the rows (observations) without a grid number:
## these are pelagic checklists. The grid was limited to the land area of the study area, so observations
## without grids occurred over open ocean.
write.csv(checklists.unique, file = "uniqueChecklists.csv")
uniqueChecklistsGridded <- read.csv(file = "uniqueChecklistsGridded.csv")

## Look at change in number of checklists submitted
uniqueChecklistsGridded %>%
  filter(COUNTRY == "United States") %>%
  group_by(year.factor) %>%
  summarize(n = n()) %>%
  ggplot(.,aes(year.factor,n)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 270,
                                                                                                vjust = 0.5,
                                                                                                size = 8)) +
  xlab("Year") + ylab("Number of checklists") + labs(title = "Number of eBird checklists submitted annually",
                                                     subtitle = "northern and western counties of Maine & NH")

#Big jump occurs from 1991, with 3 checklists submitted, to 1992, with 65.
uniqueChecklistsGridded %>%
  filter(COUNTRY == "United States") %>%
  filter(YEAR > 1990) %>%
  group_by(YEAR) %>%
  summarize(n = n())

## Look at change in number of grids with checklists submitted
uniqueChecklistsGridded %>%
  filter(COUNTRY == "United States") %>%
  group_by(year.factor) %>%
  summarize(n = n_distinct(grid.id)) %>%
  ggplot(.,aes(year.factor,n)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 270,
                                                                                                vjust = 0.5,
                                                                                                size = 8)) +
  xlab("Year") + ylab("Number of grids with lists") + labs(title = "Number of grids with eBird checklists",
                                                     subtitle = "northern and western counties of Maine & NH")

#Big jump occurs from 1991, with 1 grid covered, to 1992, with 14.
uniqueChecklistsGridded %>%
  filter(COUNTRY == "United States") %>%
  filter(YEAR > 1990) %>%
  group_by(YEAR) %>%
  summarize(n = n_distinct(grid.id))%>%
  View()


## Proportion of grid cells with Fox Sparrows present is highly variable because of small
## sample sizes:

props <- uniqueChecklistsGridded %>%
  filter(COUNTRY == "United States") %>%
  group_by(YEAR, grid.id) %>%
  summarize(FOSPp = max(FOSPp)) %>%
  group_by(YEAR, FOSPp)%>%
  summarize(n = n()) %>%
  mutate(prop = n/sum(n)) %>%
  filter(FOSPp == 0) %>%
  mutate(prop = 1-prop)

ggplot(data = props, aes(x = YEAR, y = prop)) + geom_point() + 
  xlab("Year") + ylab("Proportion of grids with FOSP") + 
  labs(title = "Number of grids with eBird checklists",
       subtitle = "northern and western counties of Maine & NH")

#In Canada? Not much signal...
props <- uniqueChecklistsGridded %>%
  filter(COUNTRY == "Canada") %>%
  group_by(YEAR, grid.id) %>%
  summarize(FOSPp = max(FOSPp)) %>%
  group_by(YEAR, FOSPp)%>%
  summarize(n = n()) %>%
  mutate(prop = n/sum(n)) %>%
  filter(FOSPp == 0) %>%
  mutate(prop = 1-prop)

ggplot(data = props, aes(x = YEAR, y = prop)) + geom_point() + 
  xlab("Year") + ylab("Proportion of grids with FOSP") + 
  labs(title = "Number of grids with eBird checklists",
       subtitle = "eastern Canada")

#BITH looks similar to FOSP
props <- uniqueChecklistsGridded %>%
  filter(COUNTRY == "United States") %>%
  group_by(YEAR, grid.id) %>%
  summarize(BITHp = max(BITHp)) %>%
  group_by(YEAR, BITHp)%>%
  summarize(n = n()) %>%
  mutate(prop = n/sum(n)) %>%
  filter(BITHp == 0) %>%
  mutate(prop = 1-prop)

ggplot(data = props, aes(x = YEAR, y = prop)) + geom_point() + 
  xlab("Year") + ylab("Proportion of grids with BITH") + 
  labs(title = "Number of grids with eBird checklists",
       subtitle = "northern and western counties of Maine & NH")

#BLPW have declined
props <- uniqueChecklistsGridded %>%
  filter(COUNTRY == "United States") %>%
  group_by(YEAR, grid.id) %>%
  summarize(BLPWp = max(BLPWp)) %>%
  group_by(YEAR, BLPWp)%>%
  summarize(n = n()) %>%
  mutate(prop = n/sum(n)) %>%
  filter(BLPWp == 0) %>%
  mutate(prop = 1-prop)

ggplot(data = props, aes(x = YEAR, y = prop)) + geom_point() + 
  xlab("Year") + ylab("Proportion of grids with BLPW") + 
  labs(title = "Number of grids with eBird checklists",
       subtitle = "northern and western counties of Maine & NH")

#Look at co-occurence of species
#Blackpoll + Fox:
tablex <- table(checklists.unique$FOSPp,checklists.unique$BLPWp)
tablex
prop.table(tablex, margin = 1) #47.4% of Fox Sparrows obs co-occur with BLPW 
prop.table(tablex, margin = 2) #34.5% of BLPW obs co-occur with Fox Sparrow

#BITH + Fox
tablex <- table(checklists.unique$FOSPp,checklists.unique$BITHp)
tablex
prop.table(tablex, margin = 1) # 11.5 % of Fox Sparrows obs co-occur with BITH 
prop.table(tablex, margin = 2) # 37.8% of BITH obs co-occur with Fox Sparrows

#turn this into a 2-column successes/failures frame:
props <- uniqueChecklistsGridded %>%
  filter(COUNTRY == "United States") %>%
  filter(YEAR > 2002) %>%
  group_by(YEAR,grid.id) %>%
  summarize(FOSPp = max(FOSPp), effort = sum(EFFORT_HRS)) %>%
  group_by(YEAR, FOSPp) %>%
  summarize(n = n_distinct(grid.id), effort = sum(effort))

props <- props %>%
mutate(success = ifelse(FOSPp == 1,n,0))

props <- props %>%
  mutate(failures = ifelse(FOSPp == 0,n,0))

props <- props %>%
  group_by(YEAR) %>%
  summarize(success = max(success), failures = max(failures),
            effort = sum(effort)) %>%
  mutate(proportion = success/(success+failures))

#Make a long version for plotting in ggplot
props.long <- gather(props, outcome, number, success:failures, factor_key = TRUE)
ggplot(data = props.long, aes(x = YEAR, y = number)) + geom_bar(aes(fill = outcome), position = "dodge", 
                                                                stat = "identity") + 
  xlab("Year") + ylab("Number of grids") + 
  labs(title = "eBird checklists with and without FOSP",
       subtitle = "northern and western counties of Maine & NH")
  
#Model incidence ~ time
y <- cbind(props$success,props$failures)  


FOSP.m1 <- glm(y ~ props$YEAR + props$effort, family = binomial)
summary(FOSP.m1)

FOSP.m2 <- glm(y ~ props$YEAR, family = binomial)
summary(FOSP.m2)

# The best model is the simpler model, with an effect of Year only:
anova(FOSP.m1, FOSP.m2, test = "Chisq")

# make sequence of x
xyear <- seq(from = 2003, to = 2016, by = 1)

# predict values of y from xv and m2
newdata <- data.frame(YEAR = seq(from = 2003, to = 2016, by = 1), effort = rep(1190,14))
yvals <- predict(FOSP.m2, newdata = newdata, type = "response",se.fit = TRUE)
yvals$upperci <- yvals$fit + yvals$se.fit*2
yvals$lowerci <- yvals$fit - yvals$se.fit*2

opar <- par()
par(mar = c(5.1,5.1,4.1,2.1))
plot(x = newdata$YEAR, y = props$proportion, ylab = "Proportion of checklists\nreporting Fox Sparrow",
     xlab = "Year",
     pch = 16, col = "blue", lwd = 2)

# add the fitted line
lines(newdata$YEAR, yvals$fit, col = "red", lwd = 2)
lines(newdata$YEAR, yvals$lowerci, col = "red", lwd = 2, lty = 3)
lines(newdata$YEAR, yvals$upperci, col = "red", lwd = 2, lty = 3)
par(opar)

# Repeat approach for BITH:
#turn this into a 2-column successes/failures frame:
propsBith <- uniqueChecklistsGridded %>%
  filter(COUNTRY == "United States") %>%
  filter(YEAR > 2002) %>%
  group_by(YEAR,grid.id) %>%
  summarize(BITHp = max(BITHp), effort = sum(EFFORT_HRS)) %>%
  group_by(YEAR, BITHp) %>%
  summarize(n = n_distinct(grid.id), effort = sum(effort))

propsBith <- propsBith %>%
  mutate(success = ifelse(BITHp == 1,n,0))

propsBith <- propsBith %>%
  mutate(failures = ifelse(BITHp == 0,n,0))

propsBith <- propsBith %>%
  group_by(YEAR) %>%
  summarize(success = max(success), failures = max(failures),
            effort = sum(effort)) %>%
  mutate(proportion = success/(success+failures))

#Make a long version for plotting in ggplot
propsBith.long <- gather(propsBith, outcome, number, success:failures, factor_key = TRUE)
ggplot(data = propsBith.long, aes(x = YEAR, y = number)) + geom_bar(aes(fill = outcome), position = "dodge", 
                                                                stat = "identity") + 
  xlab("Year") + ylab("Number of grids") + 
  labs(title = "eBird checklists with and without BITH",
       subtitle = "northern and western counties of Maine & NH")

#Model incidence ~ time
yB <- cbind(propsBith$success,propsBith$failures)  


BITH.m1 <- glm(yB ~ propsBith$YEAR + propsBith$effort, family = binomial)
summary(BITH.m1)

BITH.m2 <- glm(yB ~ propsBith$YEAR, family = binomial)
summary(BITH.m2)

# The best model is the simpler model, with an effect of Year only:
anova(BITH.m1, BITH.m2, test = "Chisq")

# predict values of y from xv and m2
yvalsBith <- predict(BITH.m2, newdata = newdata, type = "response",se.fit = TRUE)
yvalsBith$upperci <- yvalsBith$fit + yvalsBith$se.fit*2
yvalsBith$lowerci <- yvalsBith$fit - yvalsBith$se.fit*2

opar <- par()
par(mar = c(5.1,5.1,4.1,2.1))
plot(x = newdata$YEAR, y = propsBith$proportion, ylab = "Proportion of checklists\nreporting Bicknell's Thrush",
     xlab = "Year",
     pch = 16, col = "blue", lwd = 2)

# add the fitted line
lines(newdata$YEAR, yvalsBith$fit, col = "red", lwd = 2)
lines(newdata$YEAR, yvalsBith$lowerci, col = "red", lwd = 2, lty = 3)
lines(newdata$YEAR, yvalsBith$upperci, col = "red", lwd = 2, lty = 3)
par(opar)
# and for BLPW:
#turn this into a 2-column successes/failures frame:
propsBlpw <- uniqueChecklistsGridded %>%
  filter(COUNTRY == "United States") %>%
  filter(YEAR > 2002) %>%
  group_by(YEAR,grid.id) %>%
  summarize(BLPWp = max(BLPWp), effort = sum(EFFORT_HRS)) %>%
  group_by(YEAR, BLPWp) %>%
  summarize(n = n_distinct(grid.id), effort = sum(effort))

propsBlpw <- propsBlpw %>%
  mutate(success = ifelse(BLPWp == 1,n,0))

propsBlpw<- propsBlpw %>%
  mutate(failures = ifelse(BLPWp == 0,n,0))

propsBlpw <- propsBlpw %>%
  group_by(YEAR) %>%
  summarize(success = max(success), failures = max(failures),
            effort = sum(effort)) %>%
  mutate(proportion = success/(success+failures))


#Make a long version for plotting in ggplot
propsBlpw.long <- gather(propsBlpw, outcome, number, success:failures, factor_key = TRUE)
ggplot(data = propsBlpw.long, aes(x = YEAR, y = number)) + geom_bar(aes(fill = outcome), position = "dodge", 
                                                                    stat = "identity") + 
  xlab("Year") + ylab("Number of grids") + 
  labs(title = "eBird checklists with and without BLPW",
       subtitle = "northern and western counties of Maine & NH")

#Model incidence ~ time
yBl <- cbind(propsBlpw$success,propsBlpw$failures)  


BLPW.m1 <- glm(yBl ~ propsBlpw$YEAR + propsBlpw$effort, family = binomial)
summary(BLPW.m1)

BLPW.m2 <- glm(yBl ~ propsBlpw$YEAR, family = binomial)
summary(BLPW.m2)

BLPW.m3 <- glm(yBl ~ propsBlpw$YEAR + I(propsBlpw$YEAR^2) + propsBlpw$effort, family = binomial)
BLPW.m3 <- glm(yBl ~ poly(propsBlpw$YEAR,2) + propsBlpw$effort, family = binomial)

summary(BLPW.m3)

# The best model is the simpler model, with an effect of Year only:
anova(BLPW.m1, BLPW.m2, BLPW.m3, test = "Chisq")

# predict values of y from xv and m2
yvalsBlpw <- predict(BLPW.m3, newdata = newdata, type = "response",se.fit = TRUE)
yvalsBlpw$upperci <- round(yvalsBlpw$fit + yvalsBlpw$se.fit*2)
yvalsBlpw$lowerci <- round(yvalsBlpw$fit - yvalsBlpw$se.fit*2)

opar <- par()
par(mar = c(5.1,5.1,4.1,2.1))
plot(x = newdata$YEAR, y = propsBlpw$proportion, ylab = "Proportion of checklists\nreporting Blackpoll Warbler",
     xlab = "Year",
     pch = 16, col = "blue", lwd = 2)

# add the fitted line
lines(newdata$YEAR, yvalsBlpw$fit, col = "red", lwd = 2)
lines(newdata$YEAR, yvalsBlpw$lowerci, col = "red", lwd = 2, lty = 3)
lines(newdata$YEAR, yvalsBlpw$upperci, col = "red", lwd = 2, lty = 3)
par(opar)

##
opar <- par()
par(mar = c(5.1,5.1,4.1,2.1))
plot(x = newdata$YEAR, y = props$proportion, ylab = "Proportion of checklists reporting Fox Sparrow,\nBicknell's Thrush, or Blackpoll Warbler",
     xlab = "Year",
     pch = 16, col = "blue", lwd = 2,
     ylim = c(0,0.45))
points(x = newdata$YEAR,y = propsBith$proportion)
points(x = newdata$YEAR, y = propsBlpw$proportion, col = "red")

lines(newdata$YEAR, yvals$fit, col = "blue", lwd = 2)
lines(newdata$YEAR, yvals$lowerci, col = "blue", lwd = 2, lty = 3)
lines(newdata$YEAR, yvals$upperci, col = "blue", lwd = 2, lty = 3)

lines(newdata$YEAR, yvalsBith$fit, col = "black", lwd = 2)
lines(newdata$YEAR, yvalsBith$lowerci, col = "black", lwd = 2, lty = 3)
lines(newdata$YEAR, yvalsBith$upperci, col = "black", lwd = 2, lty = 3)

lines(newdata$YEAR, yvalsBlpw$fit, col = "red", lwd = 2)
lines(newdata$YEAR, yvalsBlpw$lowerci, col = "red", lwd = 2, lty = 3)
lines(newdata$YEAR, yvalsBlpw$upperci, col = "red", lwd = 2, lty = 3)
