url1 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46035h"
url2 <- ".txt.gz&dir=data/historical/stdmet/"
library(lubridate)
years <- c(1985:2011, 2014:2017)
urls <- str_c(url1, years, url2, sep = "")
filenames <- str_c("Buo", years, sep = "")

for (i in 1:length(urls)) {
  suppressMessages(
    assign(filenames[i], read_table(urls[i], col_names = TRUE))   ###  col_name = True means, there is column name
  )
  
  file <- get(filenames[i])
  
  colnames(file)[1] <-"YYYY"
  
  Buo <- file
}


#### Buoy Statation 46035 year 1985:2011,2014:2017

url1 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=adka2h"
url2 <- ".txt.gz&dir=data/historical/stdmet/"

years <- c(2012:2013)
urls <- str_c(url1, years, url2, sep = "")
filenames <- str_c("Buo", years, sep = "")

for (i in 1:length(urls)) {
  suppressMessages(
    assign(filenames[i], read_table(urls[i], col_names = TRUE))   
  )
  file <- get(filenames[i])
  colnames(file)[1] <-"YYYY"
  Buo <- file
}

all.data <- list(Buo1985,Buo1986,Buo1987,Buo1988,Buo1989,Buo1990,Buo1991,Buo1992,Buo1993,Buo1994
                 ,Buo1995,Buo1996,Buo1997,Buo1998,Buo1999,Buo2000,Buo2001,Buo2002,Buo2003,Buo2004
                 ,Buo2005,Buo2006,Buo2007,Buo2008,Buo2009,Buo2010,Buo2011,Buo2012,Buo2013,Buo2014
                 ,Buo2015,Buo2016,Buo2017)
filenames <- paste('Buo', 1985:2017, sep='')
for (j in 1:length(all.data)) {
  #### Convert column name from YY or #YY to YYYY
  colnames(all.data[[j]])[1] <- 'YYYY'
  #### Select the columns that we need.However,the data from 2012:2013 conains an extra column
  #### called mm(mins), so we selected them seperately.
  if(j %in% c(1:27,30:33)){all.data[[j]] <- select(all.data[[j]], YYYY, MM, DD, hh, ATMP, WTMP)}
  if(j %in% c(28:29)){all.data[[j]] <- select(all.data[[j]], YYYY, MM, DD, hh,mm, ATMP, WTMP)}
  if(j %in% 23:33){all.data[[j]] <- all.data[[j]][-1,] }
  #### filter out the row only contains hh == 12, 
  #if(j %in% c(1:27,30:33)) {all.data[[j]] <- all.data[[j]][!(all.data[[j]]$hh != 12),]}
  #### the data from year2010:2014 contains mm(mins) column, so we have to do two filter,
  #### first filter the row only contains hh==12, then filter out the row only contains mm ==00
  if(j %in% 28:29) {all.data[[j]] <- all.data[[j]] %>% filter(mm == "00")}
  #### then remove this mm column that we dont need anymore.
  if(j %in% c(28:29)){all.data[[j]] <- select(all.data[[j]],-mm)}
  assign(filenames[j], all.data[[j]])
}

Buo1985$YYYY[Buo1985$YYYY == "85"] <- "1985"
Buo1986$YYYY[Buo1986$YYYY == "86"] <- "1986"
Buo1987$YYYY[Buo1987$YYYY == "87"] <- "1987"
Buo1988$YYYY[Buo1988$YYYY == "88"] <- "1988"
Buo1989$YYYY[Buo1989$YYYY == "89"] <- "1989"
Buo1990$YYYY[Buo1990$YYYY == "90"] <- "1990"
Buo1991$YYYY[Buo1991$YYYY == "91"] <- "1991"
Buo1992$YYYY[Buo1992$YYYY == "92"] <- "1992"
Buo1993$YYYY[Buo1993$YYYY == "93"] <- "1993"
Buo1994$YYYY[Buo1994$YYYY == "94"] <- "1994"
Buo1995$YYYY[Buo1995$YYYY == "95"] <- "1995"
Buo1996$YYYY[Buo1996$YYYY == "96"] <- "1996"
Buo1997$YYYY[Buo1997$YYYY == "97"] <- "1997"
Buo1998$YYYY[Buo1998$YYYY == "98"] <- "1998"

All_yearh1 <- rbind(Buo1985,Buo1986,Buo1987,Buo1988,Buo1989,Buo1990,Buo1991,Buo1992,Buo1993,
                   Buo1994,Buo1995,Buo1996,Buo1997,Buo1998,Buo1999,Buo2000,Buo2001,Buo2002,Buo2003,Buo2004,Buo2005,Buo2006,Buo2007,Buo2008,Buo2009,Buo2010,Buo2011,Buo2012,Buo2013,Buo2014,Buo2015,Buo2016,Buo2017)
#as.POSIXct(do.call(sprintf, c(df1, fmt = c("%02d-%02d-%4d %02d"))), format = "%d-%m-%Y %H")
#All_year1$Date <- as.Date( paste(All_year1$YYYY,All_year1$MM ,All_year1$DD,All_year1$hh, sep = "." )  , format = "%Y.%m.%d %h" )
All_yearh2 <- with(All_yearh1, ymd_h(paste(YYYY, MM, DD, hh, sep= ' ')))
All_yearh3 <- select(All_yearh1,-YYYY,-MM,-DD,-hh)
All_yearh4 <- cbind(All_yearh2,All_yearh3)
colnames(All_yearh4)[1] <- "Date"
All_yearh5 <- All_yearh4 %>%
  select(Date, everything())%>%
  mutate(ATMP, ATMP=as.numeric(ATMP)) %>% 
  mutate(WTMP, WTMP=as.numeric(WTMP)) %>% 
  mutate(ATMP = replace(ATMP, ATMP==999.0, NA))%>% 
  mutate(WTMP = replace(WTMP, WTMP==999.0, NA))%>% 
  mutate(ATMP = replace(ATMP, ATMP==99.0, NA))%>% 
  mutate(WTMP = replace(WTMP, WTMP==99.0, NA))

All_year_hourly <- All_yearh5 %>% gather(key ="Type",value = "Temperature",-Date)
allhoursplot <- ggplot(data = All_year_hourly, aes(Date, Temperature, color = Type),na.rm = T) +geom_line()+ ggtitle("Southern Bering Sea Air and Water Temperature hourly over past 30 years")


# T-TEST
# replacing 99 values with NA
All_yearttesth <- All_yearh1 %>%
  mutate(ATMP, ATMP=as.numeric(ATMP)) %>% 
  mutate(WTMP, WTMP=as.numeric(WTMP)) %>% 
  mutate(ATMP = replace(ATMP, ATMP==999.0, NA))%>% 
  mutate(WTMP = replace(WTMP, WTMP==999.0, NA))%>% 
  mutate(ATMP = replace(ATMP, ATMP==99.0, NA))%>% 
  mutate(WTMP = replace(WTMP, WTMP==99.0, NA))

# extracting air temps for 1986 and 2017 as vectors
atmp1986h <- All_yearttesth %>% filter(YYYY == 1986) %>% pull(ATMP)
atmp2017h <- All_yearttesth %>% filter(YYYY == 2017) %>% pull(ATMP)

atmp1986h <- na.omit(atmp1986h)
atmp2017h <- na.omit(atmp2017h)
# T-test for air temp
var.test(atmp1986h, atmp2017h)
t.test(atmp1986h, atmp2017h, var.equal = FALSE)

# extracting sea temps for 1986 and 2017 as vectors 
wtmp1986h <- All_yearttesth %>% filter(YYYY == 1986) %>% pull(WTMP)
wtmp2017h <- All_yearttesth %>% filter(YYYY == 2017) %>% pull(WTMP)

wtmp1986h <- na.omit(wtmp1986h)
wtmp2017h <- na.omit(wtmp2017h)
# T-test for sea temp
var.test(wtmp1986h, wtmp2017h)
t.test(wtmp1986h, wtmp2017h, var.equal = FALSE)
