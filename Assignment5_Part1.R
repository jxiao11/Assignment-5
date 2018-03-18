library(tidyverse)
library(stringr)
library(lubridate)
library(dplyr)

# Load Data from National Data Buoy Center, Data year 1985 to 2011 and 
# 2014 to 2017 were from Buoy Station 46035.However, Buoy Station 46035 
# contains incompleted 2012 year data and missed data for 2013. So We chose
# the year 2012 and 2013 data from Buoy station adka2 in order to get a complete
# time series data. By the way, adka2 is one of the nearest Buoy station to 
# 46035.


#### Load data from web, no download needed. Buoy Statation 46035 year 1985:2011,2014:2017

url1 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46035h"
url2 <- ".txt.gz&dir=data/historical/stdmet/"
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

### Convert Seperate files to a list

all.data <- list(Buo1985,Buo1986,Buo1987,Buo1988,Buo1989,Buo1990,Buo1991,Buo1992,Buo1993,Buo1994
                 ,Buo1995,Buo1996,Buo1997,Buo1998,Buo1999,Buo2000,Buo2001,Buo2002,Buo2003,Buo2004
                 ,Buo2005,Buo2006,Buo2007,Buo2008,Buo2009,Buo2010,Buo2011,Buo2012,Buo2013,Buo2014
                 ,Buo2015,Buo2016,Buo2017)
filenames <- paste('Buo', 1985:2017, sep='')

### Use list index,dplyr,tidyverse methods to do the first part clean of years data, 
### because data Buo2010: Buo2014 from different Buoy station(adka2)

for (j in 1:length(all.data)) {
  #### Convert column name from YY or #YY to YYYY
  colnames(all.data[[j]])[1] <- 'YYYY'
  #### Select the columns that we need.However,the data from 2012:2013 conains an extra column
  #### called mm(mins), so we selected them seperately.
  if(j %in% c(1:27,30:33)){all.data[[j]] <- select(all.data[[j]], YYYY, MM, DD, hh, ATMP, WTMP)}
  if(j %in% c(28:29)){all.data[[j]] <- select(all.data[[j]], YYYY, MM, DD, hh,mm, ATMP, WTMP)}
  if(j %in% 23:33){all.data[[j]] <- all.data[[j]][-1,] }
  #### filter out the row only contains hh == 12, 
  if(j %in% c(1:27,30:33)) {all.data[[j]] <- all.data[[j]][!(all.data[[j]]$hh != 12),]}
  #### the data from year2010:2014 contains mm(mins) column, so we have to do two filter,
  #### first filter the row only contains hh==12, then filter out the row only contains mm ==00
  if(j %in% 28:29) {all.data[[j]] <- all.data[[j]] %>% filter(hh == "12")}
  if(j %in% 28:29) {all.data[[j]] <- all.data[[j]] %>% filter(mm == "00")}
  #### then remove this mm column that we dont need anymore.
  if(j %in% c(28:29)){all.data[[j]] <- select(all.data[[j]],-mm)}
  assign(filenames[j], all.data[[j]])
}

#### Change two digits year value to 4 digits.
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

#### Now, all the data from year1985 to 2017 only contains 
#### yyyy,mm,dd,hh,ATMP,WTMP. Then, we are able to combine it to one single 
#### table.More dplyr and tidyverse used to do a further data cleanning.


#### Combine all data from year 1985 to 2017
All_year1 <- rbind(Buo1985,Buo1986,Buo1987,Buo1988,Buo1989,Buo1990,Buo1991,Buo1992,Buo1993,
                   Buo1994,Buo1995,Buo1996,Buo1997,Buo1998,Buo1999,Buo2000,Buo2001,Buo2002,Buo2003,Buo2004,Buo2005,Buo2006,Buo2007,Buo2008,Buo2009,Buo2010,Buo2011,Buo2012,Buo2013,Buo2014,Buo2015,Buo2016,Buo2017)

#### combine yyyy,mm,dd columns to format date YYYY-MM-DD

All_year1$Date <- as.Date( paste(All_year1$YYYY,All_year1$MM ,All_year1$DD, sep = "." )  , format = "%Y.%m.%d" )

####remove YYYY,MM,DD,hh columns
#### convert the ATMP and WTMP columns from character class to numberic class
#### change the nonsense data such as temperature = 999.0 or 99.0 Celsius to NA 
All_year2 <- All_year1 %>%
  select(-YYYY,-MM,-DD,-hh) %>%
  select(Date, everything())%>%
  mutate(ATMP, ATMP=as.numeric(ATMP)) %>% 
  mutate(WTMP, WTMP=as.numeric(WTMP)) %>% 
  mutate(ATMP = replace(ATMP, ATMP==999.0, NA))%>% 
  mutate(WTMP = replace(WTMP, WTMP==999.0, NA))%>% 
  mutate(ATMP = replace(ATMP, ATMP==99.0, NA))%>% 
  mutate(WTMP = replace(WTMP, WTMP==99.0, NA))

#### insert NA to those missing time data in order to create a time continues shiny app.

ts <- seq.POSIXt(as.POSIXct("1985-09-14",'%y/%m/%d'), as.POSIXct("2017-12-31",'%y/%m/%d'), by="day")
ts2 <-gsub(" GMT", "",ts)
df2 <- data.frame(Date=ts2)
df2$Date <- as.Date(df2$Date)
All_year2.5 <- full_join(df2,All_year2)

#### Convert cleaned data to tidy data and do Timeseries plot.

All_year3 <- All_year2.5 %>% gather(key ="Type",value = "Temperature",-Date)

ggplot(data = All_year3, aes(Date, Temperature, color = Type),na.rm = T) +geom_line()+ ggtitle("Air and Water Temperature changes between year 1985 and 2017\nBuoy Station 46035")

write.csv(All_year3, file = "All_year3.csv")

#### Using cleaned but not tidy data to conduct T-test in order to be able to filter out specific years from the data

# replacing 99 values with NA
All_yearttest <- All_year1 %>%
  mutate(ATMP, ATMP=as.numeric(ATMP)) %>% 
  mutate(WTMP, WTMP=as.numeric(WTMP)) %>% 
  mutate(ATMP = replace(ATMP, ATMP==999.0, NA))%>% 
  mutate(WTMP = replace(WTMP, WTMP==999.0, NA))%>% 
  mutate(ATMP = replace(ATMP, ATMP==99.0, NA))%>% 
  mutate(WTMP = replace(WTMP, WTMP==99.0, NA))

# extracting air temps for 1986 and 2017 as vectors
atmp1986 <- All_yearttest %>% filter(YYYY == 1986) %>% pull(ATMP)
atmp2017 <- All_yearttest %>% filter(YYYY == 2017) %>% pull(ATMP)

atmp1986 <- na.omit(atmp1986)
atmp2017 <- na.omit(atmp2017)
# T-test for air temp
var.test(atmp1986, atmp2017)
t.test(atmp1986, atmp2017, var.equal = TRUE)

# extracting sea temps for 1986 and 2017 as vectors 
wtmp1986 <- All_yearttest %>% filter(YYYY == 1986) %>% pull(WTMP)
wtmp2017 <- All_yearttest %>% filter(YYYY == 2017) %>% pull(WTMP)

wtmp1986 <- na.omit(wtmp1986)
wtmp2017 <- na.omit(wtmp2017)
# T-test for sea temp
var.test(wtmp1986, wtmp2017)
t.test(wtmp1986, wtmp2017, var.equal = FALSE)
