library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(data.table)
library(gridExtra)

veg.1 <- read_xlsx("veg1.xlsx")

cnames.1 <- colnames(veg.1)

cnames.1 <- colnames(veg.1)

## try
n_distinct(veg.1[,1])
# 
n_distinct(veg.1[,2])
# 
unique(veg.1[,2])

## now get the count for each column

c <- apply(veg.1, 2, n_distinct)
c


c[c>1]


d <- names(c[c==1])
d

e <- names(c[c>1])
e


veg.2 <- select(veg.1, e)

cnames.2 <- colnames(veg.2)
cnames.2

apply(veg.2, 2, n_distinct)

veg.3 <- dplyr::rename(veg.2, 
                       Geo = `Geo Level`, 
                       State = `State ANSI`,
                       Data = `Data Item`,
                       Category = `Domain Category`)

cnames.3 <- colnames(veg.3)
cnames.3

# veg.3
# 
unique(veg.3[,"Commodity"])
# 
unique(veg.3[,"Data"]) %>% print(n=60)
# 
unique(veg.3[,"Domain"])
# 
unique(veg.3[,"Category"])
# 
unique(veg.3[,"Value"])


 

              
yy <- separate(veg.3, Category, into = c("label", "quant"), sep=",")
# 
n_distinct(yy[,2])
# 
# 
S <- unique(yy[,"label"]) %>% print(n=30)

#S <- unique(yy[,"Commodity"]) %>% print(n=30)

#filter out the restricted use chemical#
ru <- filter(yy, label=="RESTRICTED USE CHEMICAL")

#Separating the quant to get the chemical Name 
ru1 <- ru %>% select(label, quant) %>% 
  unique() %>% 
  separate(quant,c('type','Type','Name'))

##removing the column not needed##
ru2 <- ru1[,-2]

##printing the first 30##
ru2 %>% print(n=30)

## get CAS #
## find info at https://cfpub.epa.gov/ecotox/  (go to beta)
## or
## https://comptox.epa.gov/dashboard

CAS <- read.csv("chemicalNames.csv",stringsAsFactors = FALSE)
CAS1 <- select(CAS,c('Name','TYPE','CAS','STUDY_TYPE','SPECIES','VALUES','UNITS'))

#converting Name column to character from factor#
CAS1$Name <- as.character(as.factor(CAS1$Name))

#separating the label for all vegies with restricted use chemical#
ru3 <- ru %>% select(Year,Commodity,Data,Region,Geo,label, quant,Value) %>% 
  separate(quant,c('type','Type','Name'))
ru4 <- ru3[-7] %>% 
  unique()

#Renaming the chemical names that changed while separating the quant.
ru5 <- ru4 %>% mutate(Name = ifelse(Name == "DIAZINON", "DIAZINON5", Name),
                      Name = ifelse(Name == "EMAMECTIN", "EMAMECTINBENZOATE", Name),
                      Name = ifelse(Name == "ZETA", "ZETACYPERMETHRIN", Name),
                      Name = ifelse(Name == "OXYDEMETON", "OXYDEMETONMETHYL", Name),
                      Name = ifelse(Name == "LAMBDA", "LAMBDACYHALOTHRIN", Name),
                      Name = ifelse(Name == "GAMMA", "GAMMACYHALOTHRIN", Name))


#Grouping the commodity and the chemicals prior to joining to get the LD50#
Merge3 <- ru5 %>% 
  select(Year,Name,Commodity,Value,Data) %>% 
  group_by(Name) %>% 
  arrange(desc(Name))

#Joining the grouped data with chemicals to get the LD50#
Merge4 <- inner_join(CAS1, Merge3, by.CAS1 = 'Name', by.Merge3 = 'Name') %>% 
  select(Year,Name,Commodity,VALUES,Value,Data)

#Extracting just the APPLICATIONS, MEASURED IN LB data for the analysis and scatterplotting
LBmerge4 <- filter(Merge4,Data %like% "APPLICATIONS, MEASURED IN LB$") %>% 
  filter(Year == 2016) %>% 
  group_by(Name,Commodity) %>% 
  arrange(desc(Name,Commodity))
#2014
LBmerge5 <- filter(Merge4,Data %like% "APPLICATIONS, MEASURED IN LB$") %>% 
  filter(Year == 2014) %>% 
  group_by(Name,Commodity) %>% 
  arrange(desc(Name,Commodity))
#2010
LBmerge6 <-filter(Merge4,Data %like% "APPLICATIONS, MEASURED IN LB$") %>% 
  filter(Year == 2010) %>% 
  group_by(Name,Commodity) %>% 
  arrange(desc(Name,Commodity))
#2006
LBmerge7 <-filter(Merge4,Data %like% "APPLICATIONS, MEASURED IN LB$") %>% 
  filter(Year == 2006) %>% 
  group_by(Name,Commodity) %>% 
  arrange(desc(Name,Commodity))

#Some of the application Values were withheld, so we had to Remove such with (D)/Withheld to avoid disclosing data for individual operations and 
#(Z)/Less than half the rounding unit/2016
x <- LBmerge4[!LBmerge4$Value == "(D)", ]
x <- x[!x$Value == "(Z)", ]
#2014
y <- LBmerge5[!LBmerge5$Value == "(D)", ]
y <- y[!y$Value == "(Z)", ]
#2010
z <- LBmerge6[!LBmerge6$Value == "(D)", ]
z <- z[!z$Value == "(Z)", ]
#2006
w <- LBmerge7[!LBmerge7$Value == "(D)", ]
w <- w[!w$Value == "(Z)", ]

#converting the Values column to numeric 2016
mydata <- within(x, { 
Value <- as.numeric(as.character(Value))})
#2014
mydata2 <- within(y, { 
  Value <- as.numeric(as.character(Value))})
#2010
mydata3 <- within(z, { 
  Value <- as.numeric(as.character(Value))})
#2006
mydata4 <- within(w, { 
  Value <- as.numeric(as.character(Value))})

#converting the values column to kg.2016
mydata <- mydata %>%
  mutate(ValueVeg = Value * 0.453592) %>% 
  mutate(safe1 = ifelse(ValueVeg > VALUES, "no", "yes"))
#2014
mydata2 <- mydata2 %>%
  mutate(ValueVeg = Value * 0.453592) %>% 
  mutate(safe1 = ifelse(ValueVeg > VALUES, "no", "yes"))
#2010
mydata3 <- mydata3 %>%
  mutate(ValueVeg = Value * 0.453592) %>% 
  mutate(safe1 = ifelse(ValueVeg > VALUES, "no", "yes"))
#2006
mydata4 <- mydata4 %>%
  mutate(ValueVeg = Value * 0.453592) %>% 
  mutate(safe1 = ifelse(ValueVeg > VALUES, "no", "yes"))

#Plotting the chemicals against the values 2016
p <- ggplot(mydata,aes(ValueVeg, Name, color = safe1))+
  geom_point() +
  facet_wrap(~Commodity)
p
#Plot for 2014
g <- ggplot(mydata2,aes(ValueVeg, Name, color = safe1))+
  geom_point() +
  facet_wrap(~Commodity)
g

#Plot for 2010
h <- ggplot(mydata3,aes(ValueVeg, Name, color = safe1))+
  geom_point() +
  facet_wrap(~Commodity)
h

#Plot for 2006
k <- ggplot(mydata4,aes(ValueVeg, Name, color = safe1))+
  geom_point() +
  facet_wrap(~Commodity)
k

#
j <- grid.arrange(p, g, h, k,nrow = 4)
j

##Exporting the clean data to csv##
write.csv(ru5, file = "CleanedVegData.csv")
write.csv(mydata, file = "MyData.csv")
write.csv(mydata2, file = "mydata2.csv")
write.csv(mydata3, file = "mydata3.csv")
write.csv(mydata4, file = "mydata4.csv")

#Function attempt
myfunction <- function(Y){
  filter(Merge4,Data %like% "APPLICATIONS, MEASURED IN LB$") %>% 
    filter(Year == Y) %>% 
    group_by(Name,Commodity) %>% 
    arrange(desc(Name,Commodity))
}

#Function2 for converting the value column to numeric
myfunction2 <- function(a){
  within(a, { 
    Value <- as.numeric(as.character(Value))})}
