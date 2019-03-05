# clear memory
rm(list=ls())

library(reshape2)
library(ggplot2)
library(tidyr)
library(stats4)
library(caret)
library(dplyr)
library(tidyverse)
library(data.table)
library(tibble)
library(RColorBrewer)



##-disable scientific notation
options(scipen=999)


setwd("E:/HIV/hiv_new")

# read data 
h15 <- read.csv("HIV 1 2015.csv")
h16 <- read.csv("HIV 1 2016.csv")
h17 <- read.csv("HIV 1 2017.csv")
h18 <- read.csv("HIV 1 2018.csv")


a56<-merge(h15,h16,by="organisationunitname")
a567<-merge(a56,h17,by="organisationunitname")
a5678<-merge(a567,h18,by="organisationunitname")

hiv<-data.table(a5678)
########reshape long

mart<-melt(hiv,by.vars="organisationunitname")

# identify outliers in the original data set in opd confirmed cases 
mart[ ,unique(variable)]

###need to split variable for merging rasta files
mart$district<- sapply(strsplit(as.character(mart$organisationunitname),' '), "[", 1)

# add the year
mart[grep(pattern='2015', variable), year:=2015 ]
mart[grep(pattern='2016', variable), year:=2016 ]
mart[grep(pattern='2017', variable), year:=2017 ]
mart[grep(pattern='2018', variable), year:=2018 ]

# month variable
mart[ , variable:=tolower(variable)]

mart[grep(pattern='jan', variable), mth:=1 ]
mart[grep(pattern='feb', variable), mth:=2 ]
mart[grep(pattern='mar', variable), mth:=3 ]
mart[grep(pattern='apr', variable), mth:=4 ]
mart[grep(pattern='may', variable), mth:=5]
mart[grep(pattern='jun', variable), mth:=6 ]
mart[grep(pattern='jul', variable), mth:=7 ]
mart[grep(pattern='aug', variable), mth:=8 ]
mart[grep(pattern='sep', variable), mth:=9 ]
mart[grep(pattern='oct', variable), mth:=10 ]
mart[grep(pattern='nov', variable), mth:=11 ]
mart[grep(pattern='dec', variable), mth:=12 ]


# use month and year to create a date variable
mart[ , date:=as.Date(paste(year, mth, '01', sep='-'), '%Y-%m-%d')]

## the indicator variable
mart[grep(pattern='hiv..pregnant.women.initiated.on.art.for.emtc', variable),
     indicator:='Preg.initiated.on.art.for.emtc']
mart[grep(pattern='preg.women.newly.tested.tr...trr', variable),
     indicator:='preg.newly.tested.tr.trr']

mart[grep(pattern='hiv..pregnant.women.on.art.bfre.1visit', variable),
     indicator:='preg.on.art.bfre.1visit']

mart[grep(pattern='total.hiv.exposed.serologicalrapid.test', variable),
     indicator:='tot.serologicalrapid.test']

mart[grep(pattern='stock.balance.hiv.screening.test.kits', variable), indicator:='stock.balance.hiv.screening.test.kits']
mart[grep(pattern='h6individuals.tested.hiv.positive', variable),
     indicator:='tested.hiv.positive']

mart[grep(pattern='h7hiv...presumptive.tb', variable),
     indicator:='presumptive.tb']

mart[grep(pattern='tested.._lt18mth.1st.pcr', variable),
     indicator:='lt18mth.1st.pcr']
#smc.ct.and.circumcised.at.site
mart[grep(pattern='smc.ct.and.circumcised.at.site', variable),
    indicator:='smc.ct.and.circumcised.at.site']
#hiv.exposed.infants.started.on.cpt.tota
mart[grep(pattern='hiv.exposed.infants.started.on.cpt.tota', variable),
     indicator:='hiv.exp.infants.started.on.cpt']
#hiv.exposed.infants.enrolled.in.care
mart[grep(pattern='hiv.exposed.infants.enrolled.in.care', variable),
     indicator:='hiv.exp.infants.enrolled.in.care']

#deliveries.to.hiv..women.in.unit.total
mart[grep(pattern='deliveries.to.hiv..women.in.unit.total', variable),
     indicator:='deliveries.to.hiv.women']

#hiv.exposed.babies.given.arvs
mart[grep(pattern='hiv.exposed.babies.given.arvs', variable),
     indicator:='exp.babies.given.arvs']



totmart<-mart[ ,.(tot=sum(value, na.rm=T)),
               by=c("indicator","date")]


pdf("D:/hiv/figure_desc.pdf", width=9, height=6)

ggplot(totmart[indicator!='NA'],aes(x=date, y=tot)) +
  geom_point() +
  geom_line() +
  labs(title='', subtitle='2015-2018',
       x='Date', y='Count', caption='Source: HMIS', color='Indicators') +
  theme_bw()+
  facet_wrap(~indicator,scales = "free")
dev.off()



