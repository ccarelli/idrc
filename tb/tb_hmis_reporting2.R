# Quantify the missingness in the data
# Answer the question: how much bias is due to incomplete reporting?
# 4/7/2019

#-------------------------------
# set up R

rm(list=ls())
  library(reshape2)
  library(ggplot2)
  library(foreign)
  library(tidyr)
  library(stats4)
  library(caret)
  library(dplyr)
  library(tidyverse)
  library(data.table)
  library(RColorBrewer)
  library(lubridate)
  
#-disable scientific notation
options(scipen=999)

#-------------------------------
# set the working directory
setwd("C:/Users/Jaffer/Desktop/PCE/TB/Data/hmis_prepped")

# import the prepped data 
tb = readRDS("tb_hmis_facility_counts_prepped.rds")

# peek at the data to make sure it is ok
str(tb)

# output directory for graphs
outDir = "C:/Users/Jaffer/Desktop/PCE/TB/Data/hmis_outputs"
#-------------------------------
# analyze reporting completeness for all variables
  
# quantify the missingness

# count the facilities in the entire data set
tb[ ,.(facilities=length(unique(org_unit_id))), by=.(date)]

#-------------------------------
# reporting completeness nationally for all variables

# analyze reporting completeness
# how many facilities reported in that month?
report = tb[!is.na(value) ,.(facilities=length(unique(org_unit_id))), by=.(date)]
report
#-mean reap
fac_report = tb[!is.na(value) ,.(facilities=length(unique(org_unit_id))), by=.(date, facility_level)]
#-------------------------------
# reporting completeness

# how many facilities reported every month?
# when should we start looking at the data?
ggplot(report, aes(x=date, y=facilities))+
  geom_point()+
  geom_line()+
  theme_bw() +
  labs(title='Number of facilities reporting for all TB variables',
       x='Date', y='Number of facilities reporting') +
  theme(text=element_text(size=18))

ggplot(fac_report, aes(x=date, y=facilities))+
  geom_point()+
  geom_line()+
  theme_bw() +
  facet_wrap(~facility_level, scales="free")
  labs(title='Number of facilities reporting for all TB variables',
       x='Date', y='Number of facilities reporting')+
  theme(text=element_text(size=18))


  ggplot(fac_report, aes(x=date, y=facilities,color=facility_level))+
    geom_point()+
    geom_line()+
    theme_bw() +
    #facet_wrap(~facility_level,scales="free")
  labs(title='Number of facilities reporting by health facility_level',
       x='Date', y='Number of facilities reporting') +
    theme(text=element_text(size=18))
  
  ggplot(fac_report[date>'2015-07-01' & date<'2019-04-01'], aes(x=date, y=facilities,color=facility_level))+
    geom_point()+
    geom_line()+
    theme_bw() +
    #facet_wrap(~facility_level,scales="free")
    labs(title='Number of facilities reporting by health facility_level',
         x='Date', y='Number of facilities reporting') +
    theme(text=element_text(size=18))
  
#----------------------------------------
# reporting by variable/indicator 

# but what does reporting look like for individual indicators?
# analyze reporting completeness
# how many facilities reported in that month?
report2 = tb[!is.na(value) ,.(facilities=length(unique(org_unit_id))), by=.(date, variable)]
report2

# reporting with color for variable
ggplot(report2, aes(x=date, y=facilities, color=variable))+
  geom_point(alpha=0.2)+
  geom_line(alpha=0.6)+
  theme_bw()

# facet wrap to see overlapping variables
ggplot(report2[date>'2015-07-01' & date<'2019-04-01'], aes(x=date, y=facilities, color=variable))+
  geom_point()+
  facet_wrap(~variable, scales='free_y')+
  geom_line()+
  theme_bw() +
  labs(title = 'Number of facilities reporting by variable', 
       x='Date', y="Number of facilities reporting", color='Variable')

#----------------------------------------
# reporting by facility level and variable

# facilities reporting by level and variable
fac_report3 = tb[!is.na(value) ,.(facilities=length(unique(org_unit_id))),
        by=.(date, variable, facility_level)]

ggplot(fac_report3, aes(x=date, y=facilities, color=facility_level)) +
  geom_point()+
  geom_line()+
  facet_wrap(~variable)+
  labs(title='Reporting by variable and health facility level',
       x='Date', y='Number of facilities reporting')+ theme_bw()

ggplot(fac_report3, aes(x=date, y=facilities, color=variable)) +
  geom_point()+
  geom_line()+
  facet_wrap(~facility_level)+
  theme_bw()+
  labs(title='Reporting by variable and health facility level',
       x='Date', y='Number of facilities reporting')

#----------------------------------------
# reporting by health facility level
fac_report2 = tb[!is.na(value) ,.(facilities=length(unique(org_unit_id))),
                     by=.(date, facility_level)]

ggplot(fac_report2, aes(x=date, y=facilities, color=facility_level)) +
  geom_point()+
  geom_line()+
  theme_bw()+
  labs(title='Reporting by health facility level',
       x='Date', y='Number of facilities reporting')

#----------------------------------------
# reporting completeness by district

# number of facilities that report for each district for each month
report_dist = tb[!is.na(value) ,.(facilities=length(unique(org_unit_id))),
                 by=.(date, district)]
report_dist = report_dist[order(district)]

# create a pdf of every district's reporting completeness
list_of_plots = NULL
i = 1

for (d in unique(report_dist$district)) {
  
  district_name = as.character(d)  
  
  list_of_plots[[i]] = ggplot(report_dist[district==d], aes(x=date, y=facilities))+
    geom_point()+
    geom_line()+
    theme_bw() +
    labs(title=paste0("Reporting completeness: ", district_name),
         x="Date", y="Facilities reporting")
  
  i = i+1
}


# print the pdf - one page for each district
pdf(paste0(outDir, "/tb_district_completeness.pdf"))

for(i in seq(length(list_of_plots))){
  print(list_of_plots[[i]])
}
dev.off()

#---------------------------------------

#----------------------------------------
# reporting completeness by district and health facility level

# number of facilities that report for each district for each month
report_dist_level = tb[!is.na(value) ,.(facilities=length(unique(org_unit_id))),
                      by=.(date, district, facility_level)]

# create a pdf of every district's reporting completeness
level_list_of_plots = NULL
i = 1

for (d in unique(report_dist$district)) {
  
  district_name = as.character(d)  
  
  level_list_of_plots[[i]] = ggplot(report_dist_level[district==d], aes(x=date, y=facilities, color=facility_level))+
    geom_point()+
    geom_line()+
    theme_bw() +
    labs(title=paste0("Reporting completeness: ", district_name),
         x="Date", y="Facilities reporting", color='Facility level')
  
  i = i+1
}


# print the pdf - one page for each district
pdf(paste0(outDir, "/tb_district_completeness_facility_level.pdf"))

for(i in seq(length(level_list_of_plots))){
  print(level_list_of_plots[[i]])
}
dev.off()

#---------------------------------------
# number of bacteriologically confirmed cases per facility

cases = tb_long[variable=='ptb_bact_conf_cases', .(value=sum(value, na.rm=T)), by=date]
cases = merge(cases, report, by='date', all=T)
cases[ ,mean_cases:=value/facilities]
setnames(cases, 'value', 'conf_cases')

# number of cases per facility per month
ggplot(cases, aes(x=date, y=mean_cases))+
  geom_point()+
  geom_line()+
  theme_bw()

cases_long = melt(cases, id.vars='date')
cases_long = cases_long[2015 < year(date) & year(date) < 2019 ]

cases_long$variable = factor(cases_long$variable, c('facilities', 'conf_cases', 'mean_cases'),
       c('Facilities reporting', 'Confirmed cases', 'Mean cases per facility'))

ggplot(cases_long, aes(x=date, y=value, color=variable))+
  geom_point()+
  geom_line()+
  facet_wrap(~variable, scales='free_y')+
  theme_bw() +
  labs(x='Date', y="", color="")

#---------------------------------------
# table of facilities reporting

tb[ , year:=year(date)]
period = tb[year==2017 | year==2018]
period = period[!is.na(total_notified_tb) ,.(facilities=length(unique(org_unit_id))), by=.(year, facility_level)]
period = period[order(year, facilities)]
period = dcast(period, facility_level~year)
write.csv(period, "table_columns.csv")

tb[!is.na() , length(unique(org_unit_id)), by=year]

#-----------------------------------------------
# print a national trend for every variable

variables = unique(tb$variable)

new = tb[date>'2015-07-01' & date<'2019-04-01',.(value=sum(value, na.rm=T)), by=.(variable, date)]
new[ ,variable:=as.character(variable)]

var_list_of_plots=NULL
i = 1
for (v in variables) {
  
  var = as.character(v)
  
  var_list_of_plots[[i]] <- ggplot(new[variable==v], aes(x=date, y=value))+
    geom_point()+
    geom_line()+
    labs(title=var, x='Date', y='Count')+
    theme_bw()
  
  i=i+1
  
}


# print the pdf - one page for each district
pdf(paste0(outDir, "/tb_national_trends_all_vars.pdf"))

for(i in seq(length(var_list_of_plots))){
  print(var_list_of_plots[[i]])
}
dev.off()

