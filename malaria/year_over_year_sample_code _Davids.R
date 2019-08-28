# ---------------------------------------------------------------------------------------------
# David Phillips
# 
# 8/22/2019
# Sample code to help with a "year-over-year" analysis of the recent malaria upsurge in Uganda
# 
#	Note: this assumes the data are formatted with four columns:
#	district (character) - name of district
#	year (number) - year in which cases occurred (YYYY)
#	month (number) - month in which cases occurred (MM)
#	number_of_cases (number) - number of cases of malaria in this district-year-month
# ---------------------------------------------------------------------------------------------


# ------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(lubridate)
# ------------------
options(scipen=999)

# --------------------------------------------------------------------
# Files and directories

# root directory CHANGE THIS TO THE LOCATION OF YOUR DATA
dir = "D:/PCE/Malaria/"

# name of input data file CHANGE THIS TO THE NAME OF YOUR DATA FILE
inFile = paste0(dir, 'mal_wep_monthly.csv')

# output file
outFile = paste0('D:/PCE/Malaria_outputs/', 'year_over_year_cases.pdf')
# --------------------------------------------------------------------


# --------------------------------------------------------------------------
# Load/prep data

# load data
data = fread(inFile)

data[ ,year:=paste0(201,unlist(lapply(strsplit(as.character(periodid), ""), "[" ,4)))]
data[ ,mth:=paste0(unlist(lapply(strsplit(as.character(periodid), ""), "[" ,5)),
                   unlist(lapply(strsplit(as.character(periodid), ""), "[" ,6)))]

# format month and year like a date (assign to 15th day of month)
data[ , date:=paste0(year, '-', mth, '-15')]
data[ , date:=ymd(date)]

data[ ,district:=unlist(lapply(strsplit(organisationunitname, " "), "[" ,1))]

data[,c("organisationunitid", "periodcode","organisationunitcode","organisationunitdescription","periodname",
        "perioddescription","organisationunitname","periodid"):=NULL]

new_col=c("microscopy_treated_neg","microscopy_trested_pos","microscopy_positive","microspcopy_tested","rdt_positive",
          "rdt_tested","suspected_fever_cases","rdt_treated_neg","rdt_treated_pos","treated_not_tested",
          "year","mth","date","district")

setnames(data,new_col)

data[,number_of_cases:= (microscopy_positive +rdt_positive) ]
# ensure the data are sorted
data = data[order(district, year, mth)]



# make a national total
national = data[, .(number_of_cases=sum(number_of_cases)), by=c('year','mth','date')]

# compute "year over year", i.e. difference from last year in the same month
data<-data[date>='2018-01-15']
data[, yoy:=number_of_cases-shift(number_of_cases, 12), by='district']

# compute "year over year" for national numbers too
national<-national[date>='2018-01-15']
national[, yoy:=number_of_cases-shift(number_of_cases, 12)]

# --------------------------------------------------------------------------


# --------------------------------------------------------------------------
# Graph year-over-year at national and district level

# national level
p1 = ggplot(national, aes(y=yoy, x=date)) + 
	geom_point() + 
	geom_line() + 
	geom_hline(yintercept=0) + 
	labs(title='Year-Over-Year Malaria Cases - National Level', 
		subtitle='(Difference from Previous Year in the Same Month)', 
		y='Change in Malaria Cases from Previous Year', x='') + 
	theme_bw()

# district level
plots = lapply(unique(data$district), function(d) {
	ggplot(data[district==d], aes(y=yoy, x=date)) + 
		geom_point() + 
		geom_line() + 
		geom_hline(yintercept=0) + 
		labs(title=paste('Year-Over-Year Malaria Cases -', d, 'District'), 
			subtitle='(Difference from Previous Year in the Same Month)', 
			y='Change in Malaria Cases from Previous Year', x='') + 
		theme_bw()
})
# --------------------------------------------------------------------------


# ---------------------------------------------
# Save graphs
pdf(outFile, height=6, width=8)
p1
for(i in seq(length(plots))) print(plots[[i]])
dev.off()
# ---------------------------------------------
