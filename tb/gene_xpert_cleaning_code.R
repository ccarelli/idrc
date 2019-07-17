# ----------------------------------------------
# Quarterly cleaning code for the GeneXpert Data
# 7/10/2019
# Jaffer Okiring, Caitlin O'Brien-Carelli
# ----------------------------------------------

#
# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(stringr) 
library(plyr)
library(RColorBrewer)
library(readxl) # this library loads the data 
library(stringi) # this creates a data from the file name
library(Hmisc)
library(tools)
# --------------------


#-------------------------------
# import the files 

# set the working directory 
inDir = "C:/Users/Jaffer/Desktop/PCE/TB/Data/GeneXpert/Tb_raw_data/"

# set the working directory
setwd(inDir)

# set output directory
outDir = paste0("C:/Users/Jaffer/Desktop/PCE/TB/Data/GeneXpert/Tb_prepped/")

# create a vector of the file name
files = list.files(inDir)

# print the list of files to the console
files

# ----------------------
# read in the data 

# set the index
# the index keeps track of which iteration of the loop the code is on
i = 1

for (f in files) {
  
# import the data and make it into a data table 
dt = data.table(read.csv(paste0(inDir, f), stringsAsFactors = F))

# create a variable for the file name
dt[ , file_name:=as.character(f)]

# create a vector of the old variable names
old_names_data = names(dt)

new_names_data = c("site_id", "gene_xpert_site", "machines", "district", "region",
            "impl_partner", "samples_tested", "tb_pos_rif_neg", "rif_resist",
            "rif_indet", "total_errors", "average_util",
            "error_rate", "file_name")    

setnames(dt, old_names_data, new_names_data)

print(as.character(f))
print(old_names_data)

# drop site id as it may not be consistent by file
dt[ ,site_id:=NULL]

# clean the data so that it can be analyzed
# ----------------------
# create a date variable using the file name

dt[ ,quarter:=unlist(lapply(strsplit(f, " "), "[", 2))]
dt[ ,year:=unlist(lapply(strsplit(f, " "), "[", 3))]
dt[ ,year:=unlist(lapply(strsplit(year, "\\."), "[", 1))]

# set the date for the start of the quarter
dt[quarter==1, date:=paste0(year, '-01-01')]
dt[quarter==2, date:=paste0(year, '-04-01')]
dt[quarter==3, date:=paste0(year, '-07-01')]
dt[quarter==4, date:=paste0(year, '-10-01')]

# convert type of date
dt[ , date:=as.Date(date)]

# ---------------------------------------
# fix region and implementing partner names with distinct capitalization
# formatting for figures and tables

# -------------------------
# geographic areas

# format names of regions
#dt[,region:=capitalize(tolower(region))]
dt[region=='Fortportal', region:='Fort Portal']
dt[region=='Karamoja', region:='Moroto']
dt[ , region:=trimws(region)]

dt[district=='Kayunga', region:='Kampala Central'] # one facility in Kayunga is missing a region
dt[gene_xpert_site=='Dokolo HC IV',district:='Dokolo']
dt[gene_xpert_site=='Dokolo HC IV',region:='Lira']
#Butenga HC IV
dt[gene_xpert_site=='Butenga HC IV',district:='Bukomansimbi']
dt[gene_xpert_site=='Butenga HC IV',region:='Masaka']
#RHSP


# format the names of districts
# drop the words district and hospital, fix capitalization, trim blanks
dt[ , district:=gsub('District', "", district)]
dt[ , district:=gsub('Hospital', "", district)]
dt[ , district:=toTitleCase(tolower(district))]
dt[ , district:=str_trim(district, side="both")]

# -------------------------
# format the names of implementing partners
dt[ ,impl_partner:=gsub("DEFEAT", "Defeat", impl_partner)]
dt[ ,impl_partner:=gsub("ugcare", "Uganda Cares", impl_partner)]
dt[ ,impl_partner:=gsub("ugcare", "Uganda Cares", impl_partner)]
dt[impl_partner=="Defeat TB/ HIWA (World Vision)", impl_partner:="Defeat TB/HIWA (World Vision)" ]
dt[impl_partner=="RHSP/HIWA (World vision)", impl_partner:="RHSP/HIWA (World Vision)"]
dt[impl_partner=='BAYLOR', impl_partner:='Baylor']
dt[impl_partner=="CDC SOROTI PROJECT", impl_partner:='CDC Soroti Project']
dt[impl_partner=='No IP', impl_partner:='No partner']
dt[gene_xpert_site=='Butenga HC IV',impl_partner:='RHSP']

# -------------------------
# format facility names and add facility level

# drop out the (machines-1) designation from the facility names
dt[ , gene_xpert_site:=unlist(lapply(strsplit(gene_xpert_site, "\\("), "[", 1))]
dt[ , gene_xpert_site:=trimws(gene_xpert_site)]

# some of the hospital names are abbreviated or in all caps
dt[!grep("Hospital", gene_xpert_site) , gene_xpert_site:=gsub("Hosp", "Hospital", gene_xpert_site)]
dt[ , gene_xpert_site:=gsub("HOSPITAL", "Hospital", gene_xpert_site)]

# fix the names of sites that contain typos
dt[grep("Dzaipi", gene_xpert_site), gene_xpert_site:='Dzaipi HC III']
dt[gene_xpert_site=='Bugono HICV', level:='HC IV']
dt[gene_xpert_site=='Mbarara RRH', level:='Hospital']
dt[gene_xpert_site=='Yumbe GH', level:='Hospital']

# use facility names to determine the facility level
dt[grep("Hospital", gene_xpert_site), level:='Hospital']
dt[grep("II", gene_xpert_site), level:='HC II']
dt[grep("III", gene_xpert_site), level:='HC III']
dt[grep("IV", gene_xpert_site), level:='HC IV']


# print the percentage of facilities for which level cannot be determined
total_sites = dt[ ,length(unique(gene_xpert_site))]
print(paste0(dt[is.na(level), .(round(length(unique(gene_xpert_site))/total_sites, 1))],
             '% of total sites are missing the facility level.'))


# drop the totals from the end of the file
# all of the totals are missing the machines variable or include the word total
dt[ ,test:=tolower(gene_xpert_site)]
dt = dt[!is.na(machines)]
dt = dt[!grepl("total", test)]

# --------------------
# convert variable types

dt[ , tb_pos_rif_neg:=as.numeric(tb_pos_rif_neg)]
dt[ , rif_resist:=as.numeric(rif_resist)]
dt[ , average_util:=as.numeric(average_util)]
dt[ , error_rate:=as.numeric(error_rate)]
dt[ , rif_indet:=as.numeric(rif_indet)]

# facilities that are missing district or region
facilities = c("Joint Clinic Research Center", "Dr. Charles Fathong Hospital", "Kasese Municipal Council HC III",
"Uganda Cares HCIII-Lukaya", "Lwengo HC IV")

dt[region=="", region:=NA]
dt[district=="", district:=NA]

# # --------------------
# append the current excel sheet to the full data
# we are adding each cleaned sheet to one, single data set
# when the loop is complete, the data set will be called 'full_data'

if(i==1) full_data = dt
if(i>1) full_data = rbind(full_data, dt)
i = i+1

# --------------------
 }

# --------------------------------------------------
# examine the full data set! yay!

# look at the structure of the data 
str(full_data)

# look at the first six lines of the full data
head(full_data)

# calculate total positive for tb
full_data[ , tb_positive:=(tb_pos_rif_neg + rif_resist)]

full_data=full_data[!is.na(gene_xpert_site)]
# ----------------------
# save the prepped file - it is ready for analysis! 

saveRDS(full_data, paste0(outDir, 'clean_genexpert_data.rds'))

# ----------------------



