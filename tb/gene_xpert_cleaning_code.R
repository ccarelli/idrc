# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 4/25/2019

# ----------------------------------------------

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
# --------------------

#------------------------------------------------
# to code in the same code, we need to set file pathways for each user
# change the username to your username to code
# for example: user = 'copio'

user = 'ccarelli'

# -----------------------------------------------
# place all the tb files in a single folder in order to import the folder contents

# change to the folder on your computer where all of the TB data are saved
if (user=='ccarelli') inDir = "C:/Users/ccarelli/Documents/tb/"

# create a folder for outputs, including figures and cleaned data sets as RDS files
if (user=='ccarelli') outDir = "C:/Users/ccarelli/Documents/tb_outputs/"

# create a vector of the file name
f = list.files(inDir)

# ----------------------
# read in the data 

# to print the sheets in the excel file 
excel_sheets(paste0(inDir, 'GenXp 1st.7th.4.2019.xls'))

# import the data and make it a data table 
# ignore the warnings! they just say that the first line has a date in it
dt = data.table(read_excel(paste0(inDir, 'GenXp 1st.7th.4.2019.xls'), sheet = 1, skip = 1))

# ----------------------
# format the columns to appear correctly

# drop out the extra column added by excel
dt[ , X__1:=NULL]


# list the names for the columns that you want to appear
new_column_names = c("genexpert_site", "district", "region", "impl_partner", "reported", 
                     "total_samples", "tb_positive", "rif_resist", "rif_indet", "total_errors",
                     "children_under_14", "retreatments", "percent_reporting_region", "status")

# reset the column (variable) names         
setnames(dt, new_column_names)

# check that the column names correspond to the correct columns
# the values in row 1 that are not missing should be the same as the variable names
# View(dt)

# delete the first row, since it contains column names and not values
dt = dt[-1, ]

# ----------------------
# subset to only the indivual values, not the aggregate tables at the bottom
# we will use these data, but first just the counts

# this code finds the areas where 3 columns are false 
dt[ ,index:=1:nrow(dt)]
drop = dt[ ,lapply(.SD, is.na), by=index, .SDcols=2:4]
drop = drop[district==T & region==T & impl_partner==T, min(index)]
dt = dt[index < drop]
dt[ ,index:=NULL]

# ----------------------
# clean the data so that it can be analyzed

# add a variable for the number of genexpert machines per site 
dt[grep('machines', genexpert_site), machines:=genexpert_site]
dt[ , machines:=unlist(lapply(strsplit(dt$machines, "-"), "[", 2))]
dt[!is.na(machines), machines:=gsub(")", "", machines)]
dt[ ,machines:=as.numeric(machines)]
dt[is.na(machines), machines:=1] # warning is ok - it thinks the values are words
dt[ ,machines:=as.numeric(machines)] # machines act as a string 

# ----------------------
# create a date variable using the file name

dt[ , day:=stri_extract_first_regex(f, "[0-9]+") ]
dt[ , month:=unlist(lapply(strsplit(f, "\\."), "[", 3))]
dt[ , year:=unlist(lapply(strsplit(f, "\\."), "[", 4))]
dt[ , date:=as.Date(paste0(month,'-', day, '-', year), format="%m-%d-%Y")]
dt[ ,c('month', 'day', 'year'):=NULL]

# ----------------------
# change reported to a logical
dt[ ,reported:=reported==1]

# ----------------------
# fix region and implementing partner names with distinct capitalization

# format names of regions
dt[region!='KCCA',region:=capitalize(tolower(region))]
dt[region=='Fortportal', region:='Fort Portal']
dt[district=='Kayunga', region:='Central'] # one facility in Kayunga is missing a region 
   
# format the names of implementing partners
dt[ ,impl_partner:=gsub("DEFEAT", "Defeat", impl_partner)]
dt[ ,impl_partner:=gsub("ugcare", "Uganda Cares", impl_partner)]
dt[ ,impl_partner:=gsub("ugcare", "Uganda Cares", impl_partner)]
dt[ impl_partner=="DEFEAT TB/ HIWA (World Vision)", impl_partner:="Defeat TB/HIWA (World Vision)" ]
dt[ impl_partner=="RHSP/HIWA (World vision)", impl_partner:="RHSP/HIWA (World Vision)"]
dt[impl_partner=='BAYLOR', impl_partner:='Baylor']
dt[impl_partner=="CDC SOROTI PROJECT", impl_partner:='CDC Soroti Project']
dt[impl_partner=='No IP', impl_partner:='No partner']

#----------------------
# format facility names and add facility level 

# drop out the (machines-1) designation from the facility names
dt[ , genexpert_site:=unlist(lapply(strsplit(genexpert_site, "\\("), "[", 1))]
dt[ , genexpert_site:=trimws(genexpert_site)]

# some of the hospital names are abbreviated or in all caps
dt[ , genexpert_site:=gsub("Hosp", "Hospital", genexpert_site)]
dt[ , genexpert_site:=gsub("HOSPITAL", "Hospital", genexpert_site)]

# fix the name of this site 
dt[genexpert_site=='Dzaipi     H/C 111 ', genexpert_site:='Dzaipi HC III']

# use facility names to determine the facility level
dt[grep("Hospital", genexpert_site), level:='Hospital']
dt[grep("II", genexpert_site), level:='HC II']
dt[grep("III", genexpert_site), level:='HC III']
dt[grep("IV", genexpert_site), level:='HC IV']

# fix the facilities with typos
dt[genexpert_site=='Bugono HICV', level:='HC IV']
dt[genexpert_site=='Mbarara RRH', level:='Hospital']
dt[genexpert_site=='Yumbe GH', level:='Hospital']

# print the percentage of facilities for which level cannot be determined
total_sites = dt[ ,length(unique(genexpert_site))]
print(paste0(dt[is.na(level), .(round(length(unique(genexpert_site))/total_sites, 1))],
             '% of total sites are missing the facility level.'))

# ----------------------
# convert character types

dt = dt[ , lapply(.SD, as.numeric), 
         by=c("genexpert_site", "level", "status", "district", "region", "impl_partner",
                                    "reported", "date"), .SDcols=c(6:12, 15)]

# ----------------------
# aesthetic properties 

bar_colors = c('GeneXpert Sites'='#9ecae1', 'Reported'='#fc9272')
alt_bar_colors = c('GeneXpert Sites'='#fee090', 'Reported'='#b2abd2')
true_false = c('#bd0026', '#3182bd')
single_red = '#bd0026'
single_blue = '#a6bddb'
turq = '#80cdc1'
ratios = c('#bd0026', '#3182bd', '#74c476', '#feb24c')
sex_colors = c('#bd0026', '#3182bd', '#74c476', '#8856a7')

graph_colors = c('#bd0026', '#fecc5c', '#74c476','#3182bd', '#8856a7')
tri_sex = c('#bd0026', '#74c476', '#3182bd')
wrap_colors = c('#3182bd', '#fecc5c', '#bd0026', '#74c476', '#8856a7', '#f768a1')



# --------------------------------------------------------------
# exploratory graphs

# pdf(paste0(outDir, 'tb_sample_figures.pdf'), height=9, width=12)

#------------------------------
# reporting by facilities

# calculate facilities reporting by region 
rep = dt[ ,.(reported=sum(reported), sites=length(unique(genexpert_site))), by=region]
reporting_rate = rep[ ,.(round(100*sum(reported)/sum(sites), 1))]

# bar graph of reporting compared to total sites 
ggplot(rep, aes(x=region, y=sites, fill='GeneXpert Sites', label=sites)) +
  geom_bar(stat="identity") +
  geom_bar(aes(y=rep$reported, fill='Reported'), stat="identity") + 
  geom_text(size = 4, position=position_stack(vjust = 1.06)) +
    scale_fill_manual(name='', values=bar_colors) + theme_minimal() +
    labs(x='Region', y='Total GeneXpert Sites', subtitle=paste0('Reporting rate: ', reporting_rate, '%'), 
         title="Number of GeneXpert sites reporting by region, April 1 - 7, 2019") +
theme(text = element_text(size=18), axis.text.x=element_text(size=12, angle=90))

# percent reporting
rep[ ,rate:=round(100*reported/sites, 1)]
rep[ ,all:=rate==100]

# percent of facilities reporting and reporting size
ggplot(rep, aes(x=region, y=rate, color=all, size=sites, label=sites)) +
  geom_point(alpha=0.5) +
  geom_text(size = 4, position=position_stack(vjust = 1.02)) +
  theme_bw() +
  ylim(80, NA) +
  scale_color_manual(values=true_false) +
  labs(title="Percent of GeneXpert reporting (%), April 1 - 7, 2019", y="Percent reporting (%)", x="Region", color="100% reported", 
       size="Number of sites", subtitle='Value label = total number of GeneXpert sites') +
  theme(text = element_text(size=14), axis.text.x=element_text(size=12, angle=90)) 

# facilities reporting by facility level 
rep_level = dt[ ,.(reported=sum(reported), sites=length(unique(genexpert_site))), by=level]

# calculate the sum of facilities that did not report
rep_level[ , no_report:=(sites-reported)]
no_reports = sum(rep_level$no_report)

# bar graph of reporting compared to total sites 
ggplot(rep_level, aes(x=level, y=sites, fill='GeneXpert Sites', label=sites)) +
  geom_bar(stat="identity") +
  geom_bar(aes(y=rep_level$reported, fill='Reported'), stat="identity") + 
  geom_text(size = 4, position=position_stack(vjust = 0.5)) +
  scale_fill_manual(name='', values=alt_bar_colors) + theme_minimal() +
  labs(x='Region', y='Total GeneXpert Sites', subtitle='Value = number of total sites*', 
       title="Number of GeneXpert sites reporting by health facility level",
        caption=paste0("*", no_reports, ' facilities did not report.' )) +
  theme(text = element_text(size=18), axis.text.x=element_text(size=12))

#------------------------------
# samples submitted by children under 14
dt[ ,.(total_samples = sum(total_samples), children = sum(children_under_14)), by=region]

#------------------------------
# count of machines by region and implementer

# machines by region and implementing partner
mach = dt[ ,.(machines=sum(machines)), by=region][order(machines)]
mach_part =  dt[ ,.(machines=sum(machines)), by=impl_partner][order(machines)]

# count of genexpert machines by region 
ggplot(mach, aes(x=reorder(region, -machines), y=machines, label=machines, fill=single_red)) +
  geom_bar(stat="identity") +
  geom_text(size = 4, position=position_stack(vjust = 1.06)) +
  scale_fill_manual(name='', values=single_red) + 
  theme_minimal() +
  labs(x='Region', y='Number of GeneXpert Machines', 
       title="Number of GeneXpert machines by region") +
  theme(text = element_text(size=18), axis.text.x=element_text(size=12, angle=90)) +
  guides(fill=FALSE)

# count of genexpert machines by implementing partner
ggplot(mach_part, aes(x=reorder(impl_partner, -machines), y=machines, label=machines, fill=single_blue)) +
  geom_bar(stat="identity") +
  geom_text(size = 4, position=position_stack(vjust = 1.1)) +
  scale_fill_manual(values=single_blue) + 
  theme_minimal() +
  labs(x='Implementing Partner', y='Number of GeneXpert Machines', 
       title="Number of GeneXpert machines by implementing partner") +
  theme(text = element_text(size=18), axis.text.x=element_text(size=12, angle=90)) +
  guides(fill=FALSE)

# ------------------------------------------------------------------------------------
# rates by region 

# calculate rates 
reg = dt[ , lapply(.SD, sum, na.rm=T), by=region, .SDcols=9:15]
reg[ ,percent_pos:=round(100*tb_positive/total_samples, 1)]
reg[ ,percent_res:=round(100*rif_resist/tb_positive, 1)]
reg[ ,percent_res_total:=round(100*rif_resist/total_samples, 1)]
reg[ ,percent_indet:=round(100*rif_indet/tb_positive, 1)]
reg[ ,percent_indet_total:=round(100*rif_indet/total_samples, 1)]
reg[ ,error_rate:=round(100*total_errors/total_samples, 1)]

# ---------------------------
# count of total samples and total positive samples
reg_sub = reg[ ,.(total_samples, tb_positive) , by=region]
reg_sub = melt(reg_sub, id.vars='region')


count_labels = reg[ ,.(total_samples = sum(total_samples), tb_positive=sum(tb_positive))]
reg_sub$variable = factor(reg_sub$variable, c('total_samples', 'tb_positive'),
                          c(paste0('Total samples (n = ', count_labels$total_samples, ')'),
                            paste0('Positive for TB (n = ', count_labels$tb_positive, ')')))

ggplot(reg_sub, aes(x=reorder(region, -value), y=value, label=value, fill=variable)) +
  geom_bar(stat="identity") +
  geom_text(size = 4, position=position_stack(vjust = 0.5)) +
  facet_wrap(~variable, scales='free_y') +
  scale_fill_manual(values=sex_colors) +
  labs(x='Region', y='Count', title='Total samples tested and TB positive samples by region') +
  theme_bw() +
  theme(text = element_text(size=18), axis.text.x=element_text(size=12, angle=90)) +
  guides(fill=FALSE)


# ---------------------------
#  percent TB positive
ggplot(reg, aes(x=reorder(region, -percent_pos), y=percent_pos, label=percent_pos, fill=turq)) +
  geom_bar(stat="identity") +
  geom_text(size = 4, position=position_stack(vjust = 0.5)) +
  scale_fill_manual(values=turq) + 
  theme_minimal() +
  labs(x='Region', y='Percent positive (%)', 
       title="Percentage of total samples that were positive for TB, April 1 - 7, 2019") +
  theme(text = element_text(size=18), axis.text.x=element_text(size=12, angle=90)) +
  guides(fill=FALSE)

# comparison of percentages within total samples
compare = reg[ ,.(error_rate, percent_pos, percent_res_total, percent_indet_total), by=region]
compare = melt(compare, id.vars='region')

compare$variable = factor(compare$variable, c('error_rate', 'percent_pos', 'percent_indet_total',
                                              'percent_res_total'),
                          c('Errors', 'TB positive', 'Rifampicin indeterminate', 
                            'Rifampicin resistant')) 

# percent TB positive, rifampicin indeterminate, rifampicin resistant
ggplot(compare, aes(x=region, y=value, fill=variable)) +
  geom_bar(position ="dodge", stat="identity") +
  theme_minimal() +
  scale_fill_manual(values=ratios) +
  labs(x='Region', y='Percent (%)', 
       title="Percentage of total samples", 
       fill='') +
  theme(text = element_text(size=18), axis.text.x=element_text(size=12, angle=90)) 

# rifampicin indeterminate, rifampicin resistant
ggplot(compare[variable!='TB positive'], aes(x=region, y=value, fill=variable)) +
  geom_bar(position ="dodge", stat="identity") +
  theme_minimal() +
  scale_fill_manual(values=rev(brewer.pal(3, 'YlOrRd'))) +
  labs(x='Region', y='Percent (%)', 
       title="Percentage of total samples", 
       fill='') +
  theme(text = element_text(size=18), axis.text.x=element_text(size=12, angle=90)) 

# -------------------------------------
# percentages of TB positive 

pos = reg[ ,.(tb_positive, rif_resist, rif_indet), by=region]

# create totals for labels 
labels = pos[ ,lapply(.SD, sum), .SDcols=2:4]

# calculate ratios, label, and visualize
pos[ , res_ratio:=100*rif_resist/tb_positive]
pos[ , ind_ratio:=100*rif_indet/tb_positive]
pos = pos[ ,.(res_ratio, ind_ratio), by=region]
pos = melt(pos, id.vars='region')
pos[variable=='ind_ratio', variable:=paste0('Rifampicin indeterminate (n = ', labels$rif_indet, ')')]
pos[variable=='res_ratio', variable:=paste0('Rifampicin resistant (n = ', labels$rif_resist, ')')]


# rifampicin indeterminate, rifampicin resistant
ggplot(pos, aes(x=region, y=value, fill=variable)) +
  geom_bar(position ="dodge", stat="identity") +
  theme_minimal() +
  scale_fill_manual(values=c('#7fbc41', '#fdb863')) +
  labs(x='Region', y='Percent (%)', 
       title="Percentage of TB positive samples", 
       fill='', subtitle=paste0('n = ', labels$tb_positive)) +
  theme(text = element_text(size=18), axis.text.x=element_text(size=12, angle=90)) 


# -------------------------------------
#---------------------------------------------------------------
# tests performed by implementer

part = dt[ , lapply(.SD, sum, na.rm=T), by=impl_partner, .SDcols=9:15]
part[ ,percent_pos:=round(100*tb_positive/total_samples, 1)]
part[ ,percent_res:=round(100*rif_resist/tb_positive, 1)]
part[ ,percent_indet:=round(100*rif_indet/tb_positive, 1)]
part[ ,error_rate:=round(100*total_errors/total_samples, 1)]




total = sum(part$total_samples)

ggplot(part, aes(x=impl_partner, y=total_samples, fill='Total samples')) +
  geom_bar(stat="identity") +
  theme_minimal() +
  labs(x='Implementing Partner', y='Total Samples', subtitle=paste0('Total samples: ', total), 
   fill=" ") +
  theme(text = element_text(size=18), axis.text.x=element_text(size=12, angle=90))



ggplot(part, aes(x=impl_partner, y=percent_pos, fill='Percent positive')) +
  geom_bar(stat="identity") +
  theme_minimal() +
  labs(x='Implementing Partner', y='% positive', subtitle=paste0('Total percent positive: ', total_or), 
       fill=" ") +
  theme(text = element_text(size=18), axis.text.x=element_text(size=12, angle=90))


dev.off()



# -------------------------------------












