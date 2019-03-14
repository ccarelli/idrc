# ----------------------------------------------
# David Phillips, Caitlin O'Brien-Carelli
#
# 5/23/2018
# To extract data from Uganda Viral Load Dashboard: https://vldash.cphluganda.org/
# apply all filters for maximum number of variables (check for errors against Dashboard or verified data)
# use webscrape_vl_parallel to run parallel jobs on the cluster (more up to date)
# ----------------------------------------------


# --------------------
# detect if on windows or on the cluster 

if (Sys.info()[1] == 'Windows') {
  username <- "ccarelli"
  root <- "J:/"
} else {
  username <- Sys.getenv("USER")
  root <- "/home/j/"
}

# ----------------------------------------------
# Files and directories

# whether or not to re-download everything (or just new data)
reload_everything = FALSE


# output directory
dir = paste0(root, 'Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/webscrape/')

# ----------------------------------------------
# Load/prep data

#loop over years - can be altered to run years separately
for(y in c('14', '15', '16', '17', '18')) {
  
  # loop over months
  for(m in c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')){ 
    
    # loop over age groups
    for(a in c('0,1', '1,2,3,4', '5,6,7,8,9,10', '11, 12, 13, 14, 15', '16, 17, 18, 19, 20', '21, 22, 23, 24, 25', '26, 27, 28, 29, 30', '31, 32, 33, 34, 35', '36, 37, 38, 39', '40,41, 42, 43, 44, 45', '46, 47, 48, 49, 50', '51, 52, 53, 54, 55', '56, 57, 58, 59, 60', '61,62,63,64,65', '66,67,68,69,70', '71,72,73,74,75', '76,77,78,79,80', '81,82,83,84,85', '86,87,88,89,90', '91,92,93,94,95', '96,97,98,99')) { 
      
      # loop over tb groups - includes "unknown" option
      for(t in c('y','n','x')) { 
        
        # loop over sexes - includes "unknown" option
        for(s in c('m','f','x')) { 
          
          # check if file exists first
          outFile = paste0(dir, '/facilities_suppression_', m,'20', y,'_',a,'_', s, '_tb', t, '_', '.rds')
          check = file.exists(outFile)
          
          # only download if it doesn't already exist
          if (check==FALSE | reload_everything==TRUE) {
          
            # store url
            url = paste0('https://vldash.cphluganda.org/live?age_ids=%5B', a, '%5D&districts=%5B%5D&emtct=%5B%5D&fro_date=20', y, m,'&genders=%5B%22', s, '%22%5D&hubs=%5B%5D&indications=%5B%5D&lines=%5B%5D&regimens=%5B%5D&tb_status=%5B%22', t, '%22%5D&to_date=20',y, m)
          
            # load
            data = fromJSON(url)
            
            # save raw output
            saveRDS(data, file=outFile)
          }
        }
      }
    }
  }
}
# ----------------------------------------------
