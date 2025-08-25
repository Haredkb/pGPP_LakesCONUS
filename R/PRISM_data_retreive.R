################################################
### ###     PRISM Function           ###########
################################################
# library(devtools) #needed to download prism from github
# library(reshape2) ##melting dataframes
# library(dplyr) #data wrangling
# library(raster) ##working with raster data
# library(sp) ##manipulationg spatial data
# # fun fact pd_to_file() function conveniently returns the absolute path. 
# library(devtools)
# install_github("ropensci/prism")
# #install.packages("prism")
# library(prism) ##prism data access
# #Be sure to set the download folder using `prism_set_dl_dir()`.
# prism_set_dl_dir("C:/Users/hared/Dropbox/ResearchProjects/006_LakeGPP_CONUS/400_Analysis/GPP_CONUS_Lakes/data/prism")#set download directory 
# prism_get_dl_dir()#set download directory 
# ls_prism_data() #deprecated but actually works... 
# 
# ### Retrieve Monthly Precipitation  
# get_prism_monthlys(type = "ppt", year = 2014:2016, mon = 5:10, keepZip = FALSE)# 4km resolution
# #ppt	Total precipitation (rain and snow) - but also average precip
# 
# pd_get_name(prism_archive_ls())
# prism_archive_subset("ppt", "monthly", mon = 6)
# boulder <- c(-105.2797,40.0176)
# #prism_archive_subset("ppt", "monthly", mon = 7) #nice way to subset the growing season folders
# 
# pd <- prism_archive_subset("ppt", "monthly", mon = 7)
# pd_image(pd)
# 
# # data already exist in the prism dl dir
# boulder <- c(-105.2797, 40.0176)
# 
# # prism_archive_subset() will return prism data that matches the specified 
# # variable, time step, years, months, days, etc.
# to_slice <- prism_archive_subset("ppt", "monthly", mon = 1)
# p <- pd_plot_slice(to_slice, boulder)



# get_prism_dailys(
#   type = "tmean",
#   minDate = "2020-12-31",
#   maxDate = "2021-12-31",
#   keepZip = TRUE,
#   check = "httr"
# )
