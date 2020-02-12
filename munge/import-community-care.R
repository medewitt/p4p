# Purpose: Download the Communities of Care from US HUD


# download shape files ----------------------------------------------------


shape_url = "https://files.hudexchange.info/reports/published/CoC_GIS_State_Shapefile_NC_2018.zip"

temp_dir <- tempfile()

download.file(url = shape_url,  destfile = temp_dir)
temp2 <- tempfile()
unzip(zipfile = temp_dir, exdir = temp2)


# read shape files --------------------------------------------------------

fs::dir_ls(file.path(temp2, "North_Carolina", "NC_500"))

nc_500 <- sf::st_read(file.path(temp2, "North_Carolina", "NC_500"))

plot(nc_500)

summary(nc_500)


# -------------------------------------------------------------------------





