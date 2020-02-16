tax_info <- list()
i <-  160400
while( i < 1e6){
  call <- paste0("https://services1.arcgis.com/5Yf8nIJWE7cxpd3N/arcgis/rest/services/TPV_Main_Hosted/FeatureServer/0/query?where=0%3D0&objectIds=",i,"&returnGeometry=false&outFields=%2A&f=json")
  try_2 <- suppressMessages(suppressWarnings(jsonlite::fromJSON(readLines(call))))
  dat_to_add <- tryCatch(try_2$features[[1]], error = function(e) "stop")
  if(dat_to_add=="stop"){
    break
  }else{
    #print(dat_to_add)
    tax_info[[i]] <- dat_to_add
    i = i+1
    Sys.sleep(.1)
    print(i)
  }
}

tax_info2 <- dplyr::bind_rows(tax_info)
stamp <- gsub(pattern = " |:", "", Sys.time())
readr::write_rds(tax_info2, here::here("data", 
                                       paste0(stamp,"raw_tax_information_3.rds")))

