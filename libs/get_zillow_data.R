#' get_zillow_data
#' @param address a city of interest
#' @param city_state_zip the city, state and zipcode of interest
#' @param zillow_id the zillow API Key
#' 

get_zillow_data <- function(address, city_state_zip, zillow_key){
  
  # Format the inputs
  my_address <- gsub(pattern = " ", replacement = "+",
                     address)
  
  my_city <- gsub(pattern = " ", replacement = "+",
                  city_state_zip)
  
  # Assemble to API Details
  base_call <- paste0("http://www.zillow.com/webservice/GetDeepSearchResults.htm?zws-id=",
                      zillow_key,"&address=",my_address,"&citystatezip=", my_city)
  
  # Post the call to the API
  call <- POST(base_call)
  
  if(call$status_code!=200){
    
    print("Error")
    
  } else{
    
    # Convert to proper xml object
    call_xml <- read_xml(call)
    
    street <- xml_find_all(call_xml, ".//street") %>% 
      html_text()
    
    zipcode <- xml_find_all(call_xml, ".//zipcode") %>% 
      html_text()
    
    city <- xml_find_all(call_xml, ".//city") %>% 
      html_text()
    
    state <- xml_find_all(call_xml, ".//state") %>% 
      html_text()
    
    latitude <- xml_find_all(call_xml, ".//latitude") %>% 
      html_text()
    
    longitude <- xml_find_all(call_xml, ".//longitude") %>% 
      html_text()
    
    FIPScounty <- xml_find_all(call_xml, ".//FIPScounty") %>% 
      html_text()
    
    useCode <- xml_find_all(call_xml, ".//useCode") %>% 
      html_text()
    
    taxAssessmentYear <- xml_find_all(call_xml, ".//taxAssessmentYear") %>% 
      html_text()
    
    taxAssessment <- xml_find_all(call_xml, ".//taxAssessment") %>% 
      html_text()
    
    yearBuilt <- xml_find_all(call_xml, ".//yearBuilt") %>% 
      html_text()
    
    lotSizeSqFt <- xml_find_all(call_xml, ".//lotSizeSqFt") %>% 
      html_text()
    
    finishedSqFt <- xml_find_all(call_xml, ".//finishedSqFt") %>% 
      html_text()
    
    bathrooms <- xml_find_all(call_xml, ".//bathrooms") %>% 
      html_text()
    
    bedrooms <- xml_find_all(call_xml, ".//bedrooms") %>% 
      html_text()
    
    call_xml %>% 
      xml_find_all(".//zestimate")->x
    
    zestimate <- xml_child(x[[1]], 1) %>% 
      html_text()
    
    call_xml %>% 
      xml_find_all(".//region")->x
    
    name <- call_xml %>% 
      xml_find_all(".//region") %>% 
      xml_attrs() %>% 
      .[[1]] %>% 
      .[1]
    
    type <- call_xml %>% 
      xml_find_all(".//region") %>% 
      xml_attrs() %>% 
      .[[1]] %>% 
      .[3]
    
    output <- data_frame(street = street,
                         zipcode = zipcode,
                         city = city,
                         state = state,
                         latitude = latitude,
                         longitude = longitude,
                         FIPScounty = FIPScounty,
                         useCode = useCode,
                         taxAssessmentYear = taxAssessmentYear,
                         yearBuilt = yearBuilt,
                         lotSizeSqFt = lotSizeSqFt,
                         finishedSqFt = finishedSqFt,
                         bathrooms = bathrooms,
                         bedrooms = bedrooms,
                         zestimate = zestimate,
                         name = name,
                         type = type)
    
  }
  output
}