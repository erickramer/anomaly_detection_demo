library(httr)
library(purrr)
library(dplyr)
library(readr)
library(tidyr)

downloader = function(code, city, country, year, month){
  base_url = "https://www.wunderground.com/history/airport/"
  params = "&reqdb.zip=&reqdb.magic=&reqdb.wmo=&format=1"
  
  url = paste0(base_url,
               code, "/",
               year, "/",
               month, "/",
               1, "/",
               "MonthlyHistory.html?",
               "&req_city=", city,
               "&req_statename=", country,
               params)
  
  cat(paste("Downloading from:",
            url,
            "\n"))
  
  destination_file = paste0("./data/raw/",
                            city,
                            "_",
                            country,
                            "_",
                            year,
                            "_",
                            month)

  download.file(url, destfile=destination_file)
  
  read_csv(destination_file, skip=1) %>%
    mutate(Precipitationmm = as.numeric(Precipitationmm))
}

download_range = function(code, city, country, year_start = 2010, year_end = 2015){
  years = seq(year_start, year_end)
  months = seq(1, 12)
  
  dfs = map(years, function(year){
    map(months, function(month){
      downloader(code, city, country, year, month)
    })
  })
  
  dfs %>%
    flatten() %>%
    bind_rows()
}

impute = function(x){
  if(is.numeric(x)){
    x[is.na(x)] = median(x, na.rm=T)
  }
  x
}

cities = data.frame(code = c("LFPO", "EGLL", "KNYC"),
                    city = c("Paris", "London", "New+York"),
                    country = c("France", "UK", "New+York"),
                    stringsAsFactors=F)

df = invoke_rows(download_range, cities) %>%
  unnest() %>%
  mutate(city = gsub("[+]", " ", city, perl=T)) %>%
  select(city,
         date=CET, 
         temperature=`Mean TemperatureC`,
         humidity=`Mean Humidity`,
         visibility=`Mean VisibilityKm`,
         wind_speed=`Mean Wind SpeedKm/h`,
         precipitation=Precipitationmm,
         pressure=`Mean Sea Level PressurehPa`) %>%
  mutate_each(funs(impute))

save(df, file="./data/weather_data.Rdata")
