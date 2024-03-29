library(data.table)
library(readxl)
library(arrow)
library(sf)

source("src/key_amazon.R")
get_bucket("envbran")

# List of municipalities
comuni = read_xlsx("C:/Users/nicol/Documents/spatial_statistics/data/2_location_data/italy_geo.xlsx",
                   sheet = 2, range = "A1:D7979")
write_feather(comuni,"C:/Users/nicol/Documents/spatial_statistics/data/2_location_data/municipalities.arrow") 
municipalities = list.files("C:/Users/nicol/Documents/spatial_statistics/data/2_location_data",
                            pattern = "municipalities.arrow",full.names = T)

# INEMAR data
inemar = fread(paste0("C:/Users/nicol/Documents/spatial_statistics/data/3_pollutant_data/",
                      "Dati_INEMAR_2019_richiesti_il_18-Nov-2023_16.40.18/",
                      "Dati_INEMAR_2019_richiesti_il_18-Nov-2023_16.40.18.csv"),
               fill = TRUE, nrows = 34859)

# Sum the pollutant by distretto
inemar = inemar[-c(1),-c("V37"),with=F]
inemar[,c(names(inemar)[6:36]) := lapply(.SD, function(i) as.numeric(i)),
       .SDcols = names(inemar)[6:36]]
write_feather(inemar,"C:/Users/nicol/Documents/spatial_statistics/data/3_pollutant_data/inemar.arrow") 
inemar_data = list.files("C:/Users/nicol/Documents/spatial_statistics/data/3_pollutant_data/",
                            pattern = ".arrow",full.names = T)

write_on_s3 = c(municipalities,inemar_data)

lapply(1:NROW(write_on_s3),function(i){
  put_object(file = paste0(write_on_s3[i]),
             bucket = 'envbran/maps',
             multipart = TRUE)
})




