library(data.table)
library(readxl)
library(dplyr)
library(arrow)
library(sf)

source("src/key_amazon.R")

# INEMAR data and aggregate per municipalities
inemar = data.table(read_feather("s3://envbran/maps/inemar.arrow"))
inemar = inemar[,lapply(.SD, sum, na.rm=TRUE), by= .(`Istat comune`), 
                .SDcols = names(inemar)[6:36]]

# Load municipalities geo-coodrdinates
municipalities = data.table(read_feather("s3://envbran/maps/municipalities.arrow"))
municipalities = municipalities[istat %in% inemar$`Istat comune`]

pollution = merge(inemar,municipalities, 
                  by.x = "Istat comune",
                  by.y = "istat")

station = fread("https://www.dati.lombardia.it/resource/ib47-atvt.csv",
                encoding = "UTF-8")
station = station[nometiposensore == "PM10 (SM2005)"]
# 10320 - id sensore via senato milano
comuni = municipalities[,.(comune,lat = as.numeric(lat),lng = as.numeric(lng))]

point_mat = rbind(as.matrix(station[,.(lat1,lng1)]),
                  as.matrix(comuni[,2:3,with = F]))
geospatial_dist <- distm(point_mat, fun = distGeo)  
test = sapply(1:94, function(i) which.min(geospatial_dist[i,95:NCOL(geospatial_dist)]))
test2 = test = sapply(1:94, function(i) min(geospatial_dist[i,95:NCOL(geospatial_dist)]))

W = cbind(station,pollution[test])
W = W[,.(nomestazione,PM10,comune)]
W = cbind(W,test2)

sensori = fread("https://www.dati.lombardia.it/resource/ib47-atvt.csv")

arpa_data = sapply(1:94,function(i){
  z = fread(paste0("https://www.dati.lombardia.it/resource/",
                   "g2hp-ar79.csv?idsensore=",station$idsensore[i],"&$limit=50000000"))
  z[,data := as.Date(as.character(data))]
  mean(ifelse(z[year(data) == 2019,valore] < 0,NA,z[year(data) == 2019,valore]),na.rm = T)
})

W = cbind(W,arpa_data)

## Air quality
sensori = fread("https://www.dati.lombardia.it/resource/ib47-atvt.csv")
stazioni = fread("https://www.dati.lombardia.it/resource/ib47-atvt.csv",
                 encoding = "UTF-8")
valori = data.table(read_feather("s3://envbran/AQ/Dati_sensori_aria_2018_2023_20241221.arrow"))

test = dcast(sensori, idstazione ~ nometiposensore)
colSums(test[,-1,with=F]) 
test2 = test[,.(`Biossido di Azoto`,`Biossido di Zolfo`,
                `Monossido di Carbonio`,`Ossidi di Azoto`,Ozono,
                `PM10 (SM2005)`)]
test2[, check := rowSums(.SD), .SDcols = c("Biossido di Azoto","Biossido di Zolfo",
                                           "Monossido di Carbonio","Ossidi di Azoto",
                                           "Ozono","PM10 (SM2005)")]
pollutant = c("Biossido di Azoto","Biossido di Zolfo",
              "Monossido di Carbonio","Ossidi di Azoto",
              "Ozono","PM10 (SM2005)")
stazioni_r = stazioni[nometiposensore %in% pollutant & (is.na(datastop) | datastop >= as.POSIXct("2020-01-01"))]


head(valori)
pol_mean = valori[year(Data) == 2019 & idSensore %in% stazioni_r$idsensore & 
                    Stato == "VA"][,.(Valore = mean(Valore)), by = .(idSensore,year(Data))]
stazioni_r = merge(stazioni_r,pol_mean, by.x = c("idsensore"), by.y = c("idSensore"))
cor(dcast(stazioni_r, idstazione ~ nometiposensore, value.var = "Valore")[,-1,with=F],use = "pairwise.complete.obs")

# Hospitalization
# maps/hospitalization.arrow
# maps/hospital_geo.arrow



