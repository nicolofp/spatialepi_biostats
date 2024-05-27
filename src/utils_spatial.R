Y = fread("C:/Users/nicol/Downloads/IPCHEM_AIRQUALITY_2019/IPCHEM_AIRQUALITY_b28ba3c3-7c88-4658-8dca-1a449a63de2b.csv")
Y = Y[`Country Code` == "ITA"]

X = unique(Y[,.(Latitude,Longitude)])

geo_points = st_multipoint(cbind(X$Longitude,X$Latitude),dim = "XY")

st_intersects(ats$geometry,
              geo_points,
              sparse = T)

a = c(st_centroid(ats$geometry[1]),
      st_centroid(ats$geometry[3]))

X$ats = rep(NA,NROW(X))
sapply(1:NROW(X),function(i){
  print(paste0("Iteration: ",i))
  trash = st_intersects(ats$geometry,
                        st_point(geo_points[i,],dim = "XY"),
                        sparse = F)
  X$ats[i] <<- ifelse(sum(trash) == 0,NA,which(trash))
})
  

Y = merge(Y,X, by = c("Longitude","Latitude"), all.y = T)
Y = Y[!is.na(ats)]

D = dcast(Y, ats ~ `Chemical Name`, 
          value.var = "Concentration Value",
          fun.aggregate = mean,rm.na = T)

