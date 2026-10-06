rm(list=ls()) #removes everything in R environment
options(stringsAsFactors = FALSE) #stops R from automatically treating string as factors
options(mc.cores = parallel::detectCores()) # for Bayesian models, paralleled chain sampling
graphics.off() # removes anything in graphic environemnt
library(dplyr)
library(ggplot2)
library(brms)
library(tidybayes)
library(sf)
library(Hmsc)
setwd("~/Documents/git/dichogamous_shifts/gamma_Grass/")

d<-read.csv("pensedgeFebtoMay - pensedgeFebtoMay.csv")


dat<-filter(d,PIST==6 & STAM==5|PIST==6 & STAM==6)
dat$phase<-ifelse(dat$PIST==6 & dat$STAM==5,"protogyny","both")
###make response variable
dat <- dat %>% 
  mutate(yday = yday(make_date(year, month, day)))


## get climate data
library(daymetr)
library(dplyr)
library(lubridate)

library(daymetr)
library(dplyr)

# convert year + yday into a date
dat$date <- as.Date(
  paste(dat$year, dat$yday),
  format = "%Y %j"
)

dat<-filter(dat,year<2026)
# function to calculate mean temperature during the
# 90 days before flowering

get_preseason_temp <- function(lat,
                               lon,
                               flower_date,
                               start_year,
                               end_year) {
  
  start_date <- flower_date - 30
  flower_yday <- as.numeric(format(flower_date, "%j"))
  
  out <- tryCatch({
    
    weather <- download_daymet(
      site = "sample",
      lat = lat,
      lon = lon,
      start = start_year,
      end = end_year,
      internal = TRUE
    )
    
    daily <- weather$data
    
    daily$date <- as.Date(
      paste(daily$year, daily$yday),
      format = "%Y %j"
    )
    
    # observation-year temperature
    obs_year <- as.numeric(format(flower_date, "%Y"))
    
    obs_temp <- mean(
      daily$tmax..deg.c.[
        daily$year == obs_year &
          daily$date >= start_date &
          daily$date < flower_date
      ],
      na.rm = TRUE
    )
    
    # climatological temperature for same phenological window
    clim_temp <- daily |>
      dplyr::filter(
        yday >= (flower_yday - 30),
        yday < flower_yday
      ) |>
      dplyr::summarise(
        clim = mean(tmax..deg.c., na.rm = TRUE)
      ) |>
      dplyr::pull(clim)
    
    data.frame(
      temp_obs = obs_temp,
      temp_clim = clim_temp,
      temp_anom = obs_temp - clim_temp
    )
    
  }, error = function(e) {
    
    message(
      "Failed: lat=", lat,
      " lon=", lon,
      " Error: ", e$message
    )
    
    data.frame(
      temp_obs = NA_real_,
      temp_clim = NA_real_,
      temp_anom = NA_real_
    )
    
  })
  
  return(out)
  
}
      
      
# calculate for all specimens
yr_min <- min(dat$year, na.rm = TRUE)
yr_max <- max(dat$year, na.rm = TRUE)
table(dat$stateProvince)
#dat2<-filter(dat,stateProvince %in% c("North Carolina"))


temp_list <- mapply(
  get_preseason_temp,
  lat = dat$decimalLatitude,
  lon = dat$decimalLongitude,
  flower_date = dat$date,
  MoreArgs = list(
    start_year = yr_min,
    end_year = yr_max
  ),
  SIMPLIFY = FALSE
)

temp_df <- dplyr::bind_rows(temp_list)

dat2 <- cbind(dat, temp_df)






# your coordinates
coords <- data.frame(
  lon = dat2$decimalLongitude,
  lat = dat2$decimalLatitude
)


pts <- st_as_sf(coords,
                coords = c("lon","lat"),
                crs = 4326)

# transform to a projected CRS
pts_proj <- st_transform(pts, 5070)  # CONUS Albers

xy <- st_coordinates(pts_proj)

dat2$x <- xy[,1]
dat2$y <- xy[,2]
``
write.csv(dat2,"pensylvanica_wtemp.csv")
library(brms)
#dat<-read.csv("gamma_wtemp.csv")

#dat2$phaser<-ifelse(dat2$phase=="0","pist","stam")
#dat2<-filter(dat2,PIST.F.==1 & STAM..M.==0|PIST.F.==0 & STAM..M.==1)
dat3<-filter(dat2,stateProvince %in% c("Michigan","Indiana","Wisconsin","Illinois","Ontario"))
unique(dat3$stateProvince)
library(dplyr)




get_prior(yday ~ phaser * temp_anom +
    temp_clim +
    gp(x, y),data=dat2)

ggplot(dat2, aes(temp_anom))+geom_density()
mod.goo<-brm(
  yday ~ phase * temp_anom *
    temp_clim, #+
    #gp(x, y),
  data = dat2
)

summary(mod.goo,prob = .5)

conditional_effects(mod.goo,prob = .9)


