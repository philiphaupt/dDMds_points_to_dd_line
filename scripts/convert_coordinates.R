# aim convert and concatenate coordinates to decimal degrees: from Deciomal degrees + decimal minutes to decimal degrees
# method: split minutes decimal seconds into two columns, then multiply the decimal seconds with 60 and concatenate three coliumns to get DMS Then use sp char2DMS to get an offical data type. then convert to numeric to get decimal degrees
# correct coordinates

library(sf)
library(sp)
library(tidyverse)

# read in  data
input_dat <- read_csv("./data/input_data.txt")

# isolate characters
# Decimal_degrees_minutes
d_y <- stringr::str_split_fixed(string = as.character(input_dat$lattitude),
                                    pattern =  "d",
                                    n = 2)[,1]
d_x <- stringr::str_split_fixed(string = as.character(input_dat$longitude),
                                pattern =  "d",
                                n = 2)[,1]

# Minutes
m_y <- stringr::str_split_fixed(string = as.character(input_dat$lattitude),
                               pattern =  "[[:digit:]]+d",
                               n = 3)[,2] %>% 
  stringr::str_split_fixed(.,
                           pattern =  "[[:punct:]]",
                            n = 2) %>% 
   as_tibble() %>% 
   select(mins_y = V1)


m_x <- stringr::str_split_fixed(string = as.character(input_dat$longitude),
                                pattern =  "[[:digit:]]+d",
                                n = 3)[,2] %>% 
  stringr::str_split_fixed(.,
                           pattern =  "[[:punct:]]",
                           n = 2) %>% 
  as_tibble() %>% 
  select(mins_x = V1)

# Decimal Seconds 
s_y <- stringr::str_split_fixed(string = as.character(input_dat$lattitude),
                                                         pattern =  "[[:punct:]]",
                                                         n = 3)[,2] 

s_x <- stringr::str_split_fixed(string = as.character(input_dat$longitude),
                                                         pattern =  "[[:punct:]]",
                                                         n = 3)[,2]

# concatenate into a single character
dms_y <- paste0(d_y, "°",m_y$mins_y,"′","0", "″N")
dms_y_2 <- char2dms(dms_y, chd = "°", chm = "′", chs = "″")#
dms_x <- paste0(d_x, "°",m_x$mins_x,"′",as.character("0"), "″E")
dms_x_2 <- char2dms(dms_x, chd = "°", chm = "′", chs = "″")


#convert to decimal coordinates
dd_y <- as.numeric(dms_y_2)
dd_x <- as.numeric(dms_x_2)

# add data to dataframe
input_dat$dd_lat <- dd_y + (as.numeric(paste0("0.", s_y))/60)
input_dat$dd_lon <- dd_x + (as.numeric(paste0("0.",s_x))/60)

write_csv(input_dat, "./points_wgs84dd.csv")

# convert to sf object
points_sf <- st_as_sf(
  x = input_dat,
  coords = c("dd_lon", "dd_lat"),
  crs = 4326
  )

# yes it is correct format
st_is_longlat(points_sf)

#points_sf_2 <- st_cast(st_geometry(points_sf), "POINT")
write_sf(points_sf_2, "./output/line.gpkg", layer = "points_sf")

points_sf$grp <- 1

line_sf <- points_sf %>% select(grp) %>% group_by(grp )%>% summarize(m = mean(grp)) %>% st_cast("LINESTRING")
sf::write_sf(line_sf, "./output/line.gpkg", layer = "output")
