# aim is to plot the boat sighitngs points to get an overview
library(tmap)

tmap::tmap_mode("view")

# Plot sightings data
points_sf %>%
  tm_shape() +
  tm_symbols(
    col = "blue", 
    size = 0.6,
    alpha = 0.5
    ) +
  line_sf %>%
  tm_shape() +
  tm_lines(
    col = "blue",
    lwd = 1.2,
    lty = "solid")
