#########

## Seagrass chronosequence map data processing

###############################################################################
## Packages:

library(tidyverse)
library(data.table)
library(patchwork)
library(ggtext)
library(tidyr)
library(ggplot2)
library(scales)
library(plotly)
library(lubridate)
library(sf)
library(spdep)
library(ggspatial)
library(here)
library(MoMAColors)

###############################################################################
#                      Data processing for EPSG: 32618

## Seagrass layer folder:
seagrass_folder <- here("data", "seagrass")

## Name layers and set coordinate system:
VCR_beaches <- st_read(dsn = seagrass_folder, layer = "VCR_beaches")
VCR_beaches <- st_transform(VCR_beaches, crs = 32618)
invalid_geoms <- !st_is_valid(VCR_beaches)
sum(invalid_geoms)
if (any(invalid_geoms)) {
  VCR_beaches[invalid_geoms, ] <- st_make_valid(VCR_beaches[invalid_geoms, ])
}
sum(!st_is_valid(VCR_beaches))


VCR_land <- st_read(dsn = seagrass_folder, layer = "VCR_shoreline")
VCR_land <- st_transform(VCR_land, crs = 32618)

sg_2001 <- st_read(dsn = seagrass_folder, layer = "2001")
sg_2001 <- st_transform(sg_2001, crs = 32618)

sg_2003 <- st_read(dsn = seagrass_folder, layer = "2003")
sg_2003 <- st_transform(sg_2003, crs = 32618)

sg_2007 <- st_read(dsn = seagrass_folder, layer = "2007")
sg_2007 <- st_transform(sg_2007, crs = 32618)

sg_2008 <- st_read(dsn = seagrass_folder, layer = "2008")
sg_2008 <- st_transform(sg_2008, crs = 32618)

sg_2009 <- st_read(dsn = seagrass_folder, layer = "2009")
sg_2009 <- st_transform(sg_2009, crs = 32618)

sg_2010 <- st_read(dsn = seagrass_folder, layer = "2010")
sg_2010 <- st_transform(sg_2010, crs = 32618)

sg_2011 <- st_read(dsn = seagrass_folder, layer = "2011")
sg_2011 <- st_transform(sg_2011, crs = 32618)

sg_2013 <- st_read(dsn = seagrass_folder, layer = "2013")
sg_2013 <- st_transform(sg_2013, crs = 32618)

sg_2015 <- st_read(dsn = seagrass_folder, layer = "2015")
sg_2015 <- st_transform(sg_2015, crs = 32618)

sg_2017 <- st_read(dsn = seagrass_folder, layer = "2017")
sg_2017 <- st_transform(sg_2017, crs = 32618)

sg_2018 <- st_read(dsn = seagrass_folder, layer = "2018")
sg_2018 <- st_transform(sg_2018, crs = 32618)

sg_2019 <- st_read(dsn = seagrass_folder, layer = "2019")
sg_2019 <- st_transform(sg_2019, crs = 32618)

sg_2020 <- st_read(dsn = seagrass_folder, layer = "2020")
sg_2020 <- st_transform(sg_2020, crs = 32618)

lon_limits <- c(421575, 429970.3)  
lat_limits <- c(4116561, 4127426)

# Create a bounding box with the specified limits
bbox <- st_bbox(c(xmin = lon_limits[1], xmax = lon_limits[2], 
                  ymin = lat_limits[1], ymax = lat_limits[2]), 
                crs = st_crs(32618))

# Crop the shapefiles with the bounding box
VCR_land <- st_crop(VCR_land, bbox)
VCR_beaches <- st_crop(VCR_beaches, bbox)
sg_2001 <- st_crop(sg_2001, bbox)
sg_2003 <- st_crop(sg_2003, bbox)
sg_2007 <- st_crop(sg_2007, bbox)
sg_2008 <- st_crop(sg_2008, bbox)
sg_2009 <- st_crop(sg_2009, bbox)
sg_2010 <- st_crop(sg_2010, bbox)
sg_2011 <- st_crop(sg_2011, bbox)
sg_2013 <- st_crop(sg_2013, bbox)
sg_2015 <- st_crop(sg_2015, bbox)
sg_2017 <- st_crop(sg_2017, bbox)
sg_2018 <- st_crop(sg_2018, bbox)
sg_2019 <- st_crop(sg_2019, bbox)
sg_2020 <- st_crop(sg_2020, bbox)

vcr_plot <- 
  ggplot()+
  annotate("rect", xmin = -Inf, 
           xmax = Inf, 
           ymin = -Inf, 
           ymax = Inf, 
           fill = "#659cab",
           color = NA) +
  geom_sf(data = VCR_land, fill = "#b4d6b6", color = NA)+
  geom_sf(data = VCR_beaches, fill = "#bdbb9b", color = NA)+
  geom_sf(data = sg_2020, color = NA, fill = "darkgreen")+
  theme_minimal()+
  labs(x = "Longitude (°W)", 
       y = "Latitude (°N)", 
       fill = "Seagrass Expansion") +
  theme(axis.title.y = element_text(color = 'white', 
                                    family = 'Arial', 
                                    size = 12, 
                                    face = "bold"),
        axis.ticks.y = element_line(color = "white"),
        axis.ticks.x = element_line(color = "white"),
        axis.title.x = element_text( 
          family = 'Arial', 
          size = 12, 
          face = "bold",
          color = "white"),
        axis.text.x = element_text(angle = 0, 
                                   hjust = 0.4,
                                   color = "white"),
        axis.text.y = element_text(angle = 0, 
                                   hjust = 0.2,
                                   color = "white"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10, 
                                    face = "bold"),
        axis.line = element_line(),
        strip.background = element_rect(colour = "black", 
                                        fill = "black", 
                                        size = 1),
        panel.border = element_rect(linetype = "solid", 
                                    size = 0.5, 
                                    fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text.x = element_text(colour = "white", 
                                    face = "bold")) +
  annotation_scale(location = "br", 
                   width_hint = 0.20)+
  scale_x_continuous(labels = function(x) 
    sprintf("%.2f", abs(x)), expand = c(0, 0)) +
  scale_y_continuous(labels = function(y) 
    sprintf("%.2f", abs(y)), expand = c(-0.001, -0.5)) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         style = north_arrow_fancy_orienteering, 
                         pad_x = unit(0.05, "in"), pad_y = unit(0.05, "in"),
                         height = unit(0.5, "in"), width = unit(0.5, "in"))


vcr_plot 
ggsave("vcr_plot.png", plot = vcr_plot, dpi =2000)


### SF seagrass plot: 
sb_ss_plot <- ggplot() +
  geom_sf(data = sg_2020) +
  geom_sf(data = sg_2019) +
  geom_sf(data = sg_2018) +
  geom_sf(data = sg_2017) +
  geom_sf(data = sg_2015) +
  geom_sf(data = sg_2013) +
  geom_sf(data = sg_2011) +
  geom_sf(data = sg_2010) +
  geom_sf(data = sg_2009) +
  geom_sf(data = sg_2008) +
  geom_sf(data = sg_2007) +
  geom_sf(data = sg_2003) +
  geom_sf(data = sg_2001) +
  geom_sf(data = VCR_land)+
scale_fill_moma_d("Levine2") +
  theme_minimal()
sb_ss_plot

###############################################################################
### Add shapefiles to same dataframe

## Add "Year" identifier:
sg_2020$Year <- 2020
sg_2019$Year <- 2019
sg_2018$Year <- 2018
sg_2017$Year <- 2017
sg_2015$Year <- 2015
sg_2013$Year <- 2013
sg_2011$Year <- 2011
sg_2010$Year <- 2010
sg_2009$Year <- 2009
sg_2008$Year <- 2008
sg_2007$Year <- 2007
sg_2003$Year <- 2003
sg_2001$Year <- 2001

seagrass_chron <- bind_rows(sg_2020, 
                            sg_2019, 
                            sg_2018, 
                            sg_2017, 
                            sg_2015, 
                            sg_2013, 
                            sg_2011, 
                            sg_2010, 
                            sg_2009, 
                            sg_2008, 
                            sg_2007, 
                            sg_2003, 
                            sg_2001)

seagrass_chron_32618 <- seagrass_chron %>%
  select(Year, geometry)

## Test plot:
ggplot() +
  geom_sf(data = seagrass_chron_32618, 
          aes(fill = as.factor(Year))) +
  scale_fill_moma_d("Alkalay2", 
                    direction = -1)+
  geom_sf(data = VCR_land) +
  theme_minimal()

#################
## PLOT:

seagrass_chron_map <- 
  ggplot() +
  annotate("rect", xmin = -Inf, 
           xmax = Inf, 
           ymin = -Inf, 
           ymax = Inf, 
           fill = "#659cab",
           color = NA) +
  geom_sf(data = seagrass_chron_32618, 
          aes(fill = as.factor(Year)),
          color = NA,
          alpha = 1) +
  scale_fill_moma_d("Alkalay2", 
                    direction = -1)+
  geom_sf(data = VCR_land, 
          size = 0.25, 
          fill = "lightgray", 
          color = NA) +
  #geom_sf(data = VCR_beaches,
  #        fill = "#bdbb9b", 
  #        color = NA)+
  labs(x = "Longitude (°W)", 
       #y = "Latitude (°N)", 
       fill = "Seagrass\nExpansion") +
  theme_minimal()+
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(axis.title.y = element_text(color = 'white', 
                                    family = 'Arial', 
                                    size = 16, 
                                    face = "bold"),
        axis.ticks.y = element_line(color = "white"),
        axis.ticks.x = element_line(color = "white"),
        axis.title.x = element_text( 
          family = 'Arial', 
          size = 16, 
          face = "bold",
          color = "white"),
        axis.text.x = element_text(angle = 0, 
                                   hjust = 0.3,
                                   color = "white"),
        axis.text.y = element_text(angle = 0, 
                                   hjust = 0,
                                   color = "white"),
        legend.text = element_text(size = 10, color = 
                                     'white'),
        legend.title = element_text(size = 12, 
                                    face = "bold",
                                    color = 'white'),
        axis.line = element_line(),
        strip.background = element_rect(colour = "black", 
                                        fill = "black", 
                                        size = 1),
        panel.border = element_rect(linetype = "solid", 
                                    size = 0.5, 
                                    fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text.x = element_text(colour = "white", 
                                    face = "bold")) +
  annotation_scale(location = "bl", 
                   width_hint = 0.2)+
  annotation_north_arrow(location = "tr", which_north = "true", 
                         style = north_arrow_fancy_orienteering, 
                         pad_x = unit(0.05, "in"), pad_y = unit(0.05, "in"),
                         height = unit(0.5, "in"), width = unit(0.5, "in"))

seagrass_chron_map

ggsave("seagrass_chron_map.png", plot =  seagrass_chron_map, dpi =700)


##############################################################################
## Calculate area of each annual seagrass polygon: 
library(units)

seagrass_chron_areas <- seagrass_chron %>%
  mutate(area = st_area(geometry)) %>%
  group_by(Year) %>%
  summarize(total_area = sum(area))
# Convert the area from square meters to a more appropriate unit if necessary, for example, square kilometers
seagrass_chron_areas$total_area_km2 <- 
  seagrass_chron_areas$total_area / 1000000

# Print the resulting dataframe
print(seagrass_chron_areas)

# Assuming seagrass_chron is already loaded and is an sf object
seagrass_area_by_year <- seagrass_chron %>%
  mutate(area_km2 = 
           st_area(geometry) / 
           1000000) %>%  # Calculate area in square kilometers
  group_by(Year) %>%
  summarize(total_area_km2 = 
              sum(area_km2))  # Aggregate area by Year

# Plotting
ggplot(seagrass_area_by_year, 
       aes(x = Year, 
           y = total_area_km2)) +
  geom_line() +  # Use geom_line() for a line plot
  geom_point() 


################################################################################

########              Data processing for EPSG: 4326

## Seagrass layer folder:
seagrass_folder <- here("data", "seagrass")

## Name layers and set coordinate system:
VCR_land_4 <- st_read(dsn = seagrass_folder, layer = "VCR_shoreline")
VCR_land_4 <- st_transform(VCR_land_4, crs = 4326)
sg_2020_4 <- st_read(dsn = seagrass_folder, layer = "2020")
sg_2020_4 <- st_transform(sg_2020_4, crs = 4326)
sg_2019_4 <- st_read(dsn = seagrass_folder, layer = "2019")
sg_2019_4 <- st_transform(sg_2019_4, crs = 4326)
sg_2018_4 <- st_read(dsn = seagrass_folder, layer = "2018")
sg_2018_4 <- st_transform(sg_2018_4, crs = 4326)
sg_2017_4 <- st_read(dsn = seagrass_folder, layer = "2017")
sg_2017_4 <- st_transform(sg_2017_4, crs = 4326)
sg_2015_4 <- st_read(dsn = seagrass_folder, layer = "2015")
sg_2015_4 <- st_transform(sg_2015_4, crs = 4326)
sg_2013_4 <- st_read(dsn = seagrass_folder, layer = "2013")
sg_2013_4 <- st_transform(sg_2013_4, crs = 4326)
sg_2011_4 <- st_read(dsn = seagrass_folder, layer = "2011")
sg_2011_4 <- st_transform(sg_2011_4, crs = 4326)
sg_2010_4 <- st_read(dsn = seagrass_folder, layer = "2010")
sg_2010_4 <- st_transform(sg_2010_4, crs = 4326)
sg_2009_4 <- st_read(dsn = seagrass_folder, layer = "2009")
sg_2009_4 <- st_transform(sg_2009_4, crs = 4326)
sg_2008_4 <- st_read(dsn = seagrass_folder, layer = "2008")
sg_2008_4 <- st_transform(sg_2008_4, crs = 4326)
sg_2007_4 <- st_read(dsn = seagrass_folder, layer = "2007")
sg_2007_4 <- st_transform(sg_2007_4, crs = 4326)
sg_2003_4 <- st_read(dsn = seagrass_folder, layer = "2003")
sg_2003_4 <- st_transform(sg_2003_4, crs = 4326)
sg_2001_4 <- st_read(dsn = seagrass_folder, layer = "2001")
sg_2001_4 <- st_transform(sg_2001_4, crs = 4326)


lat_limits_4 <- c(37.19, 37.29)  
lon_limits_4 <- c(-75.7900, -75.88500)



# Create a bounding box with the specified limits
bbox_4326 <- st_bbox(c(xmin = lon_limits_4[1], xmax = lon_limits_4[2], 
                       ymin = lat_limits_4[1], ymax = lat_limits_4[2]), 
                     crs = st_crs(4326))

# Crop the shapefiles with the bounding box
VCR_land_4 <- st_crop(VCR_land_4, bbox_4326)
sg_2020_4 <- st_crop(sg_2020_4, bbox_4326)
sg_2019_4 <- st_crop(sg_2019_4, bbox_4326)
sg_2018_4 <- st_crop(sg_2018_4, bbox_4326)
sg_2017_4 <- st_crop(sg_2017_4, bbox_4326)
sg_2015_4 <- st_crop(sg_2015_4, bbox_4326)
sg_2013_4 <- st_crop(sg_2013_4, bbox_4326)

sg_2011_4 <- st_crop(sg_2011_4, bbox_4326)
sg_2010_4 <- st_crop(sg_2010_4, bbox_4326)
sg_2009_4 <- st_crop(sg_2009_4, bbox_4326)
sg_2008_4 <- st_crop(sg_2008_4, bbox_4326)
sg_2007_4 <- st_crop(sg_2007_4, bbox_4326)
sg_2003_4 <- st_crop(sg_2003_4, bbox_4326)
sg_2001_4 <- st_crop(sg_2001_4, bbox_4326)

ggplot()+
  geom_sf(data = VCR_land_4)+
  geom_sf(data = sg_2020_4)+
  geom_sf(data = sg_2019_4)+
  geom_sf(data = sg_2018_4)+
  geom_sf(data = sg_2017_4)+
  theme_minimal()

### SF seagrass plot: 
sb_ss_plot <- ggplot() +
  geom_sf(data = sg_2020_4) +
  geom_sf(data = VCR_land_4)+
  scale_fill_moma_d("Levine2") +
  theme_minimal()
sb_ss_plot


## Add "Year" identifier:
sg_2020_4$Year <- 2020
sg_2019_4$Year <- 2019
sg_2018_4$Year <- 2018
sg_2017_4$Year <- 2017
sg_2015_4$Year <- 2015
sg_2013_4$Year <- 2013
sg_2011_4$Year <- 2011
sg_2010_4$Year <- 2010
sg_2009_4$Year <- 2009
sg_2008_4$Year <- 2008
sg_2007_4$Year <- 2007
sg_2003_4$Year <- 2003
sg_2001_4$Year <- 2001

seagrass_chron_4326 <- bind_rows(sg_2020_4,
                                 sg_2019_4,
                                 sg_2018_4,
                                 sg_2017_4,
                                 sg_2015_4,
                                 sg_2013_4,
                                 
                                 sg_2011_4,
                                 sg_2010_4,
                                 sg_2009_4,
                                 sg_2008_4,
                                 sg_2007_4,
                                 sg_2003_4,
                                 sg_2001_4)

seagrass_chron_4326 <- seagrass_chron_4326 %>%
  select(Year, geometry)

