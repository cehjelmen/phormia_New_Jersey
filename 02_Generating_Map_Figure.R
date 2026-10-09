###Sept 22, 2026####
#### This script is to generate a map figure of New Jersey with Points indicating
####load in packages####
library(sf)
library(ggplot2)
library(dplyr)
library(viridis)
library(NatParksPalettes)

####pull in relevant map shapefiles for Jersey and its ecoregions####
#We are only using level 3
#all data is open for free use, we just need to cite it

# EPA Level III & IV Ecoregion shapefiles for New Jersey
# Source:  U.S. EPA Office of Research and Development
#          https://www.epa.gov/eco-research/ecoregion-download-files-state-region-2
# License: U.S. Public Domain (17 U.S.C. § 105)
#          https://edg.epa.gov/EPA_Data_License.htm

#downlaoding adn reading shapefiles
dir.create("ecoregions_nj", showWarnings = FALSE)

download.file("https://dmap-prod-oms-edc.s3.us-east-1.amazonaws.com/ORD/Ecoregions/nj/nj_eco_l3.zip",
              destfile = "ecoregions_nj/nj_eco_l3.zip", mode = "wb")
unzip("ecoregions_nj/nj_eco_l3.zip", exdir = "ecoregions_nj/l3")

#ecoregions level 3
eco_l3 <- st_read("ecoregions_nj/l3")


###here we are making our three data collection points have the labels we want
localities <- data.frame(
  # labels for legend and map
  site      = c("North", "Central", "South"),
  # Branchville, New Brunswick, Chatsworth
  longitude = c(-74.845252, -74.450318, -74.511317),  
  latitude  = c( 41.170757,  40.502707,  39.7144421)
)

# Factor with  order so legend runs North → Central → South
localities$site <- factor(localities$site, levels = c("North", "Central", "South"))

localities_sf <- st_as_sf(localities,
                          coords = c("longitude", "latitude"),
                          crs    = 4326)

# Reproject everything to UTM Zone 18N
target_crs    <- 32618
eco_l3        <- st_transform(eco_l3,        crs = target_crs)
localities_sf <- st_transform(localities_sf, crs = target_crs)


###here is the actual map
ggplot() +
  # Ecoregion shapes. we are going to fill them with a colorblind friendly palette
  #and to outline them in black
  geom_sf(data  = eco_l3,
          aes(fill = US_L3NAME),
          color = "black", linewidth = 0.4) +
scale_fill_manual(values=natparks.pals("Arches",5),
                  name="Ecoregion")+
  # Locality points for collections.  We'll use viridis here to match the data
  #points and colors from other plots
  ggnewscale::new_scale_fill() +
  geom_sf(data  = localities_sf,
          aes(fill = site),
          shape = 21, size = 5, stroke = 1, color = "black") +
  scale_fill_viridis_d(
    name   = "Locality",
    option = "D",
    begin  = 0.2, end = 0.85,
    direction=1
  ) +
  
  # Point labels
  geom_sf_label(data      = localities_sf,
                aes(label = site),
                size      = 5,
                nudge_y   = 9000,
                label.size = 0.6,
                fill      = "white",
                alpha     = 0.85) +
  
  coord_sf(crs = target_crs) +
  labs(#title    = "Sampling Localities",
       #subtitle = "North, Central, and South sampling localities",
      x = NULL, y = NULL) +
  theme_bw() +
  theme(legend.position = "right",
        legend.box      = "vertical",
        plot.title      = element_text(face = "bold", size = 15),
        plot.subtitle   = element_text(size = 14, color="black"),
        axis.text       = element_text(size = 12, color="black"),
        legend.text     = element_text(size=14, color="black"),
        legend.title    = element_text(size=15, color="black", face="bold"))


