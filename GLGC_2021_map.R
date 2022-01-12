
#### Step 0. set up parameters ####

# directory to read in the country with sample size
input_dir <- 'GLGC_2021_NbyCountry.txt'
# directory to output the map
output_dir <- 'GLGC_map_2021.tiff'

# figure title
fig_title <- 'Global Lipids Genetics Consortium'

# continent color
continent_col <- '#6A9EF0'
# continent's glow color
continent_g_col <- '#001428'
# ocean color
ocean_col <- '#001F3D'
# dot color
dot_col <- 'white'

# numeric number (the larger the number, the smaller the dot)
dot_size <- 4



#### Step 1. install and load packages ####

# ggfx is a (currently experimantal) package that allows the use of various 
# filters and shaders on ggplot2 layers.
# https://github.com/thomasp85/ggfx
devtools::install_github('thomasp85/ggfx')

library(ggrepel)
library(rgeos)
library(ggplot2)
library(sf)
sf::sf_use_s2(F)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(ggthemes)
library(maps)
library(extrafont)
library(extrafontdb)
library(ggimage)
library(ggfx)



#### Step 2. load in and process MAP info ####

# set file directory
GLGC_2021 <- read.table(file=input_dir, header=T)

world <- ne_countries(scale="medium", returnclass="sf")
class(world)

world_points<- st_centroid(world)
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))

# check all country names match the in map coordinates data
all(GLGC_2021$Country%in%world$name)

# create data from with country name, sample size, and coordinates
map_21 <- world_points[match(GLGC_2021$Country, world_points$name),]
map_21_n <- cbind(map_21, GLGC_2021)

# adj coordinates for the US to be more centered
if('United States'%in%map_21_n$Country){
  idx <- which(map_21_n$Country=='United States')
  map_21_n$X[idx] <- map_21_n$X[idx]+12.5
  map_21_n$Y[idx] <- map_21_n$Y[idx]-5
}



#### Step 3. plot the map ####

ggplot(data = world) +
  # remove labels on x- and y-axis
  xlab('') +
  ylab('') +
  # continent and its glow color
  with_outer_glow(geom_sf(color=continent_col, fill=continent_col, alpha=0.9), colour=continent_g_col, sigma=17.5, x_offset=0, y_offset=0, expand=20) +
  # dot color; add blurry glow behind the dots
  with_shadow(geom_point(data=map_21_n, aes(x=X, y=Y), shape=19, color=dot_col, size=(map_21_n$Nall)^(1/(dot_size*0.96)), alpha=0.5), colour=dot_col, sigma=12, x_offset=0, y_offset=0, stack=F) +
  with_blur(geom_point(data=map_21_n, aes(x=X, y=Y), shape=19, color=dot_col, size=(map_21_n$Nall)^(1/dot_size), alpha=0.5), sigma=3) +
  # remove antarctica
  coord_sf(xlim=c(-180.00, 180.00), ylim=c(-60.00, 90.00), expand=F) +
  # add title to the figure
  ggtitle(fig_title) +
  theme(plot.title=element_text(color="black", size=20, face="bold", hjust=0.5), 
        # ocean color
        panel.background=element_rect(fill=ocean_col),
        # no grid on the map
        panel.grid.major=element_line(colour = "transparent"),
        axis.ticks = element_blank(),
        axis.text = element_blank())

# output the figure in .tiff format
ggsave(output_dir, width=14, height=8, device='tiff')

