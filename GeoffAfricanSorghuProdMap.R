#Load libraries 
library(tidyverse)
library(vcfR)
library(data.table)
library(maps)
library(tidyr)
library(ggplot2)
library(gdata)
library(dplyr)
library(scales)

#Load files
#A2 = two letter country code
Africancountrycodes <- read.csv("~/Downloads/Africancountrycodes.csv")
View(Africancountrycodes)
Africancountrycodes<-Africancountrycodes[,1:2]

Africancountrycodes<-Africancountrycodes %>% add_row(Country = "Tanzania", A2 = "TZ")
Africancountrycodes<-Africancountrycodes %>% add_row(Country = "Lesotho", A2 = "LS")

#FAO STAT data, Value= tonnes production
FAO2023SorghMil <- read_csv("FAO2023SorghMil.csv")
View(FAO2023SorghMil)

FAOSorg2023<-subset(FAO2023SorghMil, Item =="Sorghum")
FAOMil2023<-subset(FAO2023SorghMil, Item == "Millet")


############### For 2023 FAO STAT DATA ############################
                 #####Sorghum #########
# Load world map data
world_map <- map_data("world")

colnames(world_map)[5]= "Country"


# Merge FAOSorg2023 with world_map to get coordinates

FAOSorg2023<- left_join(FAOSorg2023,Africancountrycodes, by= "Country")

merged_data= left_join( world_map,FAOSorg2023, by= "Country")


# Calculate centroids for each country to place the country names
centroids <- merged_data %>%
  group_by(Country) %>%
  summarise(
    long = mean(long, na.rm = TRUE),
    lat = mean(lat, na.rm = TRUE),
    Value = first(Value),
    A2= first(A2))


# Create the base map
base_map <- ggplot() +
  coord_cartesian(xlim = c(-18, 52), ylim = c(-35, 37)) +
  xlab("") + ylab("") +
  geom_polygon(data = merged_data, aes(x = long, y = lat, group = group),
               colour = "white", fill = "grey80")

# Map production values by country and add country names
sorghum2023production_mapped_africa <- base_map +
  geom_polygon(data = merged_data, 
               aes(x = long, y = lat, group = group, fill = Value),
               colour = "black") +
  geom_text(data = centroids,  # Use centroids to place country names
            aes(x = long, y = lat, label = A2),  # Use 'region' for country names
            size = 3, color = "black", fontface = "bold") +
  scale_fill_gradient(low = "white", high = "darkgrey", na.value = "white") +  # Black-and-white gradient
  theme_void() +
  theme(
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.position = "bottom"
  ) +
  guides(fill = guide_legend(title = "Sorghum Production tonnes 2023"))

# Display the map
sorghum2023production_mapped_africa


############# Now for Millet 2023 FAO stat data ###############
# Load world map data
world_map <- map_data("world")
colnames(world_map)[5]= "Country"

FAOMil2023<- left_join(FAOMil2023,Africancountrycodes, by= "Country")

merged_data= left_join(world_map,FAOMil2023, by= "Country")

# Calculate centroids for each country to place the country names
centroids <- merged_data %>%
  group_by(Country) %>%
  summarise(
    long = mean(long, na.rm = TRUE),
    lat = mean(lat, na.rm = TRUE),
    Value = first(Value),
    A2= first(A2))
  #) %>%
  #filter(Value > 603592)  if you want to filter by a certain value for production 

# Create the base map
base_map <- ggplot() +
  coord_cartesian(xlim = c(-18, 52), ylim = c(-35, 37)) +
  xlab("") + ylab("") +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
               colour = "black", fill = "grey80")  # Set outline color to black

# Map production values by country and add country names
Millet2023production_mapped_africa <- base_map +
  geom_polygon(data = merged_data, 
               aes(x = long, y = lat, group = group, fill = Value),
               colour = "black") +  # Set outline color to black
  geom_text(data = centroids,  # Use filtered centroids to place country names
            aes(x = long, y = lat, label = A2),  # Use 'region' for country names
            size = 3, color = "black", fontface = "bold") +
  scale_fill_gradient(low = "white", high = "darkgrey", na.value = "white") +  # Black-and-white gradient
  theme_void() +
  theme(
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.position = "bottom"
  ) +
  guides(fill = guide_legend(title = "Millet Production tonnes 2023"))

# Display the map
Millet2023production_mapped_africa

############# 2024 production from USDA #######################
#data from: https://ipad.fas.usda.gov/cropexplorer/cropview/commodityView.aspx?cropid=0459200 
#The data is in 1000 Metric tonnes (MT)
USDAsorgmilpro2024data <- read.csv("~/Downloads/USDAsorgmilpro2024data.csv")

world_map <- map_data("world")

colnames(world_map)[5]= "Country"

#We dont have 2024 production values for Malawi and Zambia so add 2023 values manually
USDAsorgmilpro2024data<-USDAsorgmilpro2024data %>% add_row(Country = "Malawi", SorgProductionMT = 117, MilletProductionMT= 43)
USDAsorgmilpro2024data<-USDAsorgmilpro2024data %>% add_row(Country = "Zambia", SorgProductionMT = 7, MilletProductionMT= 47)


# Merge FAOSorg2024 with world_map to get coordinates

FAOSorg2024<- left_join(USDAsorgmilpro2024data,Africancountrycodes, by= "Country")

merged_data= left_join(world_map,FAOSorg2024, by= "Country")


# Calculate centroids for each country to place the country names
centroids <- merged_data %>%
  group_by(Country) %>%
  summarise(
    long = mean(long, na.rm = TRUE),
    lat = mean(lat, na.rm = TRUE),
    Sorg2024 = first(SorgProductionMT),
    Mil2024 = first(MilletProductionMT),
    A2= first(A2))
 # ) %>%
 # filter(Sorg2024 > 99)  # Filter to include only countries with Value > X


# Create the base map
base_map <- ggplot() +
  coord_cartesian(xlim = c(-18, 52), ylim = c(-35, 37)) +
  xlab("") + ylab("") +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
               colour = "black")  # Set outline color to black

# Map production values by country and add country names
sorghum2024production_mapped_africa <- base_map +
  geom_polygon(data = merged_data, 
               aes(x = long, y = lat, group = group, fill = SorgProductionMT),
               colour = "grey50") + 
  theme(
    axis.title.x = element_blank(),  # Remove x-axis label
    axis.title.y = element_blank(),  # Remove y-axis label
    axis.text.x = element_blank(),   # Remove x-axis tick labels
    axis.text.y = element_blank(),   # Remove y-axis tick labels
    axis.ticks = element_blank(),    # Remove axis ticks
    panel.background = element_blank()  # Remove gray background (optional)
  ) +
  geom_text(data = centroids,  # Use filtered centroids to place country names
            aes(x = long, y = lat, label = A2),  # Use 'region' for country names
            size = 3, color = "black", fontface = "bold") +
  scale_fill_gradient(
    low = "grey94", 
    high = "grey45", 
    na.value = "white", 
    breaks = seq(0, max(merged_data$SorgProductionMT, na.rm = TRUE), by = 1000),  # Custom breaks
    labels = label_comma(),  # Format labels
    limits = c(0, max(merged_data$SorgProductionMT, na.rm = TRUE))  # Set limits
  ) +
  theme(
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.position = "bottom"
  ) +
  guides(fill = guide_colorbar(title = "Sorghum Production 1000 MT 2024",barwidth = 15, barheight = 1.5, vjust = 0.01)) +
  theme(legend.text = element_text(angle = 45))

# Display the map
sorghum2024production_mapped_africa

################# 2024 millet production map

base_map <- ggplot() +
  coord_cartesian(xlim = c(-18, 52), ylim = c(-35, 37)) +
  xlab("") + ylab("") +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
               colour = "black")  # Set outline color to black

# Map production values by country and add country names
Millet2024production_mapped_africa <- base_map +
  geom_polygon(data = merged_data, 
               aes(x = long, y = lat, group = group, fill = MilletProductionMT),
               colour = "grey50") + 
  theme(
    axis.title.x = element_blank(),  # Remove x-axis label
    axis.title.y = element_blank(),  # Remove y-axis label
    axis.text.x = element_blank(),   # Remove x-axis tick labels
    axis.text.y = element_blank(),   # Remove y-axis tick labels
    axis.ticks = element_blank(),    # Remove axis ticks
    panel.background = element_blank()  # Remove gray background (optional)
  ) +
  geom_text(data = centroids,  # Use filtered centroids to place country names
            aes(x = long, y = lat, label = A2),  # Use 'region' for country names
            size = 3, color = "black", fontface = "bold") +
  scale_fill_gradient(
    low = "grey94", 
    high = "grey45", 
    na.value = "white", 
    breaks = seq(0, max(merged_data$MilletProductionMT, na.rm = TRUE), by = 1000),  # Custom breaks
    labels = label_comma(),  # Format labels
    limits = c(0, max(merged_data$MilletProductionMT, na.rm = TRUE))  # Set limits
  ) +
  theme(
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.position = "bottom"
  ) +
  guides(fill = guide_colorbar(title = "Millet Production 1000 MT 2024",barwidth = 15, barheight = 1.5, vjust = 0.01)) +
  theme(legend.text = element_text(angle = 45))

# Display the map
Millet2024production_mapped_africa


