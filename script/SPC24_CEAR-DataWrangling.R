## DATA CLEANING FOR NEW 2024 NCRMMP

rm(list = ls())
library(dplyr)
library(lubridate)

load("/Users/mayaotsu/Downloads/ALL_REA_FISH_RAW.rdata")
load("/Users/Kisei.Tanaka/fish-paste/data/ALL_REA_FISH_RAW.rdata")
select=dplyr::select
colnames(df)

#species <- c("LUKA", "CEAR", "LUFU")[2]
#nSPC <- c("nSPC")

#rename columns 
df <- df %>%
  rename(
    island = "ISLAND", depth = "DEPTH", method = "METHOD", date_ = "DATE_", latitude = "LATITUDE", longitude = "LONGITUDE", 
    species= "SPECIES", density = "DENSITY", count = "COUNT", region = "REGION")

#transform lon
df$longitude = ifelse(df$longitude < 0, df$longitude + 360, df$longitude)
#lat and lon decimal places
df <- df %>%
  mutate(longitude = round(longitude, 4),
         latitude = round(latitude, 5))

#add year, month,day columns
df <- df %>%
  mutate(
    year = year(date_),
    month = month(date_),
    day = day(date_)
  )

df$x = ifelse(df$species == "CEAR", df$density, 0)

#make a presence column
df = df %>%
  #mutate(x = ifelse(species == "CEAR", density, 0)) %>% 
  group_by(island, depth, method, date_, latitude, longitude, region, year, month, day) %>%  # Grouping by year before summarizing
  summarise(density = sum(x, na.rm = TRUE)) #%>% 
 # mutate(presence = ifelse(x > 0, 1, 0))

df$presence = ifelse(df$density>0,1,0)
df$species <- "CEAR"

df %>% 
  filter(region %in% c("MHI", "NWHI")) %>% 
  filter(species == "CEAR") %>% 
  filter(method == "nSPC") %>% 
  ggplot(aes(longitude, latitude, fill = as.factor(presence))) + 
  geom_point(shape = 21, size = 5, alpha = 0.8) + 
  facet_wrap(~year)

#filter out for only CEAR, islands, and spc in method
df <- df %>%
  subset(region %in% c("MHI", "NWHI") & method == "nSPC" & species == "CEAR") %>%
  select("island", "depth", "method", "date_", "latitude", 
         "longitude", "species", "density", "presence", "region",
         "year", "month", "day") 

#save .RData
#save(df, file ="/Users/mayaotsu/Downloads/calibr_CEAR_abund.RData")
saveRDS(df, file = "/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_edited_CEAR")
save(df, file ="/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/SPC25_CEAR.RData")

############### END ###################

