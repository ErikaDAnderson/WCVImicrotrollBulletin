#=====================================================================================================
# Script Name: mapSounds.R
# Script Author: Erika Anderson
# Script Start Date: 2021-04
# R Version: 3.6.1
#
# Create maps salmon caught in up to five WCVI sounds
# use to QC data coordinates 
# maps used InSeasonDataUpdate.Rmd
#
#=====================================================================================================

# load libraries
library(tidyverse) # basic data manipulation
library(readxl) # read excel files
library(lubridate) # date times
library(bcmaps) # bc basemaps
library(hexbin) # stat_binhex() function
library(patchwork) # group plots together

###############
# load data
###############

# create function to load data from excel file(s)
loadExcelFn <- function(sheetName) {
  
  list.files(path = "./Data/",
             pattern = "*.xlsx", 
             full.names = T) %>% 
    map_df(~read_excel(., sheet = sheetName)) 
  
}

# load data from all excel files in Data folder
site_orig <- loadExcelFn("01 SITE")
sets_orig <- loadExcelFn("02 FISHING SETS") 
fish_orig <- loadExcelFn("04 FISH")

# tidy data
site <- site_orig %>%
  # remove extra rows generated from NAs
  filter(!is.na(date)) %>%
  # remove default date given by excel in background
  mutate(time = str_sub(time, -8, -1),
         datetime = ymd_hms(str_c(date, time, sep = " ")))

sets <- sets_orig %>%
  mutate(end_time = str_sub(end_time, -8, -1)) 

fish <- fish_orig 

# join map data
mapdf <- site %>%
  # this joins gives the most rows
  # may need to QC the results based on what is missing, before selecting columns
  full_join(., sets, by = c("site_id", "timezone")) %>%
  full_join(., fish, by = "fishingset_id") %>%
  select(sound, species, start_latitude, start_longitude, 
         end_latitude, end_longitude, fishingset_id,
         hook_depth_min, hook_depth_max, hooks_used)

###############
# load data
###############

# find and save data as csv that it outside the map ranges
# check coordinates, or
# expand range in mapping function, if valid coordinates

qcCoordinates <- mapdf %>%
  mutate(qcNeeded = case_when(
    sound == "CLAYOQUOT" & (start_latitude < 49.1 | start_latitude > 49.5) ~ "y",
    sound == "CLAYOQUOT" & (start_longitude > -125.7 | start_longitude < -126.3) ~ "y",
    sound == "QUATSINO" & (start_latitude < 50.4 | start_latitude > 50.65) ~ "y",
    sound == "QUATSINO" & (start_longitude > -127.4 | start_longitude < -128.1) ~ "y",
    sound == "BARKLEY" & (start_latitude < 48.75 | start_latitude > 49.1) ~ "y",
    sound == "BARKLEY" & (start_longitude > -124.9 | start_longitude < -125.5) ~ "y",
    sound == "NOOTKA" & (start_latitude < 49.5 | start_latitude > 49.85) ~ "y",
    sound == "NOOTKA" & (start_longitude > -126.0 | start_longitude < -126.8) ~ "y",
    sound == "KYUQUOT" & (start_latitude < 49.9 | start_latitude > 50.2) ~ "y",
    sound == "KYUQUOT" & (start_longitude > -127.0 | start_longitude < -127.4) ~ "y",
    TRUE ~ "n"
  )) %>%
  filter(qcNeeded == "y") %>%
  select(sound, fishingset_id, start_latitude, start_longitude) %>%
  distinct()

# create Figure folder if it doesn't exist yet
if (!dir.exists("Output")) dir.create("Output")

# save data to investigate
fileName <- str_c("Output/qcCoordinates_", Sys.Date(), ".csv", sep = "")
write_csv(qcCoordinates, fileName)

############################################
# create function to map each sound sampled
############################################
mapSoundfn <- function(soundName, mapdf, resolution) {
  
  # df <- mapdf %>%
  #   filter(sound == toupper(soundName)) %>%
  #   filter(!(is.na(start_latitude))) %>%
  #   filter(!(is.na(start_longitude))) %>%
  #   group_by(fishingset_id, start_latitude, start_longitude, 
  #            end_latitude, end_longitude, hook_depth_min, hook_depth_max,
  #            hooks_used, species) %>%
  #   summarize(hooks = sum(hooks_used, na.rm = TRUE),
  #             catch = n(),
  #             catchByhook = catch/hooks,
  #             .groups = 'drop')
  
  df <- mapdf %>%
    filter(sound == toupper(soundName)) %>%
    filter(!(is.na(start_latitude))) %>%
    filter(!(is.na(start_longitude))) 
  
  # use standard min and max lat and longs for limits to basemap for each sound
  # adjust if needed, if fishing expands outside these limits
  if (soundName == "CLAYOQUOT") {
    minLat <- 49.1
    maxLat <- 49.5
    minLon <- -126.3
    maxLon <- -125.7
  }
  
  if (soundName == "QUATSINO") {
    minLat <- 50.4
    maxLat <- 50.65
    minLon <- -128.1
    maxLon <- -127.4
  }
  
  if (soundName == "BARKLEY") {
    minLat <- 48.75
    maxLat <- 49.1
    minLon <- -125.5
    maxLon <- -124.9
  }
  
  if (soundName == "NOOTKA") {
    minLat <- 49.5
    maxLat <- 49.85
    minLon <- -126.8
    maxLon <- -126.0
  }
  
  # update this when data comes in
  if (soundName == "KYUQUOT") {
    minLat <- 49.9
    maxLat <- 50.2
    minLon <- -127.4
    maxLon <- -127.0
  }
  
  # df for each species
  # df_ck <- df %>% filter(species == "CN" | is.na(species))
  # df_co <- df %>% filter(species == "CO" | is.na(species))
  df_ck <- df %>% filter(species == "CN")
  df_co <- df %>% filter(species == "CO")
  df_no <- df %>% filter(is.na(species))
  
  
  ##  Mapping data from bcmaps
  bcn <- bc_neighbours()
  bcn <- bcn %>% st_transform(4326)
  bcn1 <- bcn %>%
    filter(name != "British Columbia") %>%  
    filter(name != "Pacific Ocean")
  
  
  if (resolution == "high") {
    
    bchigh <- bc_bound_hres()
    bchigh <- bchigh %>% st_transform(4326)
    waterlow <- watercourses_15M()
    water15 <- waterlow %>% st_transform(4326)
    
    #use for high res map sucha s for final
    basemap <- ggplot() +
      geom_sf(data = bchigh, fill = "lightyellow1") +
      geom_sf(data = water15, color = "blue") +
      geom_sf(data = bcn1) +
      xlim(minLon, maxLon) +
      ylim(minLat, maxLat) +
      xlab("Longitude") + ylab("Latitude") +
      theme_bw() +
      theme(panel.background = element_rect(fill = "lightcyan2"))
  } 
  
  if (resolution == "low") {
    
    bclow <- bc_bound()
    bclow <- bclow %>% st_transform(4326)

    #low res version of bcmaps - good for quicker drafts
    basemap <- ggplot() +
      geom_sf(data = bclow, fill = "lightyellow1") +
      geom_sf(data = bcn1) +
      xlim(minLon, maxLon) +
      ylim(minLat, maxLat) +
      xlab("Longitude") + 
      ylab("Latitude") +
      theme_bw() +
      theme(panel.background = element_rect(fill = "lightcyan2"))
    
  }
  
  # map in hexogonal binswith count of fish per bin
  map_ck <- basemap +
    geom_point(data = df_no,
               aes(start_longitude, start_latitude),
               shape = 3) +
    stat_binhex(data = df_ck,
                aes(start_longitude, start_latitude),
                alpha = 0.7) +
    scale_fill_gradient(low = "darkblue", high = "red") +
    theme(axis.text = element_blank(),
          axis.title = element_blank()) +
    labs(fill = "count",
         subtitle = "Chinook Salmon") 
  
  map_co <- basemap +
    geom_point(data = df_no,
               aes(start_longitude, start_latitude),
               shape = 3) +
    stat_binhex(data = df_co,
                aes(start_longitude, start_latitude),
                alpha = 0.7) +
    scale_fill_gradient(low = "darkblue", high = "red") +
    theme(axis.text = element_blank(),
          axis.title = element_blank()) +
    labs(fill = "count",
         subtitle = "Coho Salmon") 

  plottitle <- str_c(str_to_title(soundName), "Sound", sep = " ")
  
  map_ck + map_co + plot_annotation(title = plottitle)
  
}


###############
# make maps
###############
# create Figure folder if it doesn't exist yet
if (!dir.exists("Figures")) dir.create("Figures")

## future addition to run through all sounds in dataset
#sound_vec <- unique(site$sound)
#allMaps <- sound_vec %>%
#  map(mapSoundfn, mapdf = mapdf, resolution = "high")


# create maps individually and save at lower resolution
clayoquotMap <- mapSoundfn("CLAYOQUOT", mapdf, "high")
ggsave("clayoquotMap.png", clayoquotMap, path = "Figures", height = 4, units = "in", dpi = 300)

quatsinoMap <- mapSoundfn("QUATSINO", mapdf, "high")
ggsave("quatsinoMap.png", quatsinoMap, path = "Figures", height = 3, units = "in", dpi = 300)

barkleyMap <- mapSoundfn("BARKLEY", mapdf, "high")
ggsave("barkleyMap.png", barkleyMap, path = "Figures", height = 3.5, units = "in", dpi = 300)

nootkaMap <- mapSoundfn("NOOTKA", mapdf, "high")
ggsave("nootkaMap.png", nootkaMap, path = "Figures", height = 3, units = "in", dpi = 300)

kyuquotMap <- mapSoundfn("KYUQUOT", mapdf, "high")
ggsave("kyuquotMap.png", kyuquotMap, path = "Figures", height = 3, units = "in", dpi = 300)

# list warnings for removed coordinates if sourcing the code rather than stepping thru code
warnings()

##############################
