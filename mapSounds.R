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
library(viridis) # color themes for color blind people


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
  # this joins gives the most rows (and highlights missing sets or sites)
  # may need to QC the results based on what is missing, before selecting columns
  full_join(., sets, by = c("site_id", "timezone")) %>%
  full_join(., fish, by = "fishingset_id") %>%
  mutate(MONTH_DISPLAY = month(date, label = TRUE, abbr = TRUE)) %>%
  select(sound, species, start_latitude, start_longitude, 
         end_latitude, end_longitude, fishingset_id,
         hook_depth_min, hook_depth_max, hooks_used, MONTH_DISPLAY)

mapdf$MONTH_DISPLAY <- factor(mapdf$MONTH_DISPLAY,
                             levels = c("Sep", "Oct", "Nov", "Dec",
                                        "Jan", "Feb", "Mar", "Apr", "May"))

###############
# load data
###############

# find and save data as csv that is outside the map ranges
# check coordinates, or
# expand range in mapping function, if valid coordinates

qcCoordinates <- mapdf %>%
  mutate(qcNeeded = case_when(
    sound == "CLAYOQUOT" & (start_latitude < 49.1 | start_latitude > 49.5) ~ "y",
    sound == "CLAYOQUOT" & (start_longitude > -125.6 | start_longitude < -126.3) ~ "y",
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
if (!dir.exists("Data")) dir.create("Data")

# save data to investigate
fileName <- str_c("Data/qcCoordinates_", Sys.Date(), ".csv", sep = "")
write_csv(qcCoordinates, fileName)

############################################
# create function to map each sound sampled
############################################
mapSoundfn <- function(soundName, mapdf, resolution, 
                       thisSpecies) {
  
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
    maxLon <- -125.6
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

    #use for high res map sucha s for final
    basemap <- ggplot() +
      geom_sf(data = bchigh, fill = "grey80") +
      geom_sf(data = bcn1) +
      xlim(minLon, maxLon) +
      ylim(minLat, maxLat) +
      xlab("Longitude") + ylab("Latitude") +
      theme_bw() 
  } 
  
  if (resolution == "low") {
    
    bclow <- bc_bound()
    bclow <- bclow %>% st_transform(4326)

    #low res version of bcmaps - good for quicker drafts
    basemap <- ggplot() +
      geom_sf(data = bclow, fill = "grey80") +
      geom_sf(data = bcn1) +
      xlim(minLon, maxLon) +
      ylim(minLat, maxLat) +
      xlab("Longitude") + 
      ylab("Latitude") +
      theme_bw() 
    
  }
  
  if (thisSpecies == "CK" & length(unique(df_ck$MONTH_DISPLAY)) > 1) {
  # map in hexogonal binswith count of fish per bin
  thismap <- basemap +
    geom_point(data = df_no,
               aes(start_longitude, start_latitude),
               shape = 3) +
    stat_binhex(data = df_ck,
                aes(start_longitude, start_latitude),
                alpha = 0.7) +
    scale_fill_viridis() +
    #scale_fill_gradient(low = "darkblue", high = "red") +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          strip.background = element_rect(fill = "white")) +
    labs(fill = "Count",
         subtitle = str_c(str_to_title(soundName), " Chinook Salmon")) +
    facet_wrap(~MONTH_DISPLAY, ncol = 2)
    
  }
  
  if (thisSpecies == "CK" & length(unique(df_ck$MONTH_DISPLAY)) == 1) {

    thislabel <- str_c(unique(df_ck$MONTH_DISPLAY), " Only")
    
    # map in hexogonal binswith count of fish per bin
    thismap <- basemap +
      geom_point(data = df_no,
                 aes(start_longitude, start_latitude),
                 shape = 3) +
      stat_binhex(data = df_ck,
                  aes(start_longitude, start_latitude),
                  alpha = 0.7) +
      scale_fill_viridis() +
      #scale_fill_gradient(low = "darkblue", high = "red") +
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            strip.background = element_rect(fill = "white")) +
      labs(fill = "Count",
           subtitle = str_c(str_to_title(soundName), " Chinook Salmon")
           ) +
      geom_label(aes(x = maxLon, y = maxLat , label = thislabel),
                 hjust = "inward", 
                 vjust = "inward",
                 fill = "white")
  }
  
  if (thisSpecies == "CO" & length(unique(df_co$MONTH_DISPLAY)) > 1) {
  thismap <- basemap +
    geom_point(data = df_no,
               aes(start_longitude, start_latitude),
               shape = 3) +
    stat_binhex(data = df_co,
                aes(start_longitude, start_latitude),
                alpha = 0.7) +
    scale_fill_viridis() +
    #scale_fill_gradient(low = "darkblue", high = "red") +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          strip.background = element_rect(fill = "white")) +
    labs(fill = "Count",
         subtitle = str_c(str_to_title(soundName), " Coho Salmon")
         ) +
    facet_wrap(~ MONTH_DISPLAY, ncol = 2)
  }
  
  if (thisSpecies == "CO" & length(unique(df_co$MONTH_DISPLAY)) == 1) {
    
    thislabel <- str_c(unique(df_co$MONTH_DISPLAY), " Only")
    
    thismap <- basemap +
      geom_point(data = df_no,
                 aes(start_longitude, start_latitude),
                 shape = 3) +
      stat_binhex(data = df_co,
                  aes(start_longitude, start_latitude),
                  alpha = 0.7) +
      scale_fill_viridis() +
      #scale_fill_gradient(low = "darkblue", high = "red") +
      # theme(axis.text = element_blank(),
      #       axis.title = element_blank(),
      #       strip.background = element_rect(fill = "white")) +
      labs(fill = "Count",
           subtitle = str_c(str_to_title(soundName), " Coho Salmon")
                            ) +
      geom_label(aes(x = maxLon, y = maxLat , label = thislabel),
                      hjust = "inward", 
                      vjust = "inward",
                      fill = "white")
  }
  
  if (exists("thismap") == TRUE) {
  return(thismap)
  }
}


###############
# make maps
###############
# create Figure folder if it doesn't exist yet
if (!dir.exists("Figures")) dir.create("Figures")

# create function to make maps and save for each species and sound
# ***need to incorporate auto sizing depending on number of maps
mapAndSavefn <- function(soundName, binNumber) {
  
  finalmapCK <- mapSoundfn(soundName, mapdf, "high", "CK")
  if (!is.null(finalmapCK)) {
  plotnameCK <- str_c(str_to_title(soundName), "CKmap.png")
  ggsave(plotnameCK, finalmapCK, path = "Figures", dpi = 300)
  }

  finalmapCO <- mapSoundfn(soundName, mapdf, "high", "CO")
  if (!is.null(finalmapCK)) {
  plotnameCO <- str_c(str_to_title(soundName), "COmap.png")
  ggsave(plotnameCO, finalmapCO, path = "Figures", dpi = 300)
  }
  
}

# only make maps if they have fished
whereFishedvec <- mapdf %>%
  group_by(sound) %>%
  count() %>%
  filter(!(is.na(sound))) %>%
  pull(sound)

# make maps in areas fished and save
map(whereFishedvec, mapAndSavefn)

# list warnings for removed coordinates
# if sourcing the code rather than stepping thru code
warnings()

##############################
