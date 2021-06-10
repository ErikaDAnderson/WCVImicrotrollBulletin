#=====================================================================================================
# Script Name: mapSounds.R
# Script Author: Erika Anderson
# Script Start Date: 2021-04
# R Version: 3.6.1
#
# Create maps salmon caught in up to five WCVI sounds
# use to QC data coordinates 
# maps used InSeasonDataUpdate.Rmd
# manually adjusted in 2020/2021 since different fishing effort
# 
#
#=====================================================================================================

# load libraries
library(tidyverse) # basic data manipulation
library(readxl) # read excel files
library(lubridate) # date times
library(bcmaps) # bc basemaps
library(hexbin) # stat_binhex() function
library(viridis) # color themes for color blind people
library(cowplot) # arrange plots aligned with and without legends


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
  full_join(., sets, by = "site_id") %>%
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
                       thisSpecies, binNumCK, binNumCO) {
  
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
  
  if (thisSpecies == "CK" & length(unique(df_no$MONTH_DISPLAY)) > 1) {
  # map in hexogonal bins with count of fish per bin
  thismap <- basemap +
    geom_point(data = df_no,
               aes(start_longitude, start_latitude),
               shape = 3) +
    stat_binhex(data = df_ck,
                aes(start_longitude, start_latitude),
                alpha = 0.7,
                binwidth = binNumCK) +
    scale_fill_viridis() +
    #scale_fill_gradient(low = "darkblue", high = "red") +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          strip.background = element_rect(fill = "white"),
          panel.background = element_blank()) +
    labs(fill = "Count",
         subtitle = " Chinook Salmon") +
    facet_wrap(~MONTH_DISPLAY, ncol = 2)
  }
  
  if (thisSpecies == "CK" & length(unique(df_no$MONTH_DISPLAY)) == 1) {

    thislabel <- unique(df_ck$MONTH_DISPLAY)
    
    # map in hexogonal binswith count of fish per bin
    thismap <- basemap +
      geom_point(data = df_no,
                 aes(start_longitude, start_latitude),
                 shape = 3) +
      stat_binhex(data = df_ck,
                  aes(start_longitude, start_latitude),
                  alpha = 0.7,
                  binwidth = binNumCK) +
      scale_fill_viridis() +
      #scale_fill_gradient(low = "darkblue", high = "red") +
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            strip.background = element_rect(fill = "white"),
            panel.background = element_blank()) +
      labs(fill = "Count",
           subtitle = " Chinook Salmon") +
      geom_label(aes(x = maxLon, y = maxLat , label = thislabel),
                 hjust = "inward", 
                 vjust = "inward",
                 fill = "white")
  }
  
  if (thisSpecies == "CO" & length(unique(df_no$MONTH_DISPLAY)) > 1 & nrow(df_co) > 1) {
  thismap <- basemap +
    geom_point(data = df_no,
               aes(start_longitude, start_latitude),
               shape = 3) +
    stat_binhex(data = df_co,
                aes(start_longitude, start_latitude),
                alpha = 0.7,
                binwidth = binNumCO) +
    scale_fill_viridis() +
    #scale_fill_gradient(low = "darkblue", high = "red") +
    theme(axis.text = element_blank(),
          axis.title = element_blank()) +
    labs(fill = "Count",
         subtitle = " Coho Salmon") +
    facet_wrap(~ MONTH_DISPLAY, ncol = 2)
  }
  
  if (thisSpecies == "CO" & length(unique(df_no$MONTH_DISPLAY)) > 1 & nrow(df_co) == 0) {
    thismap <- basemap +
      geom_point(data = df_no,
                 aes(start_longitude, start_latitude),
                 shape = 3) +
      stat_binhex(data = df_co,
                  aes(start_longitude, start_latitude),
                  alpha = 0.7,
                  binwidth = binNumCO) +
      scale_fill_viridis() +
      #scale_fill_gradient(low = "darkblue", high = "red") +
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            strip.background = element_rect(fill = "white"),
            panel.background = element_blank()) +
      labs(fill = "Count",
           subtitle = " Coho Salmon") +
      facet_wrap(~ MONTH_DISPLAY, ncol = 2)
  }
  
  if (thisSpecies == "CO" & length(unique(df_no$MONTH_DISPLAY)) > 1 & nrow(df_co) == 1) {
    
    thismap <- basemap +
      geom_point(data = df_no,
                 aes(start_longitude, start_latitude),
                 shape = 3) +
      geom_point(data = df_co,
                  aes(start_longitude, start_latitude),
                  alpha = 0.7,
                 # use findcolor <- ggplot_build(finalmapCK)$data
                 # to find color used in chinook plot
                 color = "#440154",
                 size = 5) +
      #scale_fill_viridis() +
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            strip.background = element_rect(fill = "white"),
            panel.background = element_blank()) +
      labs(fill = "Count",
           subtitle = " Coho Salmon",
           caption = "Only one coho salmon caught in November") +
      facet_wrap(~ MONTH_DISPLAY, ncol = 2)
  }
  
  if (thisSpecies == "CO" & length(unique(df_no$MONTH_DISPLAY)) == 1) {
    
    thislabel <- unique(df_no$MONTH_DISPLAY)
    
    thismap <- basemap +
      geom_point(data = df_no,
                 aes(start_longitude, start_latitude),
                 shape = 3) +
      stat_binhex(data = df_co,
                  aes(start_longitude, start_latitude),
                  alpha = 0.7,
                  binwidth = binNumCO) +
      scale_fill_viridis() +
      #scale_fill_gradient(low = "darkblue", high = "red") +
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            strip.background = element_rect(fill = "white"),
            panel.background = element_blank()) +
      labs(fill = "Count",
           subtitle = " Coho Salmon") +
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

# this is fussy and needs more automation
# for now need to adjust based on the data
# create function to make maps and save for each species and sound
mapAndSavefn <- function(soundName, mapdf, 
                         binNumCK, binNumCO, thisheight) {
  
  finalmapCK <- mapSoundfn(soundName, mapdf, "high", "CK", 
                           binNumCK, binNumCO)
  
  finalmapCO <- mapSoundfn(soundName, mapdf, "high", "CO", 
                           binNumCK, binNumCO)
  
  if (soundName %in% c("NOOTKA", "KYUQUOT")) {
    theme_set(theme_minimal())
    
    finalmap <- cowplot::plot_grid(
      plot_grid(
        finalmapCK + theme(legend.position = "none"),
        finalmapCO,
        ncol = 1,
        align = "hv"),
      plot_grid(
        get_legend(finalmapCK),
        ggplot(),
        ncol = 1),
      rel_widths = c(8,2)
    )
    
    plotname <- str_c("Figures/", str_to_title(soundName), "map.png")
    
    if (!is.null(finalmap)) {
      ggsave(plotname, finalmap, 
             dpi = 300, height = thisheight)
    } else {
      file.remove(plotname)
    }
  }
  
  if (soundName %in% c("BARKLEY")) {
    
    finalmap <- cowplot::plot_grid(
      finalmapCK,
      finalmapCO,
      ncol = 1,
      align = "hv")
    
    plotname <- str_c("Figures/", str_to_title(soundName), "map.png")
    
    if (!is.null(finalmap)) {
      ggsave(plotname, finalmap, 
             dpi = 300, height = thisheight)
    } else {
      file.remove(plotname)
    }
    
  }
  
  if (soundName %in% c("CLAYOQUOT", "QUATSINO")) {
    
    plotnameCK <- str_c("Figures/", str_to_title(soundName), "CKmap.png")
    
    if (!is.null(finalmapCK)) {
      ggsave(plotnameCK, finalmapCK, 
             dpi = 300, height = thisheight)
    } else {
      file.remove(plotnameCK)
    }
    
    plotnameCO <- str_c("Figures/", str_to_title(soundName), "COmap.png")
    
    if (!is.null(finalmapCO)) {
      ggsave(plotnameCO, finalmapCO, 
             height = thisheight)
    } else {
      file.remove(plotnameCO)
    }
  }
}

# adjust binwidth based on data if hex bins too big or small
# adjust height to remove whitespace
mapAndSavefn("QUATSINO", mapdf, 
             c(0.07, 0.026), c(0.07, 0.026), 5)
mapAndSavefn("NOOTKA", mapdf,  
             c(0.035, 0.03), c(0.035, 0.03), 6)
mapAndSavefn("KYUQUOT", mapdf, 
             c(0.03, 0.02), c(0.03, 0.02), 6)
mapAndSavefn("CLAYOQUOT", mapdf, 
             c(0.07, 0.04), c(0.007, 0.004), 8.5)
mapAndSavefn("BARKLEY", mapdf, 
             c(0.06, 0.02), c(0.05, 0.016), 6)

# # function to make maps and save for each species and sound
# # ***need to incorporate auto sizing depending on number of maps
# mapAndSavefn <- function(soundName, mapdf, 
#                           binNumCK, binNumCO, thisheight) {
#   
#   finalmapCK <- mapSoundfn(soundName, mapdf, "high", "CK")
#   plotnameCK <- str_c("Figures/", str_to_title(soundName), "CKmap.png")
#   
#   if (!is.null(finalmapCK)) {
#     ggsave(plotnameCK, finalmapCK, dpi = 300)
#   } else {
#     file.remove(plotnameCK)
#   }
#   
#   finalmapCO <- mapSoundfn(soundName, mapdf, "high", "CO")
#   plotnameCO <- str_c("Figures/", str_to_title(soundName), "COmap.png")
#   
#   if (!is.null(finalmapCO)) {
#     ggsave(plotnameCO, finalmapCO, dpi = 300)
#   } else {
#     file.remove(plotnameCO)
#   }
#   
# }
# 
# # only make maps if they have fished
# whereFishedvec <- mapdf %>%
#   group_by(sound) %>%
#   count() %>%
#   filter(!(is.na(sound))) %>%
#   pull(sound)
# 
# # make maps in areas fished and save
# map(whereFishedvec, mapAndSavefn)

# list warnings for removed coordinates
# if sourcing the code rather than stepping thru code
warnings()

##############################