## Simulation of flight paths


# Setup -------------------------------------------------------------------
install.load::install_load("rgdal", "rgeos", "sp","trajr", "sf", "tidyverse")

# Set Detectors & Generate Trajectories --------------------------------------------------------------------

detectors_sf <-
  tibble(
    detector = as.character(c(1, 2, 3)),
    x = c(20,40, 60),
    y = c(0,0,0)
  ) %>%
  st_as_sf(coords = c("x", "y"), remove = FALSE) %>%
  st_buffer(5)

trj_sf_medium <-
  TrajGenerate(
    100,
    random = FALSE,
    angularErrorSd = 0.05,
    stepLength = 1,
    fps = 5 # speed: 5m/s = 18km/h
  ) %>%
  mutate(species = "medium",
         individual = "a") %>%
  st_as_sf(coords = c("x", "y"), remove = FALSE)
  
trj_sf_fast <-
  TrajGenerate(
    100,
    random = FALSE,
    angularErrorSd = 0.05,
    stepLength = 1,
    fps = 8 # speed: 8m/s = 28.8km/h
  ) %>%
  mutate(species = "fast",
         individual = "b",
         y = y+2) %>%
  st_as_sf(coords = c("x", "y"), remove = FALSE)

trj_sf <- rbind(trj_sf_medium, trj_sf_fast)

ggplot() +
  geom_sf(data = detectors_sf) +
  geom_line(data = trj_sf, aes(x,y, color = individual))


# Virtually record Calls  ---------------------------------------------

# st_intersects (with sparse = F) returns a logical vector of points that intersect a detector (row per detector)
recordings<-
  st_intersects(detectors_sf, trj_sf, sparse = F) %>%
  split(1:nrow(.)) %>%
  enframe(name = "detector", value = "intersected") %>%
  mutate(recordings = map(intersected, ~as.data.frame(trj_sf)[.x, ])) %>%
  select(-intersected) %>%
  unnest() %>%
  ## basically emulate the setting, that a batcorder will record as long as a bat is audible 
  select(detector, time, species, individual) %>%
  group_by(detector, species, individual) %>%
  filter(time == min(time)) %>%
  left_join(st_set_geometry(detectors_sf, NULL))
  ## alternatively, round times to emulate a fixed recording time; will result in more detections
  # mutate(time = round(time/2)*2) %>% #round to nearest even number
  # select(detector, time, species, individual) %>%
  # distinct()


# Trace Trajectories ------------------------------------------------------

# get for every recording/row the recordings of the same species at a different detector at some delta t and put them in same rows
traced <-
  recordings %>%
  ungroup() %>%
  arrange(time) %>%
  split(1:nrow(.)) %>%
  map_dfr(function(x) {
    p <- x
    names(p) <- paste0(names(x), "_start")
    
    # filter for matching records
    q <- filter(
      recordings,
      !detector == p$detector_start,
      species == p$species_start,
      #time > p$time_start
      between(time, p$time_start, p$time_start + 60)
    ) %>%
      ungroup()
    names(q) <- paste0(names(q), "_end")
    
    q %>% add_column(!!!p, .before = 1)
    ## alternatively
    # p %>%
    #   slice(rep(1, each = nrow(q))) %>%
    #   add_column(!!!q)
  }) %>%
  # add time difference, distance and speed between records
  mutate(diff_time = time_end - time_start,
         distance = sqrt((x_start - x_end)^2 + (y_start - y_end)^2),
         speed = distance/diff_time)
  


# Plausibility ------------------------------------------------------------


