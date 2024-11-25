library(pacman)
remotes::install_github("grimbough/FITfileR")
pacman::p_load(ggmap, osmdata, geosphere, dplyr,lubridate, FITfileR, purrr, zoo,
               tidyverse, rjson, rlist, gpx, xml2)

#INPUT: path to .fit file
#OUTPUT: dataframe object with contents of file
fit <- function(path) {
  fit_file <- readFitFile(path)
  data <- fit_file %>%
    records() %>%
    reduce(function(x, y) merge(x, y, all = TRUE)) %>%
    subset(select = c(timestamp, position_lat, position_long, enhanced_altitude, speed, distance))
  mutate(data, sport = (fit_file %>%
                    getMessagesByType(message_type = "session"))$sport) %>%
  tryCatch(error = function(e) {
    mutate(data, sport = "running")
  })
}

#INPUT: path to .gpx file
#OUTPUT: dataframe object with contents of file
gpx <- function(path) {
  read_gpx(path)$tracks[[1]] %>%
    rename(ele = Elevation,
           lon = Longitude,
           lat = Latitude,
           time = Time
           ) %>%
    arrange(time) %>%
    tryCatch(error = function(e) {
      pts <- xml2::read_html("input/10k/run.gpx") %>%
        xml_find_all("//trkpt")
      latlng <- xml_attrs(pts) %>%
        unlist(use.names = TRUE)
      lat <- as.numeric(latlng[names(latlng)=="lat"])
      lon <- as.numeric(latlng[names(latlng)=="lon"])
      ele <- xml_child(pts, "ele") %>%
        xml_text() %>%
        as.numeric()
      time <- xml_child(pts, "time") %>%
        xml_text() %>%
        as_datetime()
      data.frame(cbind(time, lat, lon, ele)) %>%
        mutate(time = as_datetime(time))
    })
}

#INPUT: path to .json file
#OUTPUT: dataframe object with contents of file
json <- function(path) {
  orig <- fromJSON(file=path)
  fields <- orig$data[[1]]$fields %>%
    lapply(function (x) if (x == "latlng") {list("position_lat", "position_long")} else {x}) %>%
    unlist()
  orig$data[[1]]$values %>%
    unlist() %>%
    matrix(nrow = length(fields), dimnames = list(fields, NULL)) %>%
    t() %>%
    as.data.frame() %>%
    mutate(sport = orig[[1]]$activity_type,
           timestamp = as_datetime(round(time))) %>%
    rename(enhanced_altitude = elevation) %>%
    subset(select = c(timestamp, position_lat, position_long, enhanced_altitude, speed, distance, sport))
  
}


#INPUT: path to file of type .fit or .json
#OUTPUT: call to either fit() or json() for given filepath
orig <- function(path) {
  if(length(path) == 0) {
    NULL
  }
  else if (grepl("\\.fit$", path)) {
    fit(path)
  } else if (grepl("\\.json$", path)) {
    json(path)
  } else {
    NULL
  }
}

#INPUT: list of longitudes and latitudes
#OUTPUT: distances between consecutive locations
distList <- function(lon, lat) {
  df <- data.frame(lon, lat)
  distGeo(df, df %>% mutate_all(function(a) lag(a, default = a[1])))
}


#atj <- orig("input/3189596489/Apocalypse_Training_1.json")
#mfit <- orig("input/manchester/Manchester_Marathon.fit")
#mfit_raw <- readFitFile("input/manchester/Manchester_Marathon.fit")
#atg <- gpx("input/3189596489/Apocalypse_Training_1.gpx")
#inner_join(atj, atg, by = c("timestamp" = "time"))

#INPUT: path to GPX file and to JSON/FIT file, and a session name
#OUTPUT: dataframe with merged info from two files, plus some calculations
#        of distance, grade, etc
merged <- function(gpx_path, orig_path, session) {
  (if (!is.null(orig(orig_path)) && !is.null(gpx(gpx_path))) {
    inner_join(orig(orig_path), gpx(gpx_path), by = c("timestamp" = "time")) %>%
      mutate(dist_orig = distGeo(data.frame(position_long, position_lat), c(position_long[[1]], position_lat[[1]]))) %>%
      subset(select = -c(lon, lat, enhanced_altitude)) %>%
      mutate(ele = rollmean(ele, k=3, fill = c(ele[1], 0, ele[length(ele)])), 
             distance_step = distList(position_long, position_lat),
             grade = (ele - lag(ele, default = ele[1])) / distance_step,
             pace = (1000 / 60) / speed,
             elapsed_time = as.numeric(difftime(timestamp, min(timestamp))),
             session = session)
  } else if (is.null(gpx(gpx_path))) {
    orig(orig_path) %>%
      mutate(dist_orig = distGeo(data.frame(position_long, position_lat), c(position_long[[1]], position_lat[[1]]))) %>%
      mutate(ele = rollmean(enhanced_altitude, k=3, fill = c(enhanced_altitude[1], 0, enhanced_altitude[length(enhanced_altitude)])), 
             distance_step = distList(position_long, position_lat),
             grade = (ele - lag(ele, default = ele[1])) / distance_step,
             pace = (1000 / 60) / speed,
             elapsed_time = as.numeric(difftime(timestamp, min(timestamp))),
             session = session) %>%
      subset(select = -c(enhanced_altitude))
  } else {
    gpx(gpx_path) %>%
      mutate(dist_orig = distGeo(data.frame(lon, lat), c(lon[[1]], lat[[1]]))) %>%
      mutate(ele = rollmean(ele, k=3, fill = c(ele[1], 0, ele[length(ele)])), 
             distance_step = distList(lon, lat),
             distance = cumsum(distance_step),
             grade = (ele - lag(ele, default = ele[1])) / distance_step,
             pace = 0,
             gps_accuracy = 0,
             speed = 0,
             sport = "running",
             elapsed_time = as.numeric(difftime(time, min(time))),
             session = session) %>%
      rename(timestamp = time,
             position_lat = lat,
             position_long = lon)
  }) %>%
  subset(select = c(position_long, position_lat, ele, timestamp, dist_orig, distance_step, distance, grade, pace, speed, sport, elapsed_time, session))
}

#INPUT: directory containing one or more recording files for a single activity
#OUTPUT: dataframe with combined data from all files for this activity
merged_from_dir <- function(dir) {
  gpx_files <- list.files(path = dir, pattern = "\\.gpx$", full.names = TRUE)
  orig_files <- list.files(path = dir, pattern = "\\.fit$", full.names = TRUE) %>%
    append(list.files(path = dir, pattern = "\\.json$", full.names = TRUE))
  merged(gpx_files,orig_files, dir) %>%
    tryCatch(error = function(e) {
      paste("Failed to download directory ", dir, e)
    })
}


#m <- merged("../strava-data/input/manchester/Manchester_Marathon.gpx", "../strava-data/input/manchester/Manchester_Marathon.fit", "../strava-data/input/manchester")
#merged_from_dir("input/manchester")
#f <- merged_from_dir("input/funrun")
#f <- merged_from_dir("input/RunnerUp")

#list.files(path = "input/funrun", pattern = "\\.gpx$", full.names = TRUE)
#gpx("input/manchester/Manchester_Marathon.gpx")


#INPUT: list of directory names containing recording files of activities
#OUTPUT: one big dataframe with all of the data from all of the activities
join_activities <- function(dirs) {
  activities_list <- dirs %>%
    lapply(merged_from_dir)
  activities_list[activities_list %>% lapply(typeof) == "list"] %>%
    reduce(rbind)
}

#INPUT: filename of stored "all" dataframe, directory of inputs, whether to
#       load the saved file at all or not
#OUTPUT: Compute the dataframes for all activities not already in the saved file
#        and add them all to the big dataframe. Save to file.
update_all <- function(all_path = "all_activities.Rda", dir = "input", load_existing = TRUE) {
  all_dirs <- list.dirs(path=dir)
  if (load_existing) {
    load(all_path)
    all_dirs <- list.filter(all_dirs, !(. %in% all$session))
  }
  all <- all_dirs %>%
    join_activities() %>%
    rbind(all)
  save(all,file=all_path)
}

# Some default mapping function, might not work anymore
map <- function(df) {
  bb <- matrix(c(min(df$position_long)-0.001, max(df$position_long) + 0.001,
                 min(df$position_lat) - 0.001, max(df$position_lat) + 0.001),ncol=2,byrow=TRUE)
  map <- get_map(bb)
  ggmap(map) + 
    geom_point(data = df, aes(x = position_long, y = position_lat, color = session), size = 1)
}

# Helper function for calculating convex hull: generates all possible pairs of
# lat/lng points in a dataframe
rename_and_cross <- function(df) {
  df %>%
    rename(distance_2 = distance,
           timestamp_2 = timestamp,
           session_2 = session) %>%
    crossing(df)
}

# Calculates the convex hull of all possible segments in the dataframe, meaning
# the fastest segments of all possible lengths. Dataframe has already been cross-producted
convex_hull <- function(df) {
  df %>%
    arrange(-length) %>% 
    mutate(is_on_hull = timespan < lag(cummin(timespan))) %>%
    filter(is_on_hull) %>%
    filter(length > 0)
}

# Puts together the rename-and-cross and convex-hull functions to actually get
# the best segments from a dataframe like what merge() returns
single_activity_intervals <- function(df) {
  df %>%
    select(distance, timestamp, session) %>%
    rename_and_cross() %>% 
    mutate(timespan = as.numeric(difftime(timestamp, timestamp_2)),
           length = distance - distance_2) %>%
    select(timespan, length, session) %>%
    mutate(pace = 1000 / 60 * timespan / length) %>%
    convex_hull()
}

# Take all activities in a big dataframe and calculate their individual best
# segments.
intervals <- function(df, sp) {
  times <- df %>%
    filter(sport == sp) %>%
    select(distance, timestamp, session) %>%
    split(.$session)
  times %>%
    lapply(single_activity_intervals) %>%
    reduce(rbind)
}



update_all()
load("all_activities.Rda")

downsampled <- all %>%
  slice(which(row_number() %% 5 == 0))

running_intervals <- intervals(downsampled, "running")
save(running_intervals, file="running_intervals.Rda")
#load("running_intervals.Rda")

cycling_intervals <- intervals(downsampled, "cycling")

peak_runs <- convex_hull(running_intervals)
peak_rides <- convex_hull(cycling_intervals)


#world_records <- data.frame(
#  length = c(100, 200, 400, 800, 1000, 1500, 1609.3, 2000, 3000, 5000, 10000, 21100),
#  timespan = c(9.58, 19.19, 43.03, 100.91, 131.96, 206, 223.13, 284.79, 440.67, 755.36, 1571, 3452)
#) %>%
#  mutate(pace = 1000 / 60 * timespan / length)

best_time <- function(dist) {
  ((peak_runs %>% arrange(abs(length - dist)))$timespan)[1] %>%
    seconds_to_period()
}

best_time(5000)

running_intervals %>%
  ggplot() +
  geom_point(aes(x = length,
                 y = pace,
                 color = session), size=0.1) + 
  theme(legend.position="none") + 
  geom_line(aes(x = length,
                y = pace),
            data=peak_runs) +
  ylim(6, 3) +
  xlab("Distance (m)") +
  ylab("Pace (mins/km)")


cycling_intervals %>%
  ggplot() +
  geom_point(aes(x = length,
                 y = pace,
                 color = session), size=0.1) + 
  theme(legend.position="none") + 
  geom_line(aes(x = length,
                y = pace),
            data=peak_rides) +
  ylim(0, 6) +
  xlab("Distance (m)") +
  ylab("Pace (mins/km)") +
  geom_point(aes(x = length,
                 y = pace,
                 color = "#FF0000"),
             data = latest_run)



