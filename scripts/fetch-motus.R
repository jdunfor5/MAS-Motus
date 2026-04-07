library(motus)
library(dplyr)
library(jsonlite)
library(httr)

Sys.setenv(TZ = 'UTC')

motus:::sessionVariable(name = 'userLogin', val = Sys.getenv('MOTUS_USER'))
motus:::sessionVariable(name = 'userPassword', val = Sys.getenv('MOTUS_PASS'))

dir.create('data', showWarnings = FALSE)

recv <- tagme(
  'CTT-V3023D415E26',
  new = FALSE,
  update = TRUE,
  dir = 'data/',
  skipNodes = TRUE
)

metadata(recv)

tag_deps <- tbl(recv, 'tagDeps') %>%
  collect() %>%
  select(deployID, sex, age)

proj_ids <- tbl(recv, 'projs') %>%
  collect() %>%
  select(id, name) %>%
  mutate(name_lower = tolower(name))

new_detections <- tbl(recv, 'alltags') %>%
  filter(motusFilter == 1) %>%
  collect() %>%
  group_by(runID) %>%
  slice_min(ts, n = 1) %>%
  ungroup() %>%
  left_join(tag_deps, by = c('tagDeployID' = 'deployID')) %>%
  mutate(tagProjName_lower = tolower(tagProjName)) %>%
  left_join(proj_ids, by = c('tagProjName_lower' = 'name_lower')) %>%
  select(
    date = ts,
    commonName = speciesEN,
    scientificName = speciesSci,
    tagId = mfgID,
    project = tagProjName,
    projectId = id,
    sex = sex,
    age = age,
    tagDepLat = tagDepLat,
    tagDepLon = tagDepLon
  ) %>%
  mutate(
    date = as.POSIXct(date, origin = '1970-01-01', tz = 'UTC'),
    date = format(date, '%Y-%m-%dT%H:%M:%SZ'),
    id = paste0(tagId, '_', date),
    locationName = NA
  )

existing_path <- 'data/latest_detections.json'

if (file.exists(existing_path)) {
  existing_json <- fromJSON(existing_path, flatten = TRUE)
  existing_detections <- as.data.frame(existing_json$detections)
} else {
  existing_detections <- data.frame()
}

if (nrow(existing_detections) > 0) {
  combined <- bind_rows(existing_detections, new_detections) %>%
    distinct(id, .keep_all = TRUE) %>%
    arrange(desc(date))
} else {
  combined <- new_detections %>%
    arrange(desc(date))
}

# Reverse geocode missing locationNames
geocode_cache <- list()

reverse_geocode <- function(lat, lon) {
  key <- paste0(round(lat, 4), ',', round(lon, 4))
  if (!is.null(geocode_cache[[key]])) return(geocode_cache[[key]])

  Sys.sleep(1)
  tryCatch({
    res <- httr::GET(
      url = paste0(
        'https://nominatim.openstreetmap.org/reverse?lat=', lat,
        '&lon=', lon,
        '&format=json'
      ),
      httr::add_headers('User-Agent' = 'MAS-Motus/1.0 (meckbirds.org)')
    )
    addr <- fromJSON(httr::content(res, as = 'text', encoding = 'UTF-8'))$address
    location <- paste(
      Filter(Negate(is.null), list(
        if (!is.null(addr$city)) addr$city else
        if (!is.null(addr$town)) addr$town else
        if (!is.null(addr$village)) addr$village else
        if (!is.null(addr$county)) addr$county else NULL,
        if (!is.null(addr$state)) addr$state else NULL
      )),
      collapse = ', '
    )
    location <- if (nchar(location) == 0) NA else location
    geocode_cache[[key]] <<- location
    location
  }, error = function(e) NA)
}

combined <- combined %>%
  mutate(locationName = ifelse(
    is.na(locationName) & !is.na(tagDepLat) & !is.na(tagDepLon),
    mapply(reverse_geocode, tagDepLat, tagDepLon),
    locationName
  ))

output <- list(
  updated = format(Sys.time(), '%Y-%m-%dT%H:%M:%SZ', tz = 'UTC'),
  detections = combined
)

write_json(output, existing_path, pretty = TRUE, auto_unbox = TRUE, na = 'null')
