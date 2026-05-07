# ── Libraries ──────────────────────────────────────────────────────────────────
library(motus)      # Motus Wildlife Tracking System API client
library(dplyr)      # Data manipulation
library(jsonlite)   # JSON reading and writing
library(httr)       # HTTP requests (used for reverse geocoding)

# Force UTC timezone for all timestamp handling
Sys.setenv(TZ = 'UTC')

# ── Authentication ─────────────────────────────────────────────────────────────
# Load Motus credentials from environment variables (set in .Renviron or CI secrets)
motus:::sessionVariable(name = 'userLogin',    val = Sys.getenv('MOTUS_USER'))
motus:::sessionVariable(name = 'userPassword', val = Sys.getenv('MOTUS_PASS'))

# ── Database Setup ─────────────────────────────────────────────────────────────
# Create local data directory if it doesn't already exist
dir.create('data', showWarnings = FALSE)

# Connect to the receiver database and pull the latest detections from Motus servers.
# new = FALSE: use existing local .motus SQLite file rather than creating a new one
# skipNodes = TRUE: skip node-level data to speed up the sync
recv <- tagme(
  'CTT-V3023D415E26',
  new      = FALSE,
  update   = TRUE,
  dir      = 'data/',
  skipNodes = TRUE
)

# Download species, tag, and project metadata into the local database
metadata(recv)

# ── Load Supporting Tables ─────────────────────────────────────────────────────
# Pull tag deployment info so we can join sex, age, and band number onto detections
tag_deps <- tbl(recv, 'tagDeps') %>%
  collect() %>%
  select(deployID, sex, age, bandNumber)

# Pull project names with a lowercase version for case-insensitive joining
proj_ids <- tbl(recv, 'projs') %>%
  collect() %>%
  select(id, name) %>%
  mutate(name_lower = tolower(name))

# ── Build Detections Table ─────────────────────────────────────────────────────
new_detections <- tbl(recv, 'alltags') %>%
  # Keep only detections that pass the Motus false-positive filter
  filter(motusFilter == 1) %>%
  collect() %>%
  # Keep only the earliest hit per run (one row per detection event)
  group_by(runID) %>%
  slice_min(ts, n = 1) %>%
  ungroup() %>%
  # Join sex, age, and band number from tag deployment records
  left_join(tag_deps, by = c('tagDeployID' = 'deployID')) %>%
  # Lowercase project name for case-insensitive join against projects table
  mutate(tagProjName_lower = tolower(tagProjName)) %>%
  left_join(proj_ids, by = c('tagProjName_lower' = 'name_lower')) %>%
  # Rename and select only the fields needed for the output JSON
  select(
    date        = ts,
    commonName  = speciesEN,
    scientificName = speciesSci,
    tagId       = mfgID,
    tagDeployID = tagDeployID,
    project     = tagProjName,
    projectId   = id,
    sex         = sex,
    age         = age,
    bandNumber  = bandNumber,
    antBearing  = antBearing,
    tagDepLat   = tagDepLat,
    tagDepLon   = tagDepLon
  ) %>%
  mutate(
    # Convert Unix timestamp to ISO 8601 UTC string
    date = as.POSIXct(date, origin = '1970-01-01', tz = 'UTC'),
    date = format(date, '%Y-%m-%dT%H:%M:%SZ'),
    # Unique ID per detection: tag + timestamp
    id   = paste0(tagId, '_', date),
    # Placeholder — will be filled in by reverse geocoding below
    locationName = NA
  )

# ── Merge with Existing Data ───────────────────────────────────────────────────
existing_path <- 'data/detections.json'

# Load previously saved detections if the file exists
if (file.exists(existing_path)) {
  existing_json       <- fromJSON(existing_path, flatten = TRUE)
  existing_detections <- as.data.frame(existing_json$detections)
} else {
  existing_detections <- data.frame()
}

# Combine new and existing detections, dropping any duplicates by ID
if (nrow(existing_detections) > 0) {
  combined <- bind_rows(new_detections, existing_detections) %>%
    distinct(id, .keep_all = TRUE) %>%
    arrange(desc(date))
} else {
  combined <- new_detections %>%
    arrange(desc(date))
}

# ── Reverse Geocoding ──────────────────────────────────────────────────────────
# In-memory cache keyed by rounded lat/lon to avoid redundant API calls
geocode_cache <- list()

# Given a lat/lon, return a human-readable "City, State" location string
# using the OpenStreetMap Nominatim API. Returns NA on failure.
reverse_geocode <- function(lat, lon) {
  key <- paste0(round(lat, 4), ',', round(lon, 4))

  # Return cached result if we've already looked up this coordinate
  if (!is.null(geocode_cache[[key]])) return(geocode_cache[[key]])

  # Respect Nominatim's usage policy: max 1 request per second
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

    # Build location string from the most specific available place name
    location <- paste(
      Filter(Negate(is.null), list(
        # Try city → town → village → county, in order of preference
        if (!is.null(addr$city))    addr$city    else
        if (!is.null(addr$town))    addr$town    else
        if (!is.null(addr$village)) addr$village else
        if (!is.null(addr$county))  addr$county  else NULL,
        if (!is.null(addr$state))   addr$state   else NULL
      )),
      collapse = ', '
    )

    location <- if (nchar(location) == 0) NA else location

    # Store in cache and return
    geocode_cache[[key]] <<- location
    location
  }, error = function(e) NA)
}

# Fill in locationName for any rows that are missing it and have valid coordinates
combined <- combined %>%
  mutate(locationName = ifelse(
    is.na(locationName) & !is.na(tagDepLat) & !is.na(tagDepLon),
    mapply(reverse_geocode, tagDepLat, tagDepLon),
    locationName
  ))

# ── Write Output ───────────────────────────────────────────────────────────────
# Wrap detections in an object with a top-level timestamp and write to JSON
output <- list(
  updated    = format(Sys.time(), '%Y-%m-%dT%H:%M:%SZ', tz = 'UTC'),
  detections = combined
)

write_json(output, existing_path, pretty = TRUE, auto_unbox = TRUE, na = 'null')
