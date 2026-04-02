library(motus)
library(dplyr)
library(jsonlite)

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

output <- list(
  updated = format(Sys.time(), '%Y-%m-%dT%H:%M:%SZ', tz = 'UTC'),
  detections = combined
)

write_json(output, existing_path, pretty = TRUE, auto_unbox = TRUE, na = 'null')
