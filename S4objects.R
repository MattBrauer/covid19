checkGeoData <- function(object) {
  errors <- character()

  # df <- suppressWarnings(as_tibble(object)) %>%
  #   dplyr::select(dplyr::all_of(object@geographical_unit_hierarchy)) %>%
  #   dplyr::distinct()
  # if(dim(object) > dim(df)) errors <- c(errors, "Inconsistent annotations.")
  
  if (length(errors) == 0) TRUE else errors  
}

setClass("GeoData",
         contains = class(tibble()),
         representation(geographical_scope = "character", # "global", "US", "California"
                        geographical_resolution = "character", # "county", "state", "country"
                        geographical_unit_hierarchy = "character",
                        geographical_unit_annotation = "character"),
         prototype(geographical_scope = NA_character_,
                   geographical_resolution = NA_character_,
                   geographical_unit_hierarchy = NA_character_,
                   geographical_unit_annotation = NA_character_),
         validity = checkGeoData)

GeoData <- function(df, geo_units = NULL, annotation = NULL, geo_scope = NULL, geo_resolution = NULL) {
  gd <- new("GeoData",
            df %>%
              ungroup() %>%
              dplyr::select(dplyr::all_of(geo_units), dplyr::all_of(annotation)) %>%
              dplyr::distinct(),
            geographical_scope = geo_scope,
            geographical_unit_hierarchy = geo_units,
            geographical_unit_annotation = annotation,
            geographical_resolution = geo_units[length(geo_units)])
  
  if(validObject(gd)) gd
}

setMethod("show", "GeoData", function(object) {
  df <- object
  class(df) <- "tbl_df"
  suppressWarnings(show(df))
})

checkGeoTime <- function(object) {
  errors <- character()
  class_of_date_var <- class(object[[object@date_column_name]])
  if(class_of_date_var != "Date") {
    msg <- paste0("Date variable is a ", class_of_date_var, ".  Should be a 'Date'")
    errors <- c(errors, msg)
  }

  if (length(errors) == 0) TRUE else errors  
}

setClass("GeoTime",
         contains = "tbl_df",
         representation(geoData = "GeoData",
                        date_column_name = "character",
                        variable_names = "character", # "c('cases','deaths','recovered')"
                        last_date = "Date",
                        new_calc = "logical",
                        avg_calc = "logical",
                        avg_window = "numeric",
                        exp_calc = "logical",
                        exp_window = "numeric",
                        data_source = "character"),
         prototype(date_column_name = "date",
                   variable_namess = NA_character_,
                   last_date = NA,
                   new_calc = FALSE,
                   avg_calc = FALSE,
                   avg_window = 0,
                   exp_calc = FALSE,
                   exp_window = 0,
                   data_source = NA_character_),
         validity = checkGeoTime)

GeoTime <- function(df, date_column_name = "date") {
  dd <- new("GeoTime",
            df,
            date_column_name = date_column_name,
            last_date = max(df[[date_column_name]]))
  if(validObject(dd)) dd
}

exp_fit <- function(dataset, political_unit, variable, window) {
  dataset %>%
    replace_na(list(cases = 0, deaths = 0, recovered = 0)) %>%
    group_by(!!political_unit) %>%
    mutate(days = date - min(date),
           fits = slide_index2(!!variable, days, .i=date,
                               .f=~lm(log(.x + 1) ~ .y),
                               .before = window),
           params = purrr::map(fits, tidy)) %>%
    unnest(params) %>%
    filter(term==".y") %>%
    dplyr::select(!!political_unit, date, estimate) %>%
    nest(date, estimate)
}

county_exp_fits <- csse_us_counties %>%
  unite("state_county", c(state, county), remove=FALSE) %>%
  filter(cases > 100) %>%
  exp_fit(quo(state_county), quo(cases), window = 7) %>%
  separate(state_county, into=c("state", "county"), sep="_") %>%
  group_by(state, county) %>%
  mutate(max_est = purrr::map_dbl(data, ~max(.$estimate)),
         max_date = as.Date(purrr::map_dbl(data, ~ .$date[which.max(.$estimate)]),
                            origin = "1970-01-01"),
         last_est = purrr::map_dbl(data, ~ .$estimate[which.max(.$date)]),
         last_date = as.Date(purrr::map_dbl(data, ~ max(.$date)),
                             origin = "1970-01-01"))

ca_exp_fits %>%
  filter(county %in% c("Los Angeles", "San Francisco", "Santa Clara", "Alameda")) %>%
  group_by(county) %>%
  unnest(data) %>%
  filter(date > "2020-05-15") %>%
  ggplot(aes(x=date, y=estimate, color=county)) +
  geom_line()

exp_fits <- csse_us_states %>%
  filter(cases > 9) %>%
  exp_fit(quo(state), quo(cases), window = 5) %>%
  group_by(state) %>%
  mutate(max_est = purrr::map_dbl(data, ~max(.$estimate)),
         max_date = as.Date(purrr::map_dbl(data, ~ .$date[which.max(.$estimate)]),
                            origin = "1970-01-01"),
         last_est = purrr::map_dbl(data, ~ .$estimate[which.max(.$date)]),
         last_date = as.Date(purrr::map_dbl(data, ~ max(.$date)),
                             origin = "1970-01-01"))
