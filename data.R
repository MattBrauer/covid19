
csse_data_dir <- "COVID-19/csse_covid_19_data/csse_covid_19_daily_reports"
nytimes_data_dir <- "covid-19-data"


###############################
## CA hospital data
###############################
ca_hospital_data <- "california-covid-19-hospital-data-and-case-statistics-pppjux"
ca_data <- read_csv(file.path(ca_hospital_data, "covid-19-cases.csv")) %>%
  mutate(`Most Recent Date` = as.Date(`Most Recent Date`, "%m/%d/%Y")) %>%
  rename("county" = `County Name`,
         "date" = `Most Recent Date`,
         "cases" = `Total Count Confirmed`,
         "deaths" = `Total Count Deaths`,
         "positive" = `COVID-19 Positive Patients`,
         "suspected" = `Suspected COVID-19 Positive Patients`,
         "icu_positive" = `ICU COVID-19 Positive Patients`,
         "icu_suspected" = `ICU COVID-19 Suspected Patients`)
# ca_data %>%
#   pivot_longer(cols=c(-county, -date), names_to = "desc", values_to = "count") %>%
#   group_by(county, date) %>%
#   summarize(count = sum(count))
# 
# ca_data %>%
#   group_by(date) %>%
#   summarize_if(is.numeric, sum, na.rm=TRUE) %>%
#   pivot_longer(-date, names_to = "desc", values_to = "count") %>%
#   arrange(desc(date)) %>%
#   group_by(date) %>%
#   summarize(count = sum(count)) %>%
#   ggplot(aes(x=date, y=count)) +
#   geom_point()


###############################
## OWID demographic data
###############################
owid_data_dir <- "owid-datasets/datasets"
## overkill: gets all of the owid data
# owid_data_dirs <- dir_ls(owid_data_dir) #, glob="*.csv")
# owid_covid_tests_file <- dir_ls(file.path(owid_data_dir, "COVID-19 Tests"), glob="*.csv")
# owid_csv_data <- as_tibble(list(directory = dir_ls(owid_data_dir))) %>%
#   mutate(filename = dir_ls(directory, glob="*.csv"),
#          data = purrr::map(filename, read_csv))
# 
# owid_population_data <- owid_csv_data %>%
#   filter(grepl("Population by country", filename), grepl("(Gapminder & UN)", filename)) %>%
#   unnest() %>%
#   dplyr::select(-directory, -filename)
# 
# owid_population_data %>%
#   filter(Year==2019) %>%
#   dplyr::select(1,4) %>%
#   rename(pop = `Population by country, 1800 to 2015 (Gapminder & UN)`,
#          country = Entity) %>%
#   write_csv("demographic_data/owid_global_population_2019")

global <- read_csv("demographic_data/owid_global_population_2019")

us_metro_pop <- read_csv("demographic_data/cbsa-est2019-alldata.csv") %>%
  select(CBSA, MDIV, STCOU, NAME, LSAD, POPESTIMATE2019) %>%
  rename(pop = POPESTIMATE2019)

us_county_pop <- read_csv("demographic_data/co-est2019-alldata.csv") %>%
  dplyr::select(1:7, 19) %>%
  rename(pop = POPESTIMATE2019) %>%
  mutate(county = gsub(" County", "", CTYNAME))

us_state_pop <- read_csv("demographic_data/nst-est2019-alldata.csv") %>%
  dplyr::select(1:5, 17) %>%
  rename(pop = POPESTIMATE2019)

## to get state-level data
# us_county_pop %>%
#   filter(COUNTY == "000", STNAME == "Alabama")

## to get county-level data
# us_county_pop %>%
#   filter(COUNTY != "000", STNAME == "Alabama")


###############################
## COVID tracking data
###############################
covid_tracking <- "https://covidtracking.com/api/"
covid_dataset_names <- c("states", "states/daily", "states/info",
                         "us", "us/daily",
                         "counties", "urls", "press")

covid_datasets <- lapply(as.list(paste0(covid_tracking, covid_dataset_names, ".csv")),
                         function(api_call) {
                           response <- GET(api_call)
                           content(response)
                         })

names(covid_datasets) <- covid_dataset_names

covid_datasets$states_qc <- covid_datasets$states %>%
  select(state, lastUpdateEt, checkTimeEt, dateModified, dateChecked, grade)

covid_datasets$states <- covid_datasets$states %>%
  select(-lastUpdateEt, -checkTimeEt, -dateModified, -dateChecked, -grade)

covid_datasets$`states/daily` <- covid_datasets$`states/daily` %>%
  mutate(date = as.Date(as.character(date), "%Y%m%d")) %>%
  select(-dateChecked)

covid_datasets$`us/daily` <- covid_datasets$`us/daily` %>%
  mutate(date = as.Date(as.character(date), "%Y%m%d"))

###############################
## map data
###############################
states_map <- map_data("state")
counties_map <- map_data("county")

state_abbreviations <- c("AL", "AK", "AZ", "KS", "UT", "CO", "CT", 
                         "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "AR", 
                         "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", 
                         "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", 
                         "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", 
                         "CA", "VT", "VA", "WA", "WV", "WI", "WY", "DC")
state_names <- c("Alabama", "Alaska", "Arizona", "Kansas", 
                 "Utah", "Colorado", "Connecticut", "Delaware", "Florida", 
                 "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", 
                 "Iowa", "Arkansas", "Kentucky", "Louisiana", "Maine", 
                 "Maryland", "Massachusetts", "Michigan", "Minnesota", 
                 "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", 
                 "New Hampshire", "New Jersey", "New Mexico", "New York", 
                 "North Carolina", "North Dakota", "Ohio", 
                 "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", 
                 "South Carolina", "South Dakota", "Tennessee", "Texas", 
                 "California", "Vermont", "Virginia", "Washington", "West Virginia", 
                 "Wisconsin", "Wyoming", "District of Columbia")
names(state_names) <- state_abbreviations
names(state_abbreviations) <- state_names

###############################
## NYT data
###############################
nyt_counties <- read_csv(paste0(nytimes_data_dir, "/us-counties.csv"))
nyt_states <- read_csv(paste0(nytimes_data_dir, "/us-states.csv"))

## From NY Times web page, scraped 2020-05-03
shutdown_dates <- read_csv("statewide_events.csv") %>%
  pivot_longer(-state, names_to = "event", values_to = "event_date") %>%
  mutate(event_date = as.Date(paste0("2020-", event_date)),
         state = state_names[state]) %>%
  arrange(state, event_date)


###############################
## JHU CSSE data:
##   from git repo as submodule
###############################
# daily reports data. follows different data models at different times, so a fair bit
#  of tweeking is necessary

data_model_start_dates <- as.Date(c("01-22-2020", "03-22-2020"), "%m-%d-%Y")

data_models <- Vectorize(function(obs_date, start_dates) {
  end_dates <- lead(c(start_dates,today() + days(1))-days(1))[1:length(start_dates)]
  intervals <- lubridate::interval(start = start_dates, end = end_dates)
  which(obs_date %within% intervals)
}, "obs_date")

report_files <- dir_ls(csse_data_dir, glob="*.csv")
report_dates <- as.Date(path_ext_remove(path_file(report_files)), "%m-%d-%Y")
report_models <- data_models(report_dates, data_model_start_dates)
report_sets <- as_tibble(list(file = report_files,
                              date = path_ext_remove(path_file(report_files)),
                              model = report_models))

data_columns <- cols(
  `Province/State` = col_character(),
  `Country/Region` = col_character(),
  Province_State = col_character(),
  Country_Region = col_character(),
  `Last Update` = col_skip(),
  Last_Update = col_skip(),
  Confirmed = col_double(),
  Deaths = col_double(),
  Recovered = col_double(),
  Latitude = col_double(),
  Longitude = col_double(),
  Lat = col_double(),
  Long_ = col_double(),
  FIPS = col_character(),
  Admin2 = col_character()
)

report_data <- do.call(rbind.data.frame, report_sets %>%
                         split(.$model) %>%
                         purrr::map(~ .x %>%
                                      select(date, file) %>%
                                      deframe() %>%
                                      map_dfr(read_csv, .id="date",
                                              col_types=data_columns) %>%
                                      add_column(!!!c("FIPS","Admin2")[!c("FIPS","Admin2") %in% names(.)]) %>%
                                      rename_at(vars(matches("\\/")), function(x) gsub("\\/","_",x)) %>%
                                      rename_at(vars(matches("_$")), function(x) gsub("_$","",x)) %>%
                                      rename_at(vars(matches("itude$")), function(x) gsub("itude$","",x)) %>%
                                      rename_at(vars(matches("FIPS")), function(x) "FIPS") %>%
                                      rename_at(vars(matches("Admin2")), function(x) "Admin2") %>%
                                      mutate(date = as.Date(date, "%m-%d-%Y"),
                                             FIPS = ifelse(FIPS=="FIPS", NA, FIPS),
                                             Admin2 = ifelse(Admin2=="Admin2", NA, Admin2)) %>%
                                      select(date, FIPS, Admin2, Province_State, Country_Region,
                                             Confirmed, Deaths, Recovered)
                         ))

## recoding country name errors
countries <- report_data %>%
  rename("country" = Country_Region) %>%
  mutate("name" = country) %>%
  mutate(country = ifelse(grepl("Bahamas", country), "Bahamas", country),
         country = ifelse(grepl("China", country), "China", country),
         country = ifelse(grepl("Congo", country), "Congo", country),
         country = ifelse(grepl("Czech", country), "Czechia", country),
         country = ifelse(grepl("Gambia", country), "Gambia", country),
         country = ifelse(grepl("Guinea", country), "Guinea", country),
         country = ifelse(grepl("Hong Kong", country), "Hong Kong", country),
         country = ifelse(grepl("Iran", country), "Iran", country),
         country = ifelse(grepl("Ireland$", country), "Ireland", country),
         country = ifelse(grepl("Maca", country), "Macau", country),
         country = ifelse(grepl("Martin$", country), "Saint Martin", country),
         country = ifelse(grepl("Moldova$", country), "Moldova", country),
         country = ifelse(grepl("Palestin", country), "Palestine", country),
         country = ifelse(grepl("Russia", country), "Russia", country),
         country = ifelse(grepl("Timor", country), "Timor", country),
         country = ifelse(grepl("Viet", country), "Vietnam", country),
         country = ifelse(grepl("Taiwan", country) | grepl("Taipei", country),
                          "Taiwan", country),
         country = ifelse(country=="United Kingdom", "UK", country),
         country = ifelse(country=="Korea, South", "South Korea", country),
         country = ifelse(country=="Republic of Korea", "North Korea", country),
         country = ifelse(country=="Holy See" | grepl("Vatican", country),
                          "Vatican City", country),
         country = ifelse(country=="Jersey" | country=="Guernsey", "Channel Islands", country),
         country = ifelse(country=="Cote d'Ivoire", "Ivory Coast", country)) %>%
  select(name, country) %>%
  distinct() %>%
  filter(!is.na(country)) %>%
  arrange(country) %>%
  deframe()

## recoding state name errors
states <- report_data %>%
  rename("country" = Country_Region, "state" = Province_State) %>%
  mutate("name" = state) %>%
  mutate(state = ifelse(country=="US" & grepl("Virgin Islands", state),
                        "Virgin Islands", state),
         state = ifelse(country=="US" & grepl("D.C.", state),
                        "District of Columbia", state),
         state = ifelse(country=="US" & grepl("New York", state),
                        "New York", state)) %>%
  select(name, state) %>%
  distinct() %>%
  filter(!is.na(state)) %>%
  arrange(state) %>%
  deframe()

complete <- report_data %>%
  rename("country"=Country_Region,
         "state"=Province_State,
         "county"=Admin2,
         "cases"=Confirmed,
         "deaths"=Deaths,
         "recovered"=Recovered) %>%
  mutate(country = recode(country, !!!countries)) %>%
  mutate(state = recode(state, !!!states)) %>%
  filter(!grepl("Travis, CA", state))

# build data structures for specific uses

# US states
# limit to US and remove locations (mostly through early March) that are specific within states
#   (e.g., "Berkeley, CA")
csse_by_state <- complete %>%
  filter(country=="US") %>%
  ungroup() %>%
  separate(state, into=c("region", "st"), sep=",", fill="left", remove=FALSE) %>%
  mutate(st=state_names[str_trim(st)],
         region=str_trim(region),
         state=if_else(is.na(st), state, st)) %>%
  select(-st) %>%
  group_by(state, date) %>%
  summarize(cases=sum(cases), recovered=sum(recovered), deaths=sum(deaths))

## totals by country
csse_by_country <- complete %>%
  group_by(country, date) %>%
  summarize(cases=sum(cases), recovered=sum(recovered), deaths=sum(deaths))

## total by country, except US where total by state
## (for comparing US states to countries ex-US)
csse_by_country_state <- csse_by_state %>%
  rename(country = state) %>%
  bind_rows(csse_by_country) %>%
  distinct()

## add population
csse_us_states <- csse_by_state %>%
  left_join(us_county_pop %>% filter(COUNTY == "000"), by = c("state" = "STNAME")) %>%
  dplyr::select(state, pop, date, cases, recovered, deaths)

## current numbers
csse_us_states_current <- csse_us_states %>%
  group_by(state, date) %>%
  summarize_if(is.numeric, sum) %>%
  arrange(state, date) %>%
  slice(n())

csse_us_counties_current <- complete %>%
  filter(country=="US", !is.na(county)) %>%
  group_by(state, county) %>%
  top_n(1, date) %>%
  left_join(us_county_pop %>% filter(COUNTY != "000"),
            by = c("state" = "STNAME", "county" = "county")) %>%
  filter(!is.na(pop)) %>%
  dplyr::select(state, pop, date, cases, recovered, deaths)

latest_date <- complete %>%
  filter(cases > 0 | recovered == 0 | deaths == 0) %>%
  select(date) %>%
  top_n(1, date) %>%
  distinct() %>%
  dplyr::pull(date)

latest_date <- as.Date(latest_date)
