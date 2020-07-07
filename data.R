
csse_data_dir <- "COVID-19/csse_covid_19_data/csse_covid_19_daily_reports"
nytimes_data_dir <- "covid-19-data"

qq <- function(...) sapply(substitute({ ... })[-1], deparse)
eu <- qq(Austria, Belgium, Bulgaria, Croatia, "Republic of Cyprus", "Czech Republic", Denmark, Estonia, Finland, France, Germany, Greece, Hungary, Ireland, Italy, Latvia, Lithuania, Luxembourg, Malta, Netherlands, Poland, Portugal, Romania, Slovakia, Slovenia, Spain, Sweden )
eu <- gsub("\"", "", eu)

###############################
## Google global mobility
###############################
# mobility <- read_csv("Global_Mobility_Report.csv")

###############################
## IHME projected deaths as of 5/4/2020
###############################
ihme_05_04 <- read_csv("IHME/2020_05_04/Hospitalization_all_locs.csv") %>%
  filter(location_name=="United States of America") %>%
  select(date, totdea_mean) %>%
  dplyr::rename(ihme = totdea_mean)


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
  mutate(county = gsub(" County| Parish", "", CTYNAME))

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

## From NY Times web page, scraped 2020-05-25
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

state_daily_stats <- csse_us_states %>%
  daily_from_cumulative()

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
  dplyr::select(state, county, date, pop, cases, recovered, deaths)

csse_us_counties <- complete %>%
  filter(country=="US", !is.na(county)) %>%
  group_by(state, county) %>%
  left_join(us_county_pop %>% filter(COUNTY != "000"),
            by = c("state" = "STNAME", "county" = "county")) %>%
  filter(!is.na(pop)) %>%
  dplyr::select(state, county, date, pop, cases, recovered, deaths)

latest_date <- complete %>%
  filter(cases > 0 | recovered == 0 | deaths == 0) %>%
  select(date) %>%
  top_n(1, date) %>%
  distinct() %>%
  dplyr::pull(date)

latest_date <- as.Date(latest_date)

## security checkpoint data from TSA: https://www.tsa.gov/coronavirus/passenger-throughput
## TODO: scrape website for updated data
flight_data <- read_tsv("Date	Total Traveler Throughput	Total Traveler Throughput
(1 Year Ago - Same Weekday)
6/25/2020	623,624	2,711,222
6/24/2020	494,826	2,594,661
6/23/2020	471,421	2,506,510
6/22/2020	607,540	2,716,428
6/21/2020	590,456	2,719,643
6/20/2020	507,129	2,378,559
6/19/2020	587,908	2,772,903
6/18/2020	576,514	2,728,786
6/17/2020	441,829	2,552,395
6/16/2020	417,924	2,466,574
6/15/2020	534,528	2,699,580
6/14/2020	544,046	2,642,083
6/13/2020	437,119	2,318,946
6/12/2020	519,304	2,727,860
6/11/2020	502,209	2,675,686
6/10/2020	386,969	2,509,058
6/9/2020	338,382	2,433,189
6/8/2020	430,414	2,644,981
6/7/2020	441,255	2,669,860
6/6/2020	353,016	2,225,952
6/5/2020	419,675	2,649,808
6/4/2020	391,882	2,623,947
6/3/2020	304,436	2,370,152
6/2/2020	267,742	2,247,421
6/1/2020	353,261	2,499,002
5/31/2020	352,947	2,555,578
5/30/2020	268,867	2,117,180
5/29/2020	327,133	2,570,613
5/28/2020	321,776	2,485,770
5/27/2020	261,170	2,269,035
5/26/2020	264,843	2,453,649
5/25/2020	340,769	2,512,237
5/24/2020	267,451	2,070,716
5/23/2020	253,190	2,124,825
5/22/2020	348,673	2,792,670
5/21/2020	318,449	2,673,635
5/20/2020	230,367	2,472,123
5/19/2020	190,477	2,312,727
5/18/2020	244,176	2,615,691
5/17/2020	253,807	2,620,276
5/16/2020	193,340	2,091,116
5/15/2020	250,467	2,664,549
5/14/2020	234,928	2,611,324
5/13/2020	176,667	2,343,675
5/12/2020	163,205	2,191,387
5/11/2020	215,645	2,512,315
5/10/2020	200,815	2,419,114
5/9/2020	169,580	1,985,942
5/8/2020	215,444	2,602,631
5/7/2020	190,863	2,555,342
5/6/2020	140,409	2,270,662
5/5/2020	130,601	2,106,597
5/4/2020	163,692	2,470,969
5/3/2020	170,254	2,512,598
5/2/2020	134,261	1,968,278
5/1/2020	171,563	2,546,029
4/30/2020	154,695	2,499,461
4/29/2020	119,629	2,256,442
4/28/2020	110,913	2,102,068
4/27/2020	119,854	2,412,770
4/26/2020	128,875	2,506,809
4/25/2020	114,459	1,990,464
4/24/2020	123,464	2,521,897
4/23/2020	111,627	2,526,961
4/22/2020	98,968	2,254,209
4/21/2020	92,859	2,227,475
4/20/2020	99,344	2,594,171
4/19/2020	105,382	2,356,802
4/18/2020	97,236	1,988,205
4/17/2020	106,385	2,457,133
4/16/2020	95,085	2,616,158
4/15/2020	90,784	2,317,381
4/14/2020	87,534	2,208,688
4/13/2020	102,184	2,484,580
4/12/2020	90,510	2,446,801
4/11/2020	93,645	2,059,142
4/10/2020	108,977	2,590,499
4/9/2020	104,090	2,487,398
4/8/2020	94,931	2,229,276
4/7/2020	97,130	2,091,056
4/6/2020	108,310	2,384,091
4/5/2020	122,029	2,462,929
4/4/2020	118,302	2,011,715
4/3/2020	129,763	2,476,884
4/2/2020	124,021	2,411,500
4/1/2020	136,023	2,151,626
3/31/2020	146,348	2,026,256
3/30/2020	154,080	2,360,053
3/29/2020	180,002	2,510,294
3/28/2020	184,027	2,172,920
3/27/2020	199,644	2,538,384
3/26/2020	203,858	2,487,162
3/25/2020	239,234	2,273,811
3/24/2020	279,018	2,151,913
3/23/2020	331,431	2,434,370
3/22/2020	454,516	2,542,643
3/21/2020	548,132	2,227,181
3/20/2020	593,167	2,559,307
3/19/2020	620,883	2,513,231
3/18/2020	779,631	2,320,885
3/17/2020	953,699	2,177,929
3/16/2020	1,257,823	2,465,709
3/15/2020	1,519,192	2,545,742
3/14/2020	1,485,553	2,274,658
3/13/2020	1,714,372	2,634,215
3/12/2020	1,788,456	2,503,924
3/11/2020	1,702,686	2,187,298
3/10/2020	1,617,220	2,122,898
3/9/2020	1,909,363	2,378,673
3/8/2020	2,119,867	2,485,430
3/7/2020	1,844,811	2,156,262
3/6/2020	2,198,517	2,543,689
3/5/2020	2,130,015	2,402,692
3/4/2020	1,877,401	2,143,619
3/3/2020	1,736,393	1,979,558
3/2/2020	2,089,641	2,257,920
3/1/2020	2,280,522	2,301,439") %>%
  filter(!is.na(`Total Traveler Throughput`)) %>%
  rename(`2020` = `Total Traveler Throughput`,
         `2019` = `Total Traveler Throughput_1`) %>%
  mutate(travel_date = as.Date(Date, "%m/%d/%Y")) %>%
  dplyr::select(travel_date, `2020`, `2019`) %>%
  pivot_longer(-travel_date, names_to = "year", values_to = "throughput")
  
  

