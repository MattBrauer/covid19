
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
