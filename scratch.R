us_states %>%
  filter(state %in% c("Michigan","South Dakota","North Dakota","Louisiana")) %>%
  state_log_plot(quo(cases))


us_states <- by_state %>%
  ungroup() %>%
  separate(state, into=c("region", "st"), sep=",", fill="left", remove=FALSE) %>%
  mutate(st=state_names[str_trim(st)], region=str_trim(region)) %>%
  mutate(state=if_else(is.na(st), state, st)) %>%
  left_join(us_county_pop %>% filter(COUNTY == "000"), by = c("state" = "STNAME")) %>%
  filter(!is.na(pop)) %>%
  dplyr::select(state, pop, date, cases, recovered, deaths)

us_states %>%
  mutate_at(c("cases", "deaths", "recovered"), ~ (. * 1e6) / pop) %>%
  filter(state %in% c("Michigan","South Dakota","North Dakota","Louisiana")) %>%
  state_log_plot(quo(cases))
