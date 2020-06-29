by_country_state %>%
  group_by(country) %>%
  filter(country %in% c("California", "New York", "Italy")) %>%
  mutate(days = date - min(date),
         fit_cases = slide_index2(cases, days, .i=date, .f=~lm(log(.x) + 1 ~ .y), .before = 10), #) %>% #,
         exp_cases = purrr::map(fit_cases, tidy)) %>%
  unnest(exp_cases) %>%
  filter(term==".y") %>%
  dplyr::select(-days, -fit_cases, -term, -std.error, -statistic, -p.value) %>%
  ggplot(aes(x = date, y = estimate, color = country)) +
  geom_line()
  
  