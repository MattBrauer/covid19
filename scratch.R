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

csse_us_states %>%
  daily_from_cumulative() %>%
  filter(state %in% c("Arizona", "Florida", "Texas", "California")) %>%
  group_by(state) %>%
  rolling_average() %>%
  { ggplot(., aes(x=date, y=avg_new_cases)) +
      facet_wrap(state ~ ., scales = "free_y", ncol = 2) +
      geom_line(size=0.3) +
      geom_point(aes(x=date, y=new_cases), color = "red", size=0.3) +
      scale_color_manual(values = c(NA, "red"), guide=F) +
      xlim(min(state_daily_stats$date), max(state_daily_stats$date)) +
      ylim(0, NA) +
      theme_minimal() +  
      theme(axis.title=element_blank()) +
      #     theme(axis.title=element_blank(),
      #           axis.text.y = element_blank(), axis.ticks = element_blank()) +
      theme(strip.text.y = element_text(angle = 0, vjust=0.2, hjust=0)) +
      ggtitle("Daily new cases, with 10 day rolling average")
  }
tn <- read_csv("statewide/Tennessee/Public-Dataset-Daily-Case-Info.csv")
tn %>%
  ggplot(aes(x=DATE, y=TOTAL_ACTIVE)) +
  #  geom_line(size=0.3) +
  #  geom_point(aes(x=date, y=new_cases), color = "red", size=0.3) +
  geom_point(color = "red", size=0.3) +
  scale_color_manual(values = c(NA, "red"), guide=F) +
  #  xlim(min(state_daily_stats$date), max(state_daily_stats$date)) +
  ylim(0, NA) +
  theme_minimal() +  
  theme(axis.title=element_blank()) +
  #     theme(axis.title=element_blank(),
  #           axis.text.y = element_blank(), axis.ticks = element_blank()) +
  theme(strip.text.y = element_text(angle = 0, vjust=0.2, hjust=0)) +
  ggtitle("Daily new cases, with 7 day rolling average")



csse_us_states %>%
  mutate(active = cases - recovered - deaths) %>%
  daily_from_cumulative() %>%
  filter(state == "Tennessee") %>%
  rolling_average() %>%
  { ggplot(., aes(x=date, y=avg_new_cases)) +
      facet_wrap(state ~ ., scales = "free_y", ncol = 2) +
      geom_line(size=0.3) +
      geom_point(aes(x=date, y=new_cases), color = "red", size=0.3) +
      scale_color_manual(values = c(NA, "red"), guide=F) +
      xlim(min(state_daily_stats$date), max(state_daily_stats$date)) +
      ylim(0, NA) +
      theme_minimal() +  
      theme(axis.title=element_blank()) +
      #     theme(axis.title=element_blank(),
      #           axis.text.y = element_blank(), axis.ticks = element_blank()) +
      theme(strip.text.y = element_text(angle = 0, vjust=0.2, hjust=0)) +
      ggtitle("Daily new cases, with 7 day rolling average")
  }

script <- c("\"Florida was one of the last states to close down\nand one of the first states to open up.",
  "And while the rest of the country is still freaking out,\nespecially in blue states, look at this.",
  "I'm at a bar-restaurant. We're all having a good time.",
  "Not a single face mask. It's not that bad guys!\"")
label <- "date of this moron's speech"

state_daily_stats %>%
  filter(state == "Florida") %>%
  rolling_average(window=3) %>%
  { ggplot(., aes(x=date, y=avg_new_cases)) +
      geom_line(size=0.3) +
      geom_point(aes(x=date, y=new_cases), color = "red", size=0.3) +
      scale_color_manual(values = c(NA, "red"), guide=F) +
      xlim(min(state_daily_stats$date), max(state_daily_stats$date)) +
      ylim(0, NA) +
      theme_minimal() + 
#      geom_vline(aes(xintercept = as.Date("2020-06-05")), lty=21) +
      annotate("text", x = as.Date("2020-05-01"), y = 9000, label=script[1],
               colour="dark gray", hjust = "right") +
      annotate("text", x = as.Date("2020-06-01"), y = 7850, label=script[2],
               colour="dark gray", hjust = "right") +
      annotate("text", x = as.Date("2020-02-01"), y = 6700, label=script[3],
               colour="dark gray", hjust = "left") +
      annotate("text", x = as.Date("2020-06-01"), y = 6000, label=script[4],
               colour="red", hjust = "right") +
      annotate("segment", x = as.Date("2020-05-14"), xend = as.Date("2020-05-14"),
                   y = 5500, yend = 1500, colour = "red",
                   size=0.5, alpha=0.6, arrow=arrow()) +
      theme(axis.title=element_blank()) +
      theme(strip.text.y = element_text(angle = 0, vjust=0.2, hjust=0)) +
      ggtitle("Daily new cases in Florida, with 7 day rolling average")
  }
