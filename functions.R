library(tidyverse)
library(lubridate)
library(fs)
library(jpeg)
library(broom)
library(slider)
library(kableExtra)
library(gridExtra)
library(maps)
library(viridis)
library(ggthemes)
library(gganimate)
library(gifski)
library(usmap)
library(git2r)
library(httr)
library(tsibble)


# timeplot <- function(dataset, variable, log_scale = FALSE, show_points = FALSE, lag_percentile = 10, topn = 3) {
#   var_classes <- lapply(dataset, class)
#   if(!quo_name(variable) %in% names(var_classes)[var_classes == "numeric"])
#     warning(paste0("plotting variable \'", quo_name(variable), "\' not provided."))
#   else {
#     grouping_var <- names(var_classes)[var_classes == "character"][1]
#     date_var <- names(var_classes)[var_classes == "Date"][1]
#     if(is.na(topn)) topn <- dim(dataset %>% group_by_if(is.character) %>% summarize(n=n()))[1]
#     plotdata <- dataset %>%
#       filter_if(is.character, all_vars(!is.na(.))) %>%
#       group_by_if(is.character) %>%
#       mutate(day = date - min(date))
#     if(lag_percentile > 0) {
#       lag_day <- dataset %>%
#         lag_days() %>%
#         select(!!ensym(grouping_var), !!variable) %>%
#         deframe()
#       plotdata <- plotdata %>%
#         mutate("day" = !!ensym(date_var) - min(!!ensym(date_var)),
#                "lag_date" = !!ensym(date_var) - lag_day[!!ensym(grouping_var)]) %>%
#         ungroup() %>%
#         mutate("lag_date" = lag_date - min(lag_date))
#       date_var <- "lag_date"
#     } else {
#       date_var <- "day"
#     }
    

rolling_average <- function(dataset, window = 5) {
  var_classes <- lapply(dataset, class)
  grouping_var <- names(var_classes)[var_classes == "character"][1]
  date_var <- names(var_classes)[var_classes == "Date"][1]
  
  dataset %>%
    as_tsibble(key=!!ensym(grouping_var), index=!!ensym(date_var)) %>%
    group_by(!!ensym(grouping_var)) %>%
    mutate_if(is.numeric,
              .funs = list(avg = ~slider::slide_dbl(.x = (.), .f = mean,
                                                    .before = window, .after = window))) %>%
    rename_at(vars( contains( "_avg") ),
              list(~paste("avg", gsub("_avg", "", .), sep = "_"))) %>%
    as_tibble()
}

annotate_dates <- function(dataset, annotations) {
  var_classes <- lapply(dataset, class)
  date_var <- names(var_classes)[var_classes == "Date"][1]
  grouping_var <- names(var_classes)[var_classes == "character"][1]

  dataset %>%
    left_join(annotations %>%
                rename_if(is.Date, ~str_replace(., ".*", date_var)))
}


## rolling average
rolling_average_ <- function(dataset,
                            variable = quo(new_cases),
                            event_markers = NA,
                            window = 5) {
  var_classes <- lapply(dataset, class)
  if(!quo_name(variable) %in% names(var_classes)[var_classes == "numeric"])
    warning(paste0("plotting variable \'", quo_name(variable), "\' not provided."))
  else {
    grouping_var <- names(var_classes)[var_classes == "character"][1]
    date_var <- names(var_classes)[var_classes == "Date"][1]
    start_date <- min(c(as.Date("2020-02-01"), dataset[[date_var]]))
    
    if(!is.na(event_markers)) {
      event_var_classes <- lapply(event_markers, class)
      event_date_var <- names(event_var_classes)[event_var_classes == "Date"][1]
      event_grouping_var <- names(event_var_classes)[event_var_classes == "character"][1]
      # event_markers <- event_markers %>%
      #   rename(quo_name(ensym(grouping_var)) = event_grouping_var, date_var = event_date_var)
      dataset <- dataset %>%
        left_join(event_markers, by=c(grouping_var, "date" = "event_date"))
    }
    dataset %>%
      as_tsibble(key=!!grouping_var, index=!!date_var) %>%
      group_by(!!grouping_var) %>%
      mutate(avg = slider::slide_dbl(.x = !!variable,
                                     .f = mean,
                                     .before = window,
                                     .after = window)) %>%
      { 
        p <- ggplot(., aes(x = date, y = !!variable)) +
          geom_line(aes(x = date, y=avg)) +
          geom_point(aes(x = date, y=!!variable), shape = 20, fill = "red", color = "red") +
          ylim(0,NA) +
          xlim(start_date, NA) +
          facet_wrap(vars(state),
                     scale = "free",
                     ncol=2)
        if(!is.na(event_markers))
          p <- p +
            geom_vline(data = . %>% filter(event=="sip_start"),
                   aes(xintercept = date), color = "red", linetype = 2) +
            geom_vline(data = . %>% filter(event=="max_exp"),
                     aes(xintercept = date), color = "red", linetype = 1) +
            geom_vline(data = . %>% filter(event=="sip_end"),
                     aes(xintercept = date), color = "green", linetype = 2)
        p
      }
  }
}
  
  
## function takes a tsibble with cumulative counts and breaks down to daily counts
daily_from_cumulative <- function(dataset) {
  dataset %>%
    select_at(vars(!contains("pop"))) %>%
    rename_if(is.character, ~paste0("keys_", .)) %>%
    as_tsibble(index=date, key=starts_with("keys_")) %>%
    rename_if(is.character, ~gsub("keys_", "", .)) %>%
    mutate_if(is.numeric, .funs = list(new = ~difference(.))) %>%
    rename_at(vars( contains( "_new") ),
              list(~paste("new", gsub("_new", "", .), sep = "_"))) %>%
    as_tibble()
}

## find date at which each jurisdiction reaches 10% of maximum
lag_days <- function(dataset, percentile = 10) {
  var_classes <- lapply(dataset, class)
  date_var <- names(var_classes)[var_classes == "Date"][1]
  grouping_var <- names(var_classes)[var_classes == "character"][1]
  first_deciles <- dataset %>%
    ungroup() %>%
    summarize_if(is.numeric, ~max(., na.rm=TRUE)/percentile)
  dataset %>%
    group_by(!!ensym(grouping_var)) %>%
    # summarize(cases = max(date[cases < (first_deciles %>% dplyr::pull(cases))], na.rm=TRUE),
    #           deaths = max(date[deaths < (first_deciles %>% dplyr::pull(deaths))], na.rm=TRUE),
    #           recovered = max(date[recovered < (first_deciles %>% dplyr::pull(recovered))], na.rm=TRUE))
    summarize_if(is.numeric, ~max(date[. < (first_deciles %>% dplyr::pull(.))], na.rm=TRUE))
}

## 'timeplot' takes a dataset with:
##    one 'character' column describing [country|state|county|...]
##    a column of class 'Date'
##    one or more columns of class 'numeric' for the plotting variable
timeplot <- function(dataset, variable, log_scale = FALSE,
                     show_lines = TRUE,
                     show_points = FALSE,
                     show_average = FALSE,
                     hide_legend = TRUE,
                     align = "date",
                     lag_percentile = 10, topn = 3) {
  var_classes <- lapply(dataset, class)
  if(!quo_name(variable) %in% names(var_classes)[var_classes == "numeric"] |
     (show_average  & !paste0("avg_", quo_name(variable)) %in% names(var_classes)[var_classes == "numeric"])) {
    warning(paste0("plotting variable \'", quo_name(variable), "\' or its average not available"))
  } else {
    if(show_average) {
      avg_var <- paste0("avg_", quo_name(variable))
      if(!quo_name(avg_var) %in% names(var_classes)[var_classes == "numeric"]) {
        warning(paste0("plotting variable \'", quo_name(avg_var), "\' not available"))
      }
    }
    grouping_var <- names(var_classes)[var_classes == "character"][1]
    date_var <- names(var_classes)[var_classes == "Date"][1]
    if(is.na(topn)) topn <- dim(dataset %>% group_by_if(is.character) %>% summarize(n=n()))[1]
    plotdata <- dataset %>%
      filter_if(is.character, all_vars(!is.na(.))) %>%
      filter_if(is.numeric, any_vars(!is.na(.))) %>%
      group_by_if(is.character) %>%
      mutate(day = date - min(date))
    if(lag_percentile > 0) {
    } else {
      date_var <- "day"
    }
    if(align == "date") date_var = "date"
    plotdata %>%
      group_by(!!ensym(grouping_var)) %>%
    { lp <- ggplot(.) +
        geom_text(data = . %>%
                    filter(!!ensym(date_var) == max(!!ensym(date_var))) %>%
                    ungroup() %>%
                    top_n(topn, (!! variable)),
                  aes(label = !!ensym(grouping_var), color = !!ensym(grouping_var), x = !!ensym(date_var), y = (!! variable)),
                  hjust = "left",
                  vjust = "bottom") +
        ylim(1, NA) +
        theme_minimal() +
        ggtitle(paste0(quo_name(variable), " by ", grouping_var))
    if(show_lines) lp <- lp + geom_line(aes(x = !!ensym(date_var), y = !!variable, color = !!ensym(grouping_var)), size = 0.1)
    if(show_points) lp <- lp + geom_point(aes(x = !!ensym(date_var), y = !!variable, color = !!ensym(grouping_var)), size = 0.1)
    if(show_average) lp <- lp + geom_line(aes(x = !!ensym(date_var), y = !!ensym(avg_var), color = !!ensym(grouping_var)))
    if(hide_legend) lp <- lp + theme(legend.position = "none")
    if(log_scale) lp <- lp + scale_y_log10()
      lp
    }
  }
}

## country specific plot functions
country_timeplot <- function(countries, variable, topn = 10) {
  lag_day <- csse_by_country %>%
    filter(country %in% countries) %>%
    lag_days() %>%
    select(country, (!!variable)) %>%
    deframe()
  csse_by_country %>%
    filter(country %in% countries) %>%
    mutate("day" = date - min(date),
           "lag_date" = date - lag_day[country]) %>%
    ungroup() %>%
    mutate("lag_date" = lag_date - min(lag_date)) %>%
    group_by(country) %>%
    { ggplot(.) +
        geom_line(aes(x=lag_date, y=!!variable, color=country)) +
        geom_point(aes(x=lag_date, y=!!variable, color=country)) +
        geom_text(data = . %>%
                    filter(lag_date == max(lag_date)) %>%
                    ungroup() %>%
                    top_n(topn, (!! variable)),
                  aes(label = country, color = country, x = lag_date, y = (!! variable)),
                  hjust = "right",
                  vjust = "bottom") +
        theme_minimal() +
        theme(legend.position = "none") +
        xlab("Day of pandemic") +
        ylab(quo_name(variable)) +
        ggtitle(paste0(quo_name(variable), " by country as of ", latest_date))
    }
}

country_daily_timeplot <- function(countries, variable, topn = 10) {
  csse_by_country %>%
    filter(country %in% countries) %>%
    daily_from_cumulative() %>%
    ungroup() %>%
    group_by(country) %>%
    { ggplot(.) +
        geom_line(aes(x=date, y=!!variable, color=country)) +
        geom_point(aes(x=date, y=!!variable, color=country)) +
        geom_text(data = . %>%
                    filter(date == max(date)) %>%
                    ungroup() %>%
                    top_n(topn, (!! variable)),
                  aes(label = country, color = country, x = date, y = (!! variable)),
                  hjust = "right",
                  vjust = "bottom") +
        theme_minimal() +
        theme(legend.position = "none") +
        xlab("Day of pandemic") +
        ylab(quo_name(variable)) +
        ylim(0, NA) +
        ggtitle(paste0(quo_name(variable), " by country as of ", latest_date))
    }
}

country_log_timeplot <- function(dataset, countries, variable, topn = 10) {
  lag_day <- lag_days(dataset, countries) %>%
    select(country, (!!variable)) %>%
    deframe()
  dataset %>%
    filter(country %in% countries) %>%
    mutate("day" = date - min(date),
           "lag_date" = date - lag_day[country]) %>%
    ungroup() %>%
    mutate("lag_date" = lag_date - min(lag_date)) %>%
    group_by(country) %>%
    { ggplot(.) +
        geom_line(aes(x=lag_date, y=!!variable, color=country)) +
        geom_point(aes(x=lag_date, y=!!variable, color=country)) +
        geom_text(data = . %>%
                    filter(lag_date == max(lag_date)) %>%
                    ungroup() %>%
                    top_n(topn, (!! variable)),
                  aes(label = country, color = country, x = lag_date, y = (!! variable)),
                  hjust = "right",
                  vjust = "bottom") +
        scale_y_log10() +
        theme_minimal() +
        theme(legend.position = "none") +
        xlab("Day of pandemic") +
        ylab(quo_name(variable)) +
        ggtitle(paste0(quo_name(variable), " by country as of ", latest_date))
    }
}

country_state_plot <- function(countries, variable) {
  lag_day <- lag_days(csse_by_country_state, countries) %>%
    select(country, !!variable) %>%
    deframe()
  csse_by_country_state %>%
    filter(country %in% countries) %>%
    mutate("day" = date - min(date),
           "lag_date" = date - lag_day[country]) %>%
    rename("region" = country) %>%
    ggplot(aes(x=lag_date, y=!!variable, group=region)) +
    geom_point(aes(color=region)) +
    geom_line(aes(color=region)) +
    xlab("Day of pandemic") +
    ylab(quo_name(variable)) +
    ggtitle(paste(quo_name(variable), "as of", latest_date))
}

## state specific plot functions
state_log_plot <- function(dataset, variable, topn=5) {
  dataset %>%
    filter(!!variable >= 10) %>%
    group_by(state) %>%
    mutate(day = date - min(date), maxday = max(day)) %>%
    { ggplot(., aes(x=day, y=!!variable, group=state)) +
        geom_line(aes(color=state)) +
        geom_text(data = subset(., day == maxday), aes(label = state,
                                                       colour = state,
                                                       x = day,
                                                       y = !!variable), hjust = -.1) +
        xlab("Day of pandemic") +
        ylab(quo_name(variable)) +
        scale_y_log10() +
        theme_minimal() +  
        theme(legend.position = "none") +
        xlim(0, max(.$day) + 10) +
        ggtitle(paste(quo_name(variable), "as of", latest_date))
    }
  
}

state_map <- function(dataset, variable) {
  log_breaks <- 2 * 5**seq(0,8)
  states_map %>%
    left_join(
      dataset %>%
        mutate(fips = state_abbreviations[state],
               region = tolower(state)) %>%
        filter(!is.na(region)) %>%
        select(region, !! variable)
    ) %>%
    ggplot(aes(long, lat, group = group)) +
    geom_polygon(aes(fill = (!! variable)), color = "white") +
    scale_fill_viridis_c(option = "C",
                         name = quo_name(variable),
                         trans = "log",
                         breaks = log_breaks, labels = log_breaks) +
    coord_fixed(1.3) +
    theme_void() +
    ggtitle(paste(quo_name(variable), "as of", latest_date))
}

states_current_barplot <- function(dataset, variable) {
  dataset %>%
    arrange(desc(!!variable)) %>%
    ungroup() %>%
    top_n(10, !!variable) %>%
    ggplot(aes(x=reorder(state, !!variable), y=!!variable)) +
    geom_bar(stat="identity", fill="steelblue") +
    theme_minimal() +
    coord_flip() +
    xlab("state") +
    ylab("cumulative count") +
    ggtitle(paste(quo_name(variable), " as of", latest_date))
}

state_timeplot <- function(dataset, variable, topn=3) {
  dataset %>%
    filter(!is.na(state)) %>%
    group_by(state) %>%
    { ggplot(.) +
        geom_line(aes(x=date, y=(!!variable), color=state)) +
        geom_text(data = . %>%
                    filter(date == max(date)) %>%
                    ungroup() %>%
                    top_n(topn, (!! variable)),
                  aes(label = state, color = state, x = date, y = (!! variable)),
                  hjust = "right",
                  vjust = "bottom") +
        ylim(1, NA) +
        theme_minimal() +
        theme(legend.position = "none") +
        ggtitle(paste0(quo_name(variable), " by state"))
    }
}

## county specific plot functions
county_map <- function(dataset, variable, show_state=TRUE, log_scale=TRUE) {
  log_breaks <- 2 * 5**seq(0,8)
  map_data <- counties_map %>%
    filter(region %in% tolower(dataset$state)) %>%
    mutate(state = stringr::str_to_title(region),
           county = stringr::str_to_title(subregion))
  case_data <- dataset %>%
    mutate(subregion = tolower(county)) %>%
    filter(!is.na(subregion)) %>%
    dplyr::select(subregion, !! variable)
  if(show_state==TRUE) {
    lhs <- map_data
    rhs <- case_data
  } else {
    lhs <- case_data
    rhs <- map_data
  }
  map_data <- left_join(lhs, rhs) %>%
    dplyr::select(-region, -subregion)
  map_plot <- map_data %>%
    ggplot(aes(long, lat, group = group)) +
    geom_polygon(aes(fill = (!! variable)), color = NA) +
    coord_fixed(1.3) +
    theme_void() +
    ggtitle(paste(quo_name(variable), "as of", latest_date))
  if(log_scale == TRUE) map_plot +
    scale_fill_viridis_c(option = "C",
                         name = quo_name(variable),
                         trans = "log",
                         breaks = log_breaks, labels = log_breaks)
  else map_plot + 
    scale_fill_viridis_c(option = "C",
                         name = quo_name(variable))
}

county_barplot <- function(dataset, variable) {
  dataset %>%
    arrange(desc(!!variable)) %>%
    ungroup() %>%
    top_n(10, !!variable) %>%
    ggplot(aes(x=reorder(county, !!variable), y=!!variable)) +
    geom_bar(stat="identity", fill="steelblue") +
    theme_minimal() +
    coord_flip() +
    xlab("county") +
    ylab(quo_name(variable)) +
    ggtitle(paste(quo_name(variable), "by county, as of", latest_date))
}

county_timeplot <- function(dataset, variable, topn=3) {
  dataset %>%
    filter(!is.na(county)) %>%
    group_by(county) %>%
    { ggplot(.) +
        geom_line(aes(x=date, y=(!!variable), color=county)) +
        geom_text(data = . %>%
                    filter(date == max(date)) %>%
                    ungroup() %>%
                    top_n(topn, (!! variable)),
                  aes(label = county, color = county, x = date, y = (!! variable)),
                  hjust = "right",
                  vjust = "bottom") +
        ylim(0, NA) +
        theme_minimal() +
        theme(legend.position = "none") +
        ggtitle(paste0(quo_name(variable), " by county"))
    }
}

exp_fit <- function(dataset, political_unit, variable, window) {
  dataset %>%
    replace_na(list(cases = 0, deaths = 0, recovered = 0)) %>%
    group_by(!!political_unit) %>%
    mutate(days = date - min(date),
           fits = slide_index2(!!variable, days, .i=date,
                               .f=~lm(log(.x + 1) ~ .y),
                               .before = window,
                               .after = window),
           params = purrr::map(fits, tidy)) %>%
    unnest(params) %>%
    filter(term==".y") %>%
    dplyr::select(!!political_unit, date, estimate) %>%
    nest(date, estimate)
}


