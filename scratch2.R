## given `dataset` with columns !!locality and !!variable

locality_barplot <- function(dataset, locality, variable, topn=10) {
  dataset %>%
    arrange(desc(!!variable)) %>%
    ungroup() %>%
    top_n(topn, !!variable) %>%
    ggplot(aes(x=reorder(!!locality, !!variable), y=!!variable)) +
    geom_bar(stat="identity", fill="steelblue") +
    theme_minimal() +
    coord_flip() +
    xlab(quo_name(locality)) +
    ylab(quo_name(variable)) +
    ggtitle(paste(quo_name(variable), "by", quo_name(locality), "as of", latest_date))
}


## plot variable grouped by column specified by locality
## ex:
## us_county
## complete %>%
##   filter(country=="US", state=="California") %>%
##   timeplot(locality = quo(county), variable = quo(cases))
locality_timeplot <- function(dataset, locality, variable, topn=3) {
  dataset %>%
    filter(!is.na(!!locality)) %>%
    group_by(!!locality) %>%
    { ggplot(.) +
        geom_line(aes(x=date, y=(!!variable), color=!!locality)) +
        geom_text(data = . %>%
                    filter(date == max(date)) %>%
                    ungroup() %>%
                    top_n(topn, (!!variable)),
                  aes(label = !!locality, color = !!locality, x = date, y = (!!variable)),
                  hjust = "right",
                  vjust = "bottom") +
        theme_minimal() +
        theme(legend.position = "none") +
        ggtitle(paste(quo_name(variable), "by", quo_name(locality)))
    }
}

complete %>%
   filter(country=="US", state=="California") %>%
   locality_timeplot(locality = quo(county), variable = quo(cases))

us_counties_xcurrent %>%
  filter(state=="California") %>%
  locality_barplot(locality = quo(county), variable = quo(cases))



bay_area_counties <- c("Santa Clara", "Alameda", "San Francisco", "San Mateo",
                       "Contra Costa", "Marin", "Sonoma", "Solano", "Napa")
