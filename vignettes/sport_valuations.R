load_needed_packages(c('streamgraph', 'forbesListR', 'vegalite', 'waffle'))

nba_valuations <-
  2012:2016 %>%
  get_years_forbes_list_data(list_name = "NBA Valuations")

nhl_valuations <-
  2012:2016 %>%
  get_years_forbes_list_data(list_name = "NHL Valuations")

mlb_valuations <-
  2012:2016 %>%
  get_years_forbes_list_data(list_name = "MLB Valuations")

nfl_valuations <-
  2012:2016 %>%
  get_years_forbes_list_data(list_name = "NFL Valuations")

soccer_valuations <-
  2012:2016 %>%
  get_years_forbes_list_data(list_name = "Soccer Valuations")

all_valuation_data <-
  nba_valuations %>%
  bind_rows(nhl_valuations) %>%
  bind_rows(mlb_valuations) %>%
  bind_rows(soccer_valuations) %>%
  bind_rows(nfl_valuations) %>%
  mutate(list = list %>% str_replace_all(' Valuations', '')) %>%
  dplyr::filter(year %in% c(2012:2016))


## Waffle Charts

total_by_year <-
  all_valuation_data %>%
  dplyr::filter(year %in% c(2012:2015)) %>%
  group_by(year) %>%
  summarise(aggregate_valuation.billions = sum(valuation.millions, na.rm = T) / 1000)

totals <-
  total_by_year$aggregate_valuation.billions / 10

names(totals) <-
  total_by_year$year

library(ggplot2)
sports_waffle_valuations <-
  totals %>%
  waffle(flip = F, size = 2, rows = 8, xlab = '1 square = ~$10B',  pad = 1,
         colors = c("#FF5A5F","#FFB400", "#007A87", "#FFAA91", "#7B0051")) +
  labs(title = "Aggregate Estimated Professional Sports Valuations",
       subtitle = "MLB, NHL, NBA, NFL, & Soccer -- 2012-2015") +
  theme(plot.subtitle = element_text(
    size = 8,
    hjust = 0
  ),
  plot.title = element_text(size = 10, face = "bold.italic")
  )

## Streamgraph

summary_cols <-
  c('valuation.millions', 'revenue.millions', 'operating_income.millions', 'debt.millions')

summary_data <-
  all_valuation_data %>%
  dplyr::rename(sport = list) %>%
  group_by(year, sport) %>%
  summarise_each_(funs(sum(., na.rm = T)), summary_cols) %>%
  mutate(revenue.multiple = valuation.millions / revenue.millions,
         ebitda.multiple = valuation.millions / operating_income.millions) %>%
  ungroup

summary_data %>%
  streamgraph("sport",
              "valuation.millions",
              "year",
              offset="silhouette",
              interpolate="step"
              ) %>%
  sg_axis_x(1, "year", "%Y") %>%
  sg_fill_brewer(palette = "Spectral") %>%
  sg_legend(show=TRUE, label="Sport: ")


data_long <-
  summary_data %>%
  dplyr::select(-matches("multiple")) %>%
  gather(item, value, -c(sport, year)) %>%
  mutate(item = item %>% str_replace_all(".millions", ''))

vegalite(viewport_height = 1400) %>%
  add_data(data_long) %>%
  encode_y("sport", "nominal") %>%
  encode_x("value", "quantitative") %>%
  encode_color("item", "nominal") %>%
  facet_row("year", "nominal") %>%
  mark_bar()

vegalite(viewport_height = 1400) %>%
  add_data(data_long) %>%
  encode_y("year", "ordinal") %>%
  encode_x("value", "quantitative", aggregate = 'sum') %>%
  axis_x(title = "$ in M", format = "$3,d") %>%
  axis_y(title = '') %>%
  encode_color("sport", "nominal") %>%
  facet_row("item", "nominal") %>%
  mark_bar()



# statebins ---------------------------------------------------------------
# devtools::install_github("ramnathv/rcstatebin")
# devtools::install_github("abresler/forbesListR")
library(forbesListR)

athletes_2015 <-
  get_year_forbes_list_data(list = "Athletes", year = 2015)

athlete_2015_detail <-
  get_year_list_forbes_bio_data(year = 2015,
                              list_name = "Athletes")

state_data <-
  athlete_2015_detail %>%
  dplyr::filter(!is.na(state.residence)) %>%
  dplyr::rename(state = state.residence) %>%
  group_by(state) %>%
  summarise(
    athletes = n(),
    salary_earnings.millions = sum(salary_earnings.millions, na.rm = T)
            ) %>%
  mutate(mean.salary_earnings.millons = salary_earnings.millions / athletes) %>%
  gather(item, value, -state) %>%
  arrange(desc(item))


library(rcstatebin)
statebin(data = state_data,
         x = "state",
         y = "value",
         facet = "item",
         heading =  "<b>2015 Forbes Athlete Earnings by State</b>",
         colors = RColorBrewer::brewer.pal(8, 'PuRd'),
         footer = "<small>Source: Forbes <a href='http://www.forbes.com/athletes/'>(Data)</a> via forbesListR<br>source code: <a href='https://gist.github.com/abresler/52aefb3e473d076f7351'>here</a>",
         control = 'dropdown'
)
