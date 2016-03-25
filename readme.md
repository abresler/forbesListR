
<!-- README.md is generated from README.Rmd. Please edit that file -->

forbesListR - An easy way to access the data contained lists maintained by the fine folks at [Forbes](http://www.forbes.com/) in [R](cran.r-project.org)

### Why I Built `forbesListR`?

Forbes is the preiminant maintainer of covering a wide range of business related topics including sports, entertainment, individual wealth, and locations. The lists are chocked full of phenomonal data that can be analyzed, visualized and merged with other data. Upon discovering that Forbes went to the pains of building an API, even though it is undocumented, I decided to build this package to wrap that API and make it as easy as possible for my fellow \#rstats brethren to access this data with a few simple functions.

As of now this package has 3 primary functions that I hope become widely used.

-   `get_year_forbes_list_data`: Gets the data contained in a Forbes list if it exists
-   `get_years_forbes_list_data`: Gets the data contained in multiple years of a specified list
-   `get_year_list_forbes_bio_data`: Gets the page specific for the people contained in a specific list

If and when Forbes adds new lists I will update this package to include them.

### Accessible Lists

-   Billionaires
-   Forbes 400
-   Top VCs
-   Athletes
-   Celebrities
-   NBA Valuations
-   MLB Valuations
-   NFL Valuations
-   NHL Valuations
-   Soccer Valuations
-   NASCAR Valuations
-   Powerful Brands
-   Growth Companies
-   Best Employers
-   Powerful People
-   Powerful Women
-   Top Colleges
-   Top Business Schools
-   Innovative Companies
-   Small Companies
-   Best Employers
-   Largest Private Companies
-   Global 2000
-   Richest Families
-   Self Made Women
-   Most Promising Companies
-   Best Countries for Business
-   Best Cities for Business
-   Best States for Business
-   Best Small Cities for Business
-   Richest in Tech
-   Hong Kong 50
-   Australia 50
-   China 50
-   Taiwan 50
-   India 50
-   Japan 50
-   Africa 50
-   Korea 50
-   Malaysia 50
-   Philippines 50
-   Singapore 50
-   Indonesia 50
-   Thailand 50
-   Asia 200
-   Asia Fab 50

### Installation

``` r
devtools::install_github("abresler/forbesListR")
```

### Usage

#### Get 2016 NBA Team Valuations

``` r
library(forbesListR)
nba_2016_values <- 
  get_year_forbes_list_data(list = "NBA Valuations", year = 2016)

nba_2016_values %>% 
  dplyr::select(year, team, valuation.millions) %>% 
  head(10) %>% 
  knitr::kable()
```

|  year| team                  |  valuation.millions|
|-----:|:----------------------|-------------------:|
|  2016| New York Knicks       |                3000|
|  2016| Los Angeles Lakers    |                2700|
|  2016| Chicago Bulls         |                2300|
|  2016| Boston Celtics        |                2100|
|  2016| Los Angeles Clippers  |                2000|
|  2016| Golden State Warriors |                1900|
|  2016| Brooklyn Nets         |                1700|
|  2016| Houston Rockets       |                1500|
|  2016| Dallas Mavericks      |                1400|
|  2016| Miami Heat            |                1300|

#### Get Top Venture Capitalists from 2012 to 2016

``` r
vcs_2012_2016 <- 
  get_years_forbes_list_data(years = 2012:2016, list_name = "Top VCs")
vcs_2012_2016 %>% 
  dplyr::glimpse()
# Observations: 475
# Variables: 15
# $ year           (int) 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 20...
# $ list           (chr) "Top VCs", "Top VCs", "Top VCs", "Top VCs", "Top VCs", "Top VCs", "Top VCs", "Top VCs", "Top...
# $ name           (chr) "Jim Goetz", "Steve Anderson", "Chris Sacca", "Peter Fenton", "Mary Meeker", "Josh Kopelman"...
# $ last_name      (chr) "Goetz", "Anderson", "Sacca", "Fenton", "Meeker", "Kopelman", "Shen", "Gurley", "Leone", "Th...
# $ company        (chr) "Sequoia Capital", "Baseline Ventures", "Lowercase Capital", "Benchmark", "Kleiner Perkins C...
# $ is.government  (lgl) FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, F...
# $ position       (int) NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ...
# $ rank           (int) 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 2...
# $ age            (int) 50, 47, 40, 43, 56, 44, 46, 49, 58, 48, 54, 59, 50, 51, 44, 52, 54, 48, 48, 51, 45, 64, 42, ...
# $ url.bio.forbes (chr) "http://www.forbes.com/profile/jim-goetz", "http://www.forbes.com/profile/steve-anderson", "...
# $ url.image      (chr) "http://i.forbesimg.com/media/lists/people/jim-goetz_200x200.jpg", "http://i.forbesimg.com/m...
# $ gender         (chr) "M", "M", "M", "M", "F", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M...
# $ country        (chr) "United States", "United States", "United States", "United States", "United States", "United...
# $ description    (chr) "A year-and-a-half after the sale of WhatsApp to Facebook for near $22 billion, Jim Goetz co...
# $ notableDeal    (chr) "WhatsApp", "Instagram", "Twitter", "Twitter", "Facebook", "LinkedIn", "Alibaba", "Uber", "F...
```

#### Get Detailed Biography Data for 2015 Top Athletes - Explore Earnings by Maritial Status

``` r
library(dplyr)
athletes_2015 <- 
  get_year_list_forbes_bio_data(year = 2015, list_name = 'Athletes')
# Warning: closing unused connection 5 (http://www.forbes.com/profile/sergio-aguero)
# Warning: closing unused connection 5 (http://www.forbes.com/profile/russell-westbrook)

athletes_2015 %>% 
  group_by(martial_status) %>% 
  summarise(earnings.millions = sum(salary_earnings.millions, na.rm = T)) %>% 
  arrange(desc(earnings.millions)) %>% 
  ungroup %>% 
  knitr::kable()
```

| martial\_status |  earnings.millions|
|:----------------|------------------:|
| Married         |             1257.3|
| Single          |              939.1|
| Divorced        |              659.0|
| Engaged         |               58.5|
| NA              |               33.1|
