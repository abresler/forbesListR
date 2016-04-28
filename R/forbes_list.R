load_needed_packages <-
  function(required_packages = function_packages) {
    loaded_packages <- gsub("package:", "", search())
    package_to_load <- required_packages[!required_packages %in%
                                           loaded_packages]
    if (length(package_to_load) > 0) {
      lapply(package_to_load, library, character.only = T)
    }
  }

load_needed_packages(c("magrittr", "dplyr", "tidyr", "purrr", 'jsonlite', 'purrr',
                       "stringr"))

get_forbes_tables <-
  function() {
    load_needed_packages('dplyr')
    forbes_tables <-
      data_frame(
        name = c(
          'Top VCs',
          'Athletes',
          "Richest Families",
          'Forbes 400',
          'Billionaires',
          'Self Made Women',
          "Richest in Tech",
          'Hong Kong 50',
          'Australia 50',
          'China 50',
          'Taiwan 50',
          'India 50',
          'Japan 50',
          'Africa 50',
          "Korea 50",
          'Malaysia 50',
          "Philippines 50",
          'Singapore 50',
          'Indonesia 50',
          'Thailand 50',
          "Powerful People",
          "30 Under 30",
          "Powerful Women",
          "Celebrities",
          'MLB Valuations',
          'NASCAR Valuations',
          'NFL Valuations',
          'NBA Valuations',
          'NHL Valuations',
          'Soccer Valuations',
          "Top Colleges",
          "Top Business Schools",
          "Innovative Companies",
          "Small Companies",
          'Best Employers',
          "Largest Private Companies",
          'Asia 200',
          'Asia Fab 50',
          'Most Promising Companies',
          'Powerful Brands',
          "Growth Companies",
          "Best Employers",
          'Global 2000',
          "Best Countries for Business",
          'Best Cities for Business',
          'Best States for Business',
          'Best Small Cities for Business'
        ),
        uri = c(
          "midas",
          'athletes',
          'families',
          'forbes-400',
          'billionaires',
          'self-made-women',
          'richest-in-tech',
          'hong-kong-billionaires',
          'australia-billionaires',
          'china-billionaires',
          'taiwan-billionaires',
          'india-billionaires',
          'japan-billionaires',
          'africa-billionaires',
          'korea-billionaires',
          'malaysia-billionaires',
          'philippines-billionaires',
          'singapore-billionaires',
          'indonesia-billionaires',
          'thailand-billionaires',
          'powerful-people',
          NA,
          'power-women',
          'celebrities',
          'mlb-valuations',
          'nascar-valuations',
          'nfl-valuations',
          'nba-valuations',
          'nhl-valuations',
          'soccer-valuations',
          'top-colleges',
          'business-schools',
          'innovative-companies',
          'best-small-companies',
          'best-employers',
          'largest-private-companies',
          'asia200',
          'fab50',
          'most-promising-companies',
          'powerful-brands',
          'growth-companies',
          'best-employers',
          'global2000',
          'best-countries-for-business',
          'best-places-for-business',
          'best-states-for-business',
          'best-small-places-for-business'

        ),
        type = c(
          "person",
          'person',
          'person',
          'person',
          'person',
          'person',
          'person',
          'person',
          'person',
          'person',
          'person',
          'person',
          'person',
          'person',
          'person',
          'person',
          'person',
          'person',
          'person',
          'person',
          'person',
          'person',
          'person',
          'person',
          'organization',
          'organization',
          'organization',
          'organization',
          'organization',
          'organization',
          'organization',
          'organization',
          'organization',
          'organization',
          'organization',
          'organization',
          'organization',
          'organization',
          'organization',
          'organization',
          'organization',
          'organization',
          'organization',
          'place',
          'place',
          'place',
          'place'
        )
      ) %>%
      unique
    return(forbes_tables)
  }


get_year_forbes_list_data <-
  function(list = "NBA Valuations", year = 2016) {
    c('jsonlite', 'stringr', 'dplyr', 'magrittr') %>%
      load_needed_packages()

    forbes_tables <-
      get_forbes_tables()

    if (!list %in% forbes_tables$name) {
      stop("List can be:\n",
           forbes_tables$name %>% paste0(collapse = '\n'))
    }

    uri <-
      forbes_tables %>%
      mutate(name = name %>% str_to_lower()) %>%
      dplyr::filter(name == list %>% str_to_lower()) %>%
      .$uri

    type <-
      forbes_tables %>%
      mutate(name = name %>% str_to_lower()) %>%
      dplyr::filter(name == list %>% str_to_lower()) %>%
      .$type

    url <-
      'http://www.forbes.com/ajax/list/data?year=' %>%
      paste0(year, '&uri=', uri, '&type=', type) %>%
      unique()

    if (url %>% fromJSON() %>% as_data_frame %>% nrow == 0) {
      stop("Sorry Forbes ", list, " for ", year, " has no data")
    }

    json_data <-
      url %>%
      fromJSON(simplifyDataFrame = T, flatten = T) %>%
      as_data_frame()

    if(!'rank' %in% names(json_data)) {
      if('position' %in% names(json_data)) {
        json_data %<>%
          arrange(position)
      }
      json_data %<>%
        dplyr::mutate(rank = 1:n()) %>%
        dplyr::select(rank, everything())
    }

    if ('footNotes' %in% names(json_data)) {
      json_data %<>%
        dplyr::select(-footNotes)

    }

    column_class_df <-
      purrr::map(json_data, class) %>%
      unlist() %>%
      data.frame(class = .) %>%
      tbl_df %>%
      mutate(table = rownames(.),
             column = 1:n())


    if ('list' %in% column_class_df$class) {
      list_col <-
        column_class_df %>%
        dplyr::filter(class == 'list') %>%
        .$column

      list_df <-
        json_data[, list_col] %>%
        mutate(id.table = 1:n()) %>%
        unnest()
      all_list_data <-
        data_frame()
      for (id in list_df$id.table %>% unique) {
        if (list_df %>%
            dplyr::filter(id.table == id) %>%
            nrow > 1) {
          names <-
            list_df %>%
            dplyr::filter(id.table == id)

          names <-
            names[,2] %>%
            extract2(1) %>%
            str_trim %>%
            paste0(collapse = ', ')
        } else {
          names <-
            list_df %>%
            dplyr::filter(id.table == id) %>%
            .[,2] %>%
            extract2(1)
        }

        all_list_data %<>% bind_rows(data_frame(id.table = id,
                                                name = names))
      }
      names(all_list_data)[2] <-
        column_class_df %>%
        dplyr::filter(class == 'list') %>%
        .$table
      json_data <-
        json_data[, -list_col] %>%
        bind_cols(all_list_data) %>%
        dplyr::select(-id.table)
    }
    json_data %<>%
      mutate(year, list) %>%
      dplyr::select(year, list, everything())
    if ('title' %in% names(json_data) && list == "Top VCs") {
      json_data %<>%
        dplyr::rename(company = title) %>%
        mutate(company = company %>% str_replace_all('&#38;', '&'))
    }

    if (type == "person") {
      json_data %<>%
        mutate(
          url.bio.forbes = 'http://www.forbes.com/profile/' %>% paste0(uri),
          url.image = 'http://i.forbesimg.com/media/lists/people/' %>% paste0(imageUri, '_200x200.jpg')
        ) %>%
        dplyr::select(year, everything())

    }

    if (type == "organization") {
      json_data %<>%
        mutate(
          url.company.forbes = 'http://www.forbes.com/companies/' %>% paste0(uri),
          url.image = 'http://i.forbesimg.com/media/lists/companies/' %>% paste0(imageUri, '_200x200.jpg')
        ) %>%
        dplyr::select(year, everything())

    }

    if ('government' %in% names(json_data)) {
      json_data %<>%
        mutate(government = ifelse(government %>% is.na, F, government)) %>%
        dplyr::rename(is.government = government)
    }

    if ('lastName' %in% names(json_data)) {
      json_data %<>%
        dplyr::rename(last_name = lastName)
    }

    if ('timestamp' %in% names(json_data)) {
      json_data %<>%
        mutate(timestamp = (timestamp / 1000) %>% as.POSIXct(origin = "1970-01-01"))
    }

    if ('date' %in% names(json_data)) {
      json_data %<>%
        mutate(date = (date / 1000) %>% as.POSIXct(origin = "1970-01-01"))
    }

    if (list %in%  c("Billionaires", "Forbes 400")) {


      json_data %<>%
        dplyr::rename(
          net_worth.millions = worth,
          net_worth_change.millions = worthChange,
          real_time.rank = realTimeRank,
          real_time.position = realTimePosition,
          net_worth.realtime.millions = realTimeWorth,
          title.company = title
        )
    }

    if ('managementAssets' %in% names(json_data)) {
      json_data %<>%
        dplyr::rename(assets_under_management.millions = managementAssets)
    }

    if (list %in% c(
      'MLB Valuations',
      'NASCAR Valuations',
      'NFL Valuations',
      'NBA Valuations',
      'NHL Valuations',
      'Soccer Valuations'
    )) {
      json_data %<>%
        mutate(url.team.forbes = 'http://www.forbes.com/teams/' %>% paste0(uri))

      json_data %<>%
        dplyr::rename(
          team = name,
          valuation.millions = valueList,
          revenue.millions = revenue,
          operating_income.millions = operatingIncome,
          pct.debt = debtValue,
          pct.value_change = oneYearValueChange
        ) %>%
        mutate(
          expense.millions = revenue.millions - operating_income.millions,
          pct.debt = pct.debt / 100,
          debt.millions = pct.debt * valuation.millions,
          pct.value_change = pct.value_change / 100,
          revenue_multiple = valuation.millions / revenue.millions,
          ebitda_multiple = valuation.millions / operating_income.millions,
          ebitda_multiple = ifelse(ebitda_multiple < 0, NA, ebitda_multiple)
        ) %>%
        dplyr::select(
          year:team,
          valuation.millions,
          pct.value_change,
          debt.millions,
          pct.debt,
          revenue_multiple,
          ebitda_multiple,
          revenue.millions,
          expense.millions,
          operating_income.millions,
          everything()
        )

    }
    if (list %in% c('Growth Companies', "Best Employers", 'Powerful Brands',
                    'Innovative Companies', 'Small Companies', 'Most Promising Companies',
                    'Asia 200',
                    'Largest Private Companies', 'Global 2000'
                    )) {
      json_data %<>%
        dplyr::rename(company = name)
    }

    if (list %in% c('Growth Companies')) {
      json_data %<>%
        dplyr::rename(market_capitalization.millions = marketValue,
                      innovation_premium = innovationPremium,
                      pct.sales_growth = salesGrowth,
                      pct.income_change = incomeChange) %>%
        mutate(
          pct.sales_growth = pct.sales_growth / 100,
          innovation_premium = innovation_premium / 100,
          pct.income_change = pct.income_change / 100
        )
    }

    if (list %in% 'Athletes') {
      json_data %<>%
        dplyr::rename(sport = title) %>%
        mutate(sport = sport %>% str_replace_all('Athlete, ', ''))
    }

    if (list %in% c('Top Colleges', 'Top Business Schools')) {
      json_data %<>%
        dplyr::rename(school = name)
    }

    if ('returnOnEquity' %in% names(json_data)) {
      json_data %<>%
        dplyr::rename(pct.return_on_equity = returnOnEquity) %>%
        mutate(pct.return_on_equity = pct.return_on_equity / 100)
    }

    if ('netIncome' %in% names(json_data)) {
      json_data %<>%
        dplyr::rename(net_income.millions = netIncome)
    }

    if ('innovationPremium' %in% names(json_data)) {
      json_data %<>%
        dplyr::rename(innovation_premium = innovationPremium) %>%
        mutate(innovation_premium = innovation_premium / 100)
    }

    if ('salesGrowth' %in% names(json_data)) {
      json_data %<>%
        dplyr::rename(pct.sales_growth = salesGrowth) %>%
        mutate(pct.sales_growth = pct.sales_growth / 100)
    }

    if ('totalAnnualCost' %in% names(json_data)) {
      json_data %<>%
        dplyr::select(-totalAnnualCost)
    }

    if ('rank' %in% names(json_data)) {
      json_data %<>%
        arrange(rank)
    }

    if ('brandValue' %in% names(json_data)) {
      json_data %<>%
        dplyr::rename(brand_value.millions = brandValue)
    }

    if ('numberOfSiblingsEst' %in% names(json_data)) {
      json_data %<>%
        dplyr::rename(is.siblings.estimated = numberOfSiblingsEst)
    }

    if ('numberOfSiblings' %in% names(json_data)) {
      json_data %<>%
        dplyr::rename(siblings = numberOfSiblings)
    }

    if ('profits' %in% names(json_data)) {
      json_data %<>%
        dplyr::rename(profit.millions = profits)
    }

    if ('assets' %in% names(json_data)) {
      json_data %<>%
        dplyr::rename(assets.millions = assets)
    }

    if ('worth' %in% names(json_data)) {
      json_data %<>%
        dplyr::rename(net_worth.millions = worth)
    }

    if ('marketValue' %in% names(json_data)) {
      json_data %<>%
        dplyr::rename(market_capitalization.millions = marketValue)
    }

    if ('revenue' %in% names(json_data)) {
      json_data %<>%
        dplyr::rename(revenue.millions = revenue)
    }

    if ('advertising' %in% names(json_data)) {
      json_data %<>%
        dplyr::rename(advertising.millions = advertising)
    }

    if ('OneYearValueChange' %in% names(json_data)) {
      json_data %<>%
        dplyr::rename(pct.value_change = OneYearValueChange)
    }

    if ('earnings' %in% names(json_data)) {
      json_data %<>%
        dplyr::rename(earnings.millions = earnings)
    }

    if (names(json_data)[names(json_data) %in% c('pay', 'salary', 'endorsements')] %>% length > 0) {
      names(json_data)[names(json_data) %in% c('pay', 'salary', 'endorsements')] %<>%
        paste0(".millions")
    }

    if (type == "place") {
      json_data %<>%
        mutate(url.place.forbes = 'http://www.forbes.com/places/' %>% paste0(uri))
    }

    if (list == "Richest Families") {
      json_data %<>%
        dplyr::rename(family = name,
                      age.family = age) %>%
        mutate(family = family %>% str_replace_all("family",'') %>% str_trim)
    }

    if (list == "Best Countries for Business") {
      json_data %<>%
        dplyr::rename(country = name,
                      rank.innovation = innovation,
                      rank.tax_burden = taxBurden,
                      rank.monetary_freedom = monetaryFreedom,
                      pct.gdp_growth = gdpGrowth,
                      gdp_per_capita = gdpPerCapita,
                      pct.trade_balance.gdp = tradeBalance,
                      gdp.billions = gdp
                      ) %>%
        mutate(pct.trade_balance.gdp = pct.trade_balance.gdp / 100,
               pct.gdp_growth = pct.gdp_growth / 100)
    }

    if (list == "Best Cities for Business") {
      json_data %<>%
        dplyr::rename(city = name,
                      rank.education = education,
                      pct.job_growth.projected = projectedAnnualJobGrowth,
                      pct.high_tech_employement = hiTechEmployment,
                      rank.cost_doing_business = costOfDoingBusiness,
                      pct.college_graduates = collegeAttainment,
                      rank.job_growth = jobGrowth,
                      gross_metro_product.billions = grossMetroProduct
        ) %>%
        mutate(pct.job_growth.projected = pct.job_growth.projected / 100,
               pct.high_tech_employement = pct.high_tech_employement / 100,
               pct.college_graduates = pct.college_graduates / 100
               )
    }

    if (list == "Best Small Cities for Business") {
      json_data %<>%
        dplyr::rename(city = name,
                      rank.education = education,
                      rank.cost_doing_business = costOfDoingBusiness,
                      rank.job_growth = jobGrowth
        )
    }

    if (list == "Best States for Business") {
      json_data %<>%
        dplyr::rename(state = name,
                      rank.business_cost = businessCost,
                      rank.regulatory_environment = regulatoryEnvironment,
                      rank.economic_climate = economicClimate,
                      rank.growth_pospects = growthProspects,
                      rank.labor_supply = laborSupply,
                      rank.life_quality = lifeQuality,
                      pct.job_growth.projected = projectedAnnualJobGrowth,
                      pct.college_graduates = collegeAttainment,
                      gross_metro_product.billions = grossMetroProduct
        ) %>%
        mutate(pct.job_growth.projected = pct.job_growth.projected / 100,
               pct.college_graduates = pct.college_graduates / 100
        )
    }

    if (json_data$position %>% identical(json_data$rank)) {
      json_data %<>%
        dplyr::select(-position)
    }

    if (list %in% c('Top Business Schools')) {
      json_data %<>%
        dplyr::rename(salary.avg.post_mba = salary.millions,
                      tution = cost,
                      mba_gain.5_years = total5YearMbaGain,
                      payback_period = yearsToPayback,
                      salary.avg.pre_mba = preMbaSalary,
                      gmat.median = medianGmat,
                      students = annualEnrollment
                      )
    }

    if ('uri' %in% names(json_data)) {
      json_data %<>%
        dplyr::select(-uri)
    }

    if ('imageUri' %in% names(json_data)) {
      json_data %<>%
        dplyr::select(-imageUri)
    }

    return(json_data)
  }

get_year_forbes_list_data_safe <-
  failwith(NULL, get_year_forbes_list_data)

parse_forbes_bio_url <-
  function(url = "http://www.forbes.com/profile/floyd-mayweather/",
           return_message = T) {
    c("jsonlite", 'rvest', 'dplyr', 'tidyr', 'stringr') %>%
      load_needed_packages()

    names_df <-
      data_frame(
        name = c(
          'age',
          'wealth.source',
          'residence',
          'citizenship',
          'martial_status',
          'education',
          'net_worth',
          'salary_earnings.millions',
          'children',
          'endorsements.millions',
          'agent',
          'agency',
          'bonus.millions',
          'salary.millions',
          'self_made_score'
        ),
        item = c(
          "Age",
          "Source Of Wealth",
          "Residence",
          "Citizenship",
          "Marital Status",
          "Education",
          "Net Worth Over Time",
          'Salary/Winnings',
          'Children',
          'Endorsements',
          'Agent',
          'Agency',
          'Bonus',
          'Salary',
          'Self-Made Score'
        )
      )


    page <-
      url %>%
      read_html

    person <-
      page %>%
      html_nodes('#left_rail h1') %>%
      html_text()

    if ('#' %>% grepl(person)) {
      person %<>%
        str_replace('\\ ', '-')

      person <-
        person %>%
        str_split('\\-') %>%
        unlist()

      rank <-
        person[1] %>% extract_numeric()

      name <-
        person[2]

    } else {
      name <-
        person
      rank <-
        NA
    }

    items <-
      page %>%
      html_nodes('dt') %>%
      html_text %>%
      str_trim()

    values <-
      page %>%
      html_nodes('dd') %>%
      html_text %>%
      str_trim()

    if (!(items %>% length) == (values %>% length)) {
      items <-
        items[1:(values %>% length)]
    }


    bio <-
      page %>%
      html_nodes('.profile') %>%
      html_text() %>%
      str_trim() %>%
      str_split('  More ') %>%
      unlist %>%
      .[1]

    bio_df <-
      data_frame(item = items, value = values) %>%
      left_join(names_df) %>%
      suppressMessages() %>%
      dplyr::filter(!name %>% is.na) %>%
      dplyr::select(-item) %>%
      dplyr::rename(item = name) %>%
      dplyr::select(item, value, everything()) %>%
      mutate(rank, name) %>%
      spread(item, value) %>%
      mutate(age = age %>% as.numeric(),
             url.bio.forbes = url,
             bio) %>%
      dplyr::select(rank, name, bio, everything())

    if ('education' %in% names(bio_df)) {
      bio_df %<>%
        separate(
          education,
          sep = '\\, ',
          remove = F,
          into = c("type.degree", 'university')
        ) %>%
        suppressWarnings()

      bio_df %<>%
        mutate(
          university = ifelse(type.degree == education, type.degree, university),
          type.degree = ifelse(type.degree == education, NA, type.degree)
        )
    }
    if ('endorsements.millions' %in% names(bio_df)) {
      bio_df %<>%
        mutate(endorsements.millions = endorsements.millions %>% extract_numeric())
    }

    if ('salary_earnings.millions' %in% names(bio_df)) {
      bio_df %<>%
        mutate(salary_earnings.millions = salary_earnings.millions %>% extract_numeric())
    }

    if ('bonus.millions' %in% names(bio_df)) {
      bio_df %<>%
        mutate(bonus.millions = bonus.millions %>% extract_numeric())
    }

    if ('salary.millions' %in% names(bio_df)) {
      bio_df %<>%
        mutate(salary.millions = salary.millions %>% extract_numeric())
    }

    if ('residence' %in% names(bio_df)) {
      bio_df %<>%
        separate(
          residence,
          sep = '\\, ',
          into = c('city.residence', 'state.residence', 'country.residence'),
          remove = F
        ) %>%
        suppressWarnings()

      bio_df %<>%
        mutate(
          country.residence = ifelse(
            state.residence %>% nchar == 2,
            "United States",
            state.residence
          ),
          state.residence = ifelse(state.residence %>% nchar > 2, NA, state.residence)
        )
    }

    if ('wealth.source' %in% names(bio_df)) {
      bio_df %<>%
        separate(
          wealth.source,
          sep = '\\, ',
          into = c('type.wealth_source', 'source.wealth_source')
        ) %>%
        suppressWarnings()
    }

    if (return_message == T) {
      "You parsed and returned Forbes biography data for " %>%
        paste0(name) %>%
        message
    }

    return(bio_df)
  }

get_years_forbes_list_data <-
  function(years = 2012:2016,
           list_name = "NBA Valuations") {
    c('jsonlite', 'stringr', 'dplyr', 'magrittr', 'purrr') %>%
      load_needed_packages()
    all_data <-
      years %>%
      purrr::map({
        function(x)
          get_year_forbes_list_data_safe(year = x, list = list_name)
      }) %>%
      bind_rows() %>%
      arrange(desc(year))

    return(all_data)
  }

parse_forbes_bio_url_safe <-
  failwith(NULL, parse_forbes_bio_url)

get_year_list_forbes_bio_data <-
  function(year = 2016,
           list_name = "Top VCs",
           return_message = T) {
    c('jsonlite', 'stringr', 'dplyr', 'magrittr', 'purrr') %>%
      load_needed_packages()

    list_data <-
      get_year_forbes_list_data_safe(year = year, list = list_name)

    if (!'url.bio.forbes' %in% names(list_data)) {
      stop("Sorry " %>% paste0(list_name, ' has no biography urls'))
    }

    urls <-
      list_data$url.bio.forbes

    all_data <-
      urls %>%
      purrr::map(function(x)
        parse_forbes_bio_url_safe(url = x)) %>%
      bind_rows()

    all_data %<>%
      mutate(year, list = list_name) %>%
      dplyr::select(year, list, everything())

    if (return_message == T) {
      "You got forbes biography data for " %>%
        paste0(all_data %>% nrow, ' people on the ', year, ' ', list_name, ' list') %>%
        message()
    }
    return(all_data)
  }
