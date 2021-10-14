Code-along, Week 04: Fisheries
================
Alex Homer
14 October 2021

``` r
library(tidyverse)
```

## Read data

The data are drawn from the “Tidy Tuesday” project: [2021 Week 42:
Global
Fishing](https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-10-12/readme.md)
(credit:
[OurWorldinData.org](https://ourworldindata.org/seafood-production),
from Hannah Ritchie and Max Roser).

``` r
farmed <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/aquaculture-farmed-fish-production.csv"
)
captured_vs_farmed <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/capture-fisheries-vs-aquaculture.csv"
)
captured <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/capture-fishery-production.csv"
)
consumption <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/fish-and-seafood-consumption-per-capita.csv"
)
stock <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/fish-stocks-within-sustainable-levels.csv"
)
fishery <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/global-fishery-catch-by-sector.csv"
)
production <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/seafood-and-fish-production-thousand-tonnes.csv"
)
```

We have a lot of data frames this week!

## Data cleanup

``` r
production_cleaned <- production %>%
  rename(
    entity = Entity,
    code = Code,
    yr = Year, #Avoid conflict with the `year` function
    balance_pelagic_fish = `Commodity Balances - Livestock and Fish Primary Equivalent - Pelagic Fish - 2763 - Production - 5510 - tonnes`,
    balance_crustaceans = `Commodity Balances - Livestock and Fish Primary Equivalent - Crustaceans - 2765 - Production - 5510 - tonnes`,
    balance_cephalopods = `Commodity Balances - Livestock and Fish Primary Equivalent - Cephalopods - 2766 - Production - 5510 - tonnes`,
    balance_demersal_fish = `Commodity Balances - Livestock and Fish Primary Equivalent - Demersal Fish - 2762 - Production - 5510 - tonnes`,
    balance_freshwater_fish = `Commodity Balances - Livestock and Fish Primary Equivalent - Freshwater Fish - 2761 - Production - 5510 - tonnes`,
    balance_other_molluscs = `Commodity Balances - Livestock and Fish Primary Equivalent - Molluscs, Other - 2767 - Production - 5510 - tonnes`,
    balance_other_marine_fish = `Commodity Balances - Livestock and Fish Primary Equivalent - Marine Fish, Other - 2764 - Production - 5510 - tonnes`
  )

production_longer <- production_cleaned %>%
  pivot_longer(
    starts_with("balance"),
    names_to = "marine_type",
    values_to = "balance",
    names_prefix = "balance_"
  ) %>%
  mutate(
    marine_type = marine_type %>%
      str_to_sentence() %>%
      str_replace_all(pattern = "_", replacement = " ")
  )
```

``` r
fish_or_not_totals <- production_longer %>%
  mutate(
    is_fish = if_else(
      marine_type %in% c("Crustaceans", "Cephalopods", "Other molluscs"),
      true = "Total not fish",
      false = "Total fish"
    )
  ) %>%
  group_by(entity, code, yr, is_fish) %>%
  summarise(balance = sum(balance)) %>%
  rename(marine_type = is_fish)
```

    ## `summarise()` has grouped output by 'entity', 'code', 'yr'. You can override using the `.groups` argument.

``` r
production_with_totals <- production_longer %>%
  bind_rows(fish_or_not_totals) %>%
  arrange(entity, code, yr) %>%
  filter(entity == "Africa")
```
