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

The rest of this document will be filled in during the live session on
Thursday.
