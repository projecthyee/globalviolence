P8105 Final Project: Determinants of Global Violence
================
My An Huynh, Jeffrey Lin, Soo Min You, Hyun Kim, Malika Top

# Data: Source, Scraping Method & Cleaning

## Source

Since there are many determinants and indicators of violence, we chose
the indicators and outcomes that we thought were most interesting and
relevant in exploring violence from the following sources:

- International Monetary Fund (IMF)
  - Unemployment Rate
- United Nations Development Program (UNDP):
  - Human Development Index
- United Nations Office of Drugs and Crime (UNODC)
  - Corruption and Economic Crime
  - Criminal Justice Personnel
  - Drug Seizure (2018 - 2022)
  - Firearms Trafficking
  - Human Trafficking
  - Intentional Homicide
  - Violent and Sexual Crimes
- World Bank
  - Gross Domestic Product (GDP)
  - Inflation Rate (Measured by Consumer Price Index)
- World Health Organization (WHO)
  - Alcohol Consumption

Intentional homicide, and violent and sexual crimes were chosen as the
outcome variable to quantify violence.

## Scraping Method

The datasets were downloaded from the official websites of the sources
above. The names of the files were also changed accordingly for clarity
and to avoid confusion. For example, the alcohol consumption data file
was renamed from “data.csv” to “alcohol_consumption.csv”.

## Cleaning

### Economic Determinants

``` r
gdp_df = 
  read_excel(
    path = "data/worldbank/gdp.xls",
    sheet = "Data",
    skip = 3,
    na = ""
  ) |>
  select(country = "Country Code", "2015":"2023") |>
  pivot_longer(
    "2015":"2023",
    names_to = "year",
    values_to = "gdp"
  ) |>
  janitor::clean_names() |>
  mutate(year = as.numeric(year),
         country = countrycode(country, origin = "iso3c",
                               destination = "country.name",
                               nomatch = NA)) |>
  drop_na()
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `country = countrycode(...)`.
    ## Caused by warning:
    ## ! Some values were not matched unambiguously: AFE, AFW, ARB, CEB, CHI, CSS, EAP, EAR, EAS, ECA, ECS, EMU, EUU, FCS, HIC, HPC, IBD, IBT, IDA, IDB, IDX, INX, LAC, LCN, LDC, LIC, LMC, LMY, LTE, MEA, MIC, MNA, NAC, OED, OSS, PRE, PSS, PST, SAS, SSA, SSF, SST, TEA, TEC, TLA, TMN, TSA, TSS, UMC, WLD, XKX

``` r
inflation_df = 
  read_excel(
    path = "data/worldbank/inflation_rate.xls",
    sheet = "Data",
    skip = 3,
    na = ""
  ) |>
  select(country = "Country Code", "2015":"2023") |>
  pivot_longer(
    "2015":"2023",
    names_to = "year",
    values_to = "inflation_rate"
  ) |>
  janitor::clean_names() |>
  mutate(year = as.numeric(year),
         country = countrycode(country, origin = "iso3c",
                               destination = "country.name",
                               nomatch = NA)) |>
  drop_na()
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `country = countrycode(...)`.
    ## Caused by warning:
    ## ! Some values were not matched unambiguously: AFE, AFW, ARB, CEB, CHI, CSS, EAP, EAR, EAS, ECA, ECS, EMU, EUU, FCS, HIC, HPC, IBD, IBT, IDA, IDB, IDX, INX, LAC, LCN, LDC, LIC, LMC, LMY, LTE, MEA, MIC, MNA, NAC, OED, OSS, PRE, PSS, PST, SAS, SSA, SSF, SST, TEA, TEC, TLA, TMN, TSA, TSS, UMC, WLD, XKX

``` r
unemployment_df =
  read_excel(
    path = "data/imf/unemployment_rate.xls",
    range = "A1:AY116",
    na = "no data"
  ) |>
  select(country = "Unemployment rate (Percent)", "2015":"2023") |>
  drop_na(country) |>
  pivot_longer(
    "2015":"2023",
    names_to = "year",
    values_to = "unemployment_rate"
  ) |>
  janitor::clean_names() |>
  mutate(year = as.numeric(year),
         country = countrycode(country, origin = "country.name",
                               destination = "country.name",
                               nomatch = NA)) 

human_development_df =
  read_csv(
    file = "data/undp/human_development_index.csv",
    na = "") |>
  head(-11) |>
  pivot_longer(
    "hdi_2015":"hdi_2022",
    names_to = "year",
    values_to = "human_development_index"
  ) |>
  janitor::clean_names() |>
  select(country = "iso3", year, human_development_index) |>
  mutate(year = str_replace_all(year, "hdi_", ""),
         year = as.numeric(year),
         country = countrycode(country, origin = "iso3c",
                               destination = "country.name",
                               nomatch = NA))
```

    ## Rows: 206 Columns: 1076
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr    (4): iso3, country, hdicode, region
    ## dbl (1072): hdi_rank_2022, hdi_1990, hdi_1991, hdi_1992, hdi_1993, hdi_1994,...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

### Social Determinants

``` r
crime_rate_df = 
  read_excel(
    path = "data/unodc/corruption_economic_crime.xlsx",
    skip = 2
  ) |>
  janitor::clean_names() |>
  filter(unit_of_measurement == "Rate per 100,000 population") |>
  mutate(year = as.numeric(year),
         iso3_code = str_replace_all(iso3_code, "^GBR.*", "GBR"),
         iso3_code = str_replace_all(iso3_code, "^IRQ.*", "IRQ"),
         country = countrycode(iso3_code, origin = "iso3c",
                               destination = "country.name",
                               nomatch = NA)) |>
  group_by(country, region, year) |>
  summarize(avg_crime_rate = mean(value))
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `country = countrycode(...)`.
    ## Caused by warning:
    ## ! Some values were not matched unambiguously: XKX

    ## `summarise()` has grouped output by 'country', 'region'. You can override using
    ## the `.groups` argument.

``` r
justice_personnel_df =
  read_excel(
    path = "data/unodc/criminal_justice_personnel.xlsx",
    skip = 2
  ) |>
  janitor::clean_names() |>
  filter(indicator == "Criminal Justice Personnel",
         unit_of_measurement == "Rate per 100,000 population",
         sex == "Total") |>
  mutate(year = as.numeric(year),
         iso3_code = str_replace_all(iso3_code, "^GBR.*", "GBR"),
         iso3_code = str_replace_all(iso3_code, "^IRQ.*", "IRQ"),
         country = countrycode(iso3_code, origin = "iso3c",
                               destination = "country.name",
                               nomatch = NA)) |>
  group_by(country, region, year) |>
  summarize(avg_personnel_rate = mean(value))
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `country = countrycode(...)`.
    ## Caused by warning:
    ## ! Some values were not matched unambiguously: XKX

    ## `summarise()` has grouped output by 'country', 'region'. You can override using
    ## the `.groups` argument.

``` r
drugs_2018_2022_df =
  read_excel(
    path = "data/unodc/drug_seizures_2018_2022.xlsx",
    skip = 1
  ) |>
  janitor::clean_names() |>
  rename(year = reference_year) |>
  mutate(year = as.numeric(year),
         country = countrycode(ms_code, origin = "iso3c",
                               destination = "country.name",
                               nomatch = NA)) |>
  group_by(country, region, year) |>
  summarize(total_drug_seizures = sum(kilograms))
```

    ## `summarise()` has grouped output by 'country', 'region'. You can override using
    ## the `.groups` argument.

``` r
firearms_df = 
    read_excel(
    path = "data/unodc/firearms_trafficking.xlsx",
    skip = 2
  ) |>
  janitor::clean_names() |>
  mutate(year = as.numeric(year),
         iso3_code = str_replace_all(iso3_code, "^GBR.*", "GBR"),
         iso3_code = str_replace_all(iso3_code, "^IRQ.*", "IRQ"),
         country = countrycode(iso3_code, origin = "iso3c",
                               destination = "country.name",
                               nomatch = NA)) |>
  filter(indicator == "Arms seized",
         category == "Total") |>
  group_by(country, region, year) |>
  summarize(total_arms_seized = sum(value))
```

    ## `summarise()` has grouped output by 'country', 'region'. You can override using
    ## the `.groups` argument.

``` r
trafficking_df =
  read_excel(
    path = "data/unodc/human_trafficking.xlsx",
    skip = 2
  ) |>
  janitor::clean_names() |>
  filter(category == "Total",
         sex == "Total",
         age == "Total",
         txt_value != "<5") |>
  mutate(txt_value = str_replace_all(txt_value, ",", ""),
         txt_value = as.numeric(txt_value), 
         year = as.numeric(year), 
         country = countrycode(iso3_code, origin = "iso3c",
                               destination = "country.name",
                               nomatch = NA)) |>
  group_by(country, region, year) |>
  summarize(total_trafficking = sum(txt_value))
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `country = countrycode(...)`.
    ## Caused by warning:
    ## ! Some values were not matched unambiguously: CAR, CAS, EEU, MCA, MCN, SAF, SAM

    ## `summarise()` has grouped output by 'country', 'region'. You can override using
    ## the `.groups` argument.

``` r
alcohol_consumption_df =
  read_csv(
    file = "data/who/alcohol_consumption.csv", 
    na = ""
  ) |>
  janitor::clean_names() |>
  filter(dim1 == "Both sexes") |>
  select(country = spatial_dim_value_code, year = period, 
         alcohol_consumption = fact_value_numeric) |>
  mutate(year = as.numeric(year),
         country = countrycode(country, origin = "iso3c",
                               destination = "country.name",
                               nomatch = NA))
```

    ## Rows: 11799 Columns: 34
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (15): IndicatorCode, Indicator, ValueType, ParentLocationCode, ParentLo...
    ## dbl   (4): Period, FactValueNumeric, FactValueNumericLow, FactValueNumericHigh
    ## lgl  (14): IsLatestYear, Dim2 type, Dim2, Dim2ValueCode, Dim3 type, Dim3, Di...
    ## dttm  (1): DateModified
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

### Outcomes of Violence

``` r
homicide_rate_df = 
    readxl::read_excel(
    path = "data/unodc/intentional_homicide.xlsx",
    skip = 2
  ) |>
  janitor::clean_names() |>
  filter(indicator == "Victims of intentional homicide",
         unit_of_measurement == "Rate per 100,000 population",
         dimension == "Total",
         sex == "Total",
         age == "Total") |>
  mutate(year = as.numeric(year),
         iso3_code = str_replace_all(iso3_code, "^GBR.*", "GBR"),
         iso3_code = str_replace_all(iso3_code, "^IRQ.*", "IRQ"),
         country = countrycode(iso3_code, origin = "iso3c",
                               destination = "country.name",
                               nomatch = NA)) |>
  select(country, region, year, homicide_rate = value)
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `country = countrycode(...)`.
    ## Caused by warning:
    ## ! Some values were not matched unambiguously: XKX

``` r
violence_rate_df = 
    readxl::read_excel(
    path = "data/unodc/violent_sexual_crime.xlsx",
    skip = 2
  ) |>
  janitor::clean_names() |>
  filter(indicator == "Violent offences",
         unit_of_measurement == "Rate per 100,000 population") |>
  pivot_wider(
    names_from = indicator,
    values_from = value
  ) |>
  rename(violence_rate = "Violent offences") |>
  mutate(year = as.numeric(year),
         iso3_code = str_replace_all(iso3_code, "^GBR.*", "GBR"),
         iso3_code = str_replace_all(iso3_code, "^IRQ.*", "IRQ"),
         country = countrycode(iso3_code, origin = "iso3c",
                               destination = "country.name",
                               nomatch = NA)) |>
  group_by(country, year) |>
  summarize(avg_violence_rate = mean(violence_rate)) 
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `country = countrycode(...)`.
    ## Caused by warning:
    ## ! Some values were not matched unambiguously: XKX

    ## `summarise()` has grouped output by 'country'. You can override using the
    ## `.groups` argument.

For the economic determinants, all the datasets had years organized as
different columns, where each column represented values for that
specific year. Therefore, pivot_longer() was applied to pivot years and
their respective values into two columns.

For rates of all social determinants, we filtered and chose the data to
be rates per 100,000 people since it allows standardization for
comparability by adjusting for differences in population size.

The countrycode() function from the countrycode package was also
implemented in order to standardize the country names of each dataset
since some of the country names were represented inconsistently across
datasets. For example, South Korea was represented as “Korea (Republic
of)” and “Korea, Rep.”

### Merge Datasets

``` r
merged_violence_df =
  left_join(homicide_rate_df, violence_rate_df) |>
  left_join(gdp_df) |>
  left_join(inflation_df) |>
  left_join(unemployment_df) |>
  left_join(human_development_df) |>
  left_join(crime_rate_df) |>
  left_join(justice_personnel_df) |>
  left_join(drugs_2018_2022_df) |>
  left_join(firearms_df) |>
  left_join(trafficking_df) |>
  left_join(alcohol_consumption_df) |>
  mutate(country = as.factor(country),
         region = as.factor(region)) |>
  filter(between(year, 2015, 2023)) |>
  drop_na(country, region)
```

    ## Joining with `by = join_by(country, year)`
    ## Joining with `by = join_by(country, year)`
    ## Joining with `by = join_by(country, year)`
    ## Joining with `by = join_by(country, year)`
    ## Joining with `by = join_by(country, year)`
    ## Joining with `by = join_by(country, region, year)`
    ## Joining with `by = join_by(country, region, year)`
    ## Joining with `by = join_by(country, region, year)`
    ## Joining with `by = join_by(country, region, year)`
    ## Joining with `by = join_by(country, region, year)`
    ## Joining with `by = join_by(country, year)`

``` r
str(merged_violence_df)
```

    ## tibble [1,028 × 15] (S3: tbl_df/tbl/data.frame)
    ##  $ country                : Factor w/ 199 levels "Afghanistan",..: 1 6 2 5 188 9 10 4 12 13 ...
    ##  $ region                 : Factor w/ 5 levels "Africa","Americas",..: 3 1 4 4 3 2 3 5 5 4 ...
    ##  $ year                   : num [1:1028] 2015 2015 2015 2015 2015 ...
    ##  $ homicide_rate          : num [1:1028] 9.975 4.458 2.22 0 0.673 ...
    ##  $ avg_violence_rate      : num [1:1028] NA NA 5.51 41.26 2.16 ...
    ##  $ gdp                    : num [1:1028] 1.91e+10 9.05e+10 1.14e+10 2.79e+09 3.70e+11 ...
    ##  $ inflation_rate         : num [1:1028] -0.662 9.356 1.896 NA 4.07 ...
    ##  $ unemployment_rate      : num [1:1028] NA NA 17.1 3.9 NA 6.5 18.5 NA 6.1 6.2 ...
    ##  $ human_development_index: num [1:1028] 0.479 0.591 0.797 0.856 0.86 0.85 0.769 NA 0.933 0.91 ...
    ##  $ avg_crime_rate         : num [1:1028] NA NA 25.9 784.7 31.9 ...
    ##  $ avg_personnel_rate     : num [1:1028] NA NA 165 148 NA ...
    ##  $ total_drug_seizures    : num [1:1028] NA NA NA NA NA NA NA NA NA NA ...
    ##  $ total_arms_seized      : num [1:1028] NA NA NA NA NA NA NA NA NA NA ...
    ##  $ total_trafficking      : num [1:1028] NA NA 109 NA 34 ...
    ##  $ alcohol_consumption    : num [1:1028] 0.009 7.76 5.04 10.76 2.37 ...

The final merged dataset includes 1028 rows and 15 columns, including
country, region, year, homicide rate, average violence offence rate,
gdp, inflation rate, unemployment rate, average crime rate, average
(criminal justice) personnel rate, total drug seized (2018 - 2022),
total arm seized, total trafficking and alcohol consumption as
variables. There is a total of 163 distinct countries.

After merging the datasets, country and region were converted to
categorical variables. Rows with NA values for country and region were
dropped, and the dataset was filtered for the years between 2015 and
2023.

# EDA

## Visualization

``` r
merged_violence_df |>
  group_by(region, year) |>
  summarize(avg_homicide_rate = mean(homicide_rate)) |>
  ggplot(aes(y = avg_homicide_rate, x = year, color = region)) +
  geom_line()
```

    ## `summarise()` has grouped output by 'region'. You can override using the
    ## `.groups` argument.

![](p8105_fp_report_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

## Data Transformation

## Regression

Split data into training and testing + visualize distributions

``` r
train_df = sample_frac(merged_violence_df, size = 0.8)
test_df = anti_join(merged_violence_df, train_df, by = "gdp")

plot_distributions = function(column, name) {
  
  if(is.numeric(column) & name != "year") {
    ggplot(train_df, aes(x = column)) +
    geom_density() +
    labs(title = paste("Distribution of", name))
  }
  
}

train_list = colnames(train_df)
map(train_list, \(x) plot_distributions(pull(train_df, x), x))
```

    ## [[1]]
    ## NULL
    ## 
    ## [[2]]
    ## NULL
    ## 
    ## [[3]]
    ## NULL
    ## 
    ## [[4]]

![](p8105_fp_report_files/figure-gfm/train/test%20+%20distribution-1.png)<!-- -->

    ## 
    ## [[5]]

    ## Warning: Removed 199 rows containing non-finite outside the scale range
    ## (`stat_density()`).

![](p8105_fp_report_files/figure-gfm/train/test%20+%20distribution-2.png)<!-- -->

    ## 
    ## [[6]]

    ## Warning: Removed 18 rows containing non-finite outside the scale range
    ## (`stat_density()`).

![](p8105_fp_report_files/figure-gfm/train/test%20+%20distribution-3.png)<!-- -->

    ## 
    ## [[7]]

    ## Warning: Removed 83 rows containing non-finite outside the scale range
    ## (`stat_density()`).

![](p8105_fp_report_files/figure-gfm/train/test%20+%20distribution-4.png)<!-- -->

    ## 
    ## [[8]]

    ## Warning: Removed 249 rows containing non-finite outside the scale range
    ## (`stat_density()`).

![](p8105_fp_report_files/figure-gfm/train/test%20+%20distribution-5.png)<!-- -->

    ## 
    ## [[9]]

    ## Warning: Removed 51 rows containing non-finite outside the scale range
    ## (`stat_density()`).

![](p8105_fp_report_files/figure-gfm/train/test%20+%20distribution-6.png)<!-- -->

    ## 
    ## [[10]]

    ## Warning: Removed 215 rows containing non-finite outside the scale range
    ## (`stat_density()`).

![](p8105_fp_report_files/figure-gfm/train/test%20+%20distribution-7.png)<!-- -->

    ## 
    ## [[11]]

    ## Warning: Removed 378 rows containing non-finite outside the scale range
    ## (`stat_density()`).

![](p8105_fp_report_files/figure-gfm/train/test%20+%20distribution-8.png)<!-- -->

    ## 
    ## [[12]]

    ## Warning: Removed 484 rows containing non-finite outside the scale range
    ## (`stat_density()`).

![](p8105_fp_report_files/figure-gfm/train/test%20+%20distribution-9.png)<!-- -->

    ## 
    ## [[13]]

    ## Warning: Removed 554 rows containing non-finite outside the scale range
    ## (`stat_density()`).

![](p8105_fp_report_files/figure-gfm/train/test%20+%20distribution-10.png)<!-- -->

    ## 
    ## [[14]]

    ## Warning: Removed 343 rows containing non-finite outside the scale range
    ## (`stat_density()`).

![](p8105_fp_report_files/figure-gfm/train/test%20+%20distribution-11.png)<!-- -->

    ## 
    ## [[15]]

    ## Warning: Removed 239 rows containing non-finite outside the scale range
    ## (`stat_density()`).

![](p8105_fp_report_files/figure-gfm/train/test%20+%20distribution-12.png)<!-- -->

After looking at the distributions of all variables, all variables are
skewed right, except for human development index which is bimodal and
slightly left-skewed. I will apply ln transformations and box-cox
transformations to these variables, and use the Shapiro-Wilk test to
test for normality. Even though it is not necessary to normalize the
predictors, this step will stablize variance and reduce
heteroscedasticity. I think it will be helpful for further steps with
model.

Transformation step involves writing a function for ln transformation
and a function for box_cox transformation. The functions will be mapped
into the nested listcol which includes all continuous variables in the
train_df dataset.

``` r
ln_transform = function(value) {
  return(log(abs(value)))
}

ln_train_df = 
  train_df |>
  mutate(across(c(homicide_rate:alcohol_consumption), 
                ln_transform))

map(train_list, \(x) plot_distributions(pull(ln_train_df, x), x))
```

    ## [[1]]
    ## NULL
    ## 
    ## [[2]]
    ## NULL
    ## 
    ## [[3]]
    ## NULL
    ## 
    ## [[4]]

    ## Warning: Removed 14 rows containing non-finite outside the scale range
    ## (`stat_density()`).

![](p8105_fp_report_files/figure-gfm/ln_transform-1.png)<!-- -->

    ## 
    ## [[5]]

    ## Warning: Removed 201 rows containing non-finite outside the scale range
    ## (`stat_density()`).

![](p8105_fp_report_files/figure-gfm/ln_transform-2.png)<!-- -->

    ## 
    ## [[6]]

    ## Warning: Removed 18 rows containing non-finite outside the scale range
    ## (`stat_density()`).

![](p8105_fp_report_files/figure-gfm/ln_transform-3.png)<!-- -->

    ## 
    ## [[7]]

    ## Warning: Removed 83 rows containing non-finite outside the scale range
    ## (`stat_density()`).

![](p8105_fp_report_files/figure-gfm/ln_transform-4.png)<!-- -->

    ## 
    ## [[8]]

    ## Warning: Removed 249 rows containing non-finite outside the scale range
    ## (`stat_density()`).

![](p8105_fp_report_files/figure-gfm/ln_transform-5.png)<!-- -->

    ## 
    ## [[9]]

    ## Warning: Removed 51 rows containing non-finite outside the scale range
    ## (`stat_density()`).

![](p8105_fp_report_files/figure-gfm/ln_transform-6.png)<!-- -->

    ## 
    ## [[10]]

    ## Warning: Removed 221 rows containing non-finite outside the scale range
    ## (`stat_density()`).

![](p8105_fp_report_files/figure-gfm/ln_transform-7.png)<!-- -->

    ## 
    ## [[11]]

    ## Warning: Removed 378 rows containing non-finite outside the scale range
    ## (`stat_density()`).

![](p8105_fp_report_files/figure-gfm/ln_transform-8.png)<!-- -->

    ## 
    ## [[12]]

    ## Warning: Removed 485 rows containing non-finite outside the scale range
    ## (`stat_density()`).

![](p8105_fp_report_files/figure-gfm/ln_transform-9.png)<!-- -->

    ## 
    ## [[13]]

    ## Warning: Removed 555 rows containing non-finite outside the scale range
    ## (`stat_density()`).

![](p8105_fp_report_files/figure-gfm/ln_transform-10.png)<!-- -->

    ## 
    ## [[14]]

    ## Warning: Removed 343 rows containing non-finite outside the scale range
    ## (`stat_density()`).

![](p8105_fp_report_files/figure-gfm/ln_transform-11.png)<!-- -->

    ## 
    ## [[15]]

    ## Warning: Removed 247 rows containing non-finite outside the scale range
    ## (`stat_density()`).

![](p8105_fp_report_files/figure-gfm/ln_transform-12.png)<!-- -->

``` r
library(MASS)
```

    ## 
    ## Attaching package: 'MASS'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

``` r
boxcox_transform = function(value) {
  value = value + abs(min(value, na.rm = TRUE)) + 0.00001
  
  boxcox_result = boxcox(value ~ 1, plotit = FALSE)
  lambda = boxcox_result$x[which.max(boxcox_result$y)]
  if(lambda == 0) {
    return(log(value))
  }
  return((value^lambda - 1) / lambda)
}

boxcox_train_df = 
  train_df |>
  mutate(across(c(homicide_rate:alcohol_consumption), 
                boxcox_transform))

map(train_list, \(x) plot_distributions(pull(boxcox_train_df, x), x))
```

    ## [[1]]
    ## NULL
    ## 
    ## [[2]]
    ## NULL
    ## 
    ## [[3]]
    ## NULL
    ## 
    ## [[4]]

![](p8105_fp_report_files/figure-gfm/boxcox_transform-1.png)<!-- -->

    ## 
    ## [[5]]

    ## Warning: Removed 199 rows containing non-finite outside the scale range
    ## (`stat_density()`).

![](p8105_fp_report_files/figure-gfm/boxcox_transform-2.png)<!-- -->

    ## 
    ## [[6]]

    ## Warning: Removed 18 rows containing non-finite outside the scale range
    ## (`stat_density()`).

![](p8105_fp_report_files/figure-gfm/boxcox_transform-3.png)<!-- -->

    ## 
    ## [[7]]

    ## Warning: Removed 83 rows containing non-finite outside the scale range
    ## (`stat_density()`).

![](p8105_fp_report_files/figure-gfm/boxcox_transform-4.png)<!-- -->

    ## 
    ## [[8]]

    ## Warning: Removed 249 rows containing non-finite outside the scale range
    ## (`stat_density()`).

![](p8105_fp_report_files/figure-gfm/boxcox_transform-5.png)<!-- -->

    ## 
    ## [[9]]

    ## Warning: Removed 51 rows containing non-finite outside the scale range
    ## (`stat_density()`).

![](p8105_fp_report_files/figure-gfm/boxcox_transform-6.png)<!-- -->

    ## 
    ## [[10]]

    ## Warning: Removed 215 rows containing non-finite outside the scale range
    ## (`stat_density()`).

![](p8105_fp_report_files/figure-gfm/boxcox_transform-7.png)<!-- -->

    ## 
    ## [[11]]

    ## Warning: Removed 378 rows containing non-finite outside the scale range
    ## (`stat_density()`).

![](p8105_fp_report_files/figure-gfm/boxcox_transform-8.png)<!-- -->

    ## 
    ## [[12]]

    ## Warning: Removed 484 rows containing non-finite outside the scale range
    ## (`stat_density()`).

![](p8105_fp_report_files/figure-gfm/boxcox_transform-9.png)<!-- -->

    ## 
    ## [[13]]

    ## Warning: Removed 554 rows containing non-finite outside the scale range
    ## (`stat_density()`).

![](p8105_fp_report_files/figure-gfm/boxcox_transform-10.png)<!-- -->

    ## 
    ## [[14]]

    ## Warning: Removed 343 rows containing non-finite outside the scale range
    ## (`stat_density()`).

![](p8105_fp_report_files/figure-gfm/boxcox_transform-11.png)<!-- -->

    ## 
    ## [[15]]

    ## Warning: Removed 239 rows containing non-finite outside the scale range
    ## (`stat_density()`).

![](p8105_fp_report_files/figure-gfm/boxcox_transform-12.png)<!-- -->

## Stepwise Regression

``` r
model = lm(homicide_rate ~ year + gdp + inflation_rate + unemployment_rate 
           + human_development_index + avg_crime_rate + avg_personnel_rate 
           + total_drug_seizures + total_arms_seized + total_trafficking 
           + alcohol_consumption, data = na.omit(merged_violence_df))

step_model = step(model, direction = "both", trace = 0)
summary(step_model)
```

    ## 
    ## Call:
    ## lm(formula = homicide_rate ~ gdp + unemployment_rate + human_development_index + 
    ##     avg_personnel_rate + total_arms_seized, data = na.omit(merged_violence_df))
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -13.2360  -4.3080   0.3352   2.0792  30.7240 
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)              7.775e+01  1.007e+01   7.719 7.73e-11 ***
    ## gdp                     -1.626e-12  6.655e-13  -2.443  0.01722 *  
    ## unemployment_rate       -5.061e-01  2.372e-01  -2.134  0.03653 *  
    ## human_development_index -8.697e+01  1.211e+01  -7.179 7.23e-10 ***
    ## avg_personnel_rate       5.504e-02  1.834e-02   3.002  0.00377 ** 
    ## total_arms_seized        1.143e-04  3.732e-05   3.062  0.00317 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 7.233 on 67 degrees of freedom
    ## Multiple R-squared:  0.5363, Adjusted R-squared:  0.5017 
    ## F-statistic:  15.5 on 5 and 67 DF,  p-value: 4.148e-10
