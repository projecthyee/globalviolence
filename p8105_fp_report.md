P8105 Final Project: Determinants of Violence
================
My An Huynh, Jeffrey Lin, Soo Min You, Hyun Kim, Malika Top

# Import, Tidy & Merge Project Data

## Economic Determinants

``` r
gdp_df = 
  read_excel(
    path = "data/worldbank/API_NY.GDP.MKTP.CD_DS2_en_excel_v2_10034.xls",
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
                               destination = "country.name")) |>
  drop_na()
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `country = countrycode(country, origin = "iso3c", destination =
    ##   "country.name")`.
    ## Caused by warning:
    ## ! Some values were not matched unambiguously: AFE, AFW, ARB, CEB, CHI, CSS, EAP, EAR, EAS, ECA, ECS, EMU, EUU, FCS, HIC, HPC, IBD, IBT, IDA, IDB, IDX, INX, LAC, LCN, LDC, LIC, LMC, LMY, LTE, MEA, MIC, MNA, NAC, OED, OSS, PRE, PSS, PST, SAS, SSA, SSF, SST, TEA, TEC, TLA, TMN, TSA, TSS, UMC, WLD, XKX

``` r
inflation_df = 
  read_excel(
    path = "data/worldbank/API_FP.CPI.TOTL.ZG_DS2_en_excel_v2_9839.xls",
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
                               destination = "country.name")) |>
  drop_na()
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `country = countrycode(country, origin = "iso3c", destination =
    ##   "country.name")`.
    ## Caused by warning:
    ## ! Some values were not matched unambiguously: AFE, AFW, ARB, CEB, CHI, CSS, EAP, EAR, EAS, ECA, ECS, EMU, EUU, FCS, HIC, HPC, IBD, IBT, IDA, IDB, IDX, INX, LAC, LCN, LDC, LIC, LMC, LMY, LTE, MEA, MIC, MNA, NAC, OED, OSS, PRE, PSS, PST, SAS, SSA, SSF, SST, TEA, TEC, TLA, TMN, TSA, TSS, UMC, WLD, XKX

``` r
unemployment_df =
  read_excel(
    path = "data/imf/imf-dm-export-20241108.xls",
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
                               destination = "country.name")) |>
  drop_na()

hdi_index_df =
  read_csv(
    file = "data/undp/HDR23-24_Composite_indices_complete_time_series.csv",
    na = "") |>
  head(-11) |>
  pivot_longer(
    "hdi_2015":"hdi_2022",
    names_to = "year",
    values_to = "hdi_index"
  ) |>
  janitor::clean_names() |>
  select(country = "iso3", year, hdi_index) |>
  mutate(year = str_replace_all(year, "hdi_", ""),
         year = as.numeric(year),
         country = countrycode(country, origin = "iso3c", 
                              destination = "country.name"))
```

    ## Rows: 206 Columns: 1076
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr    (4): iso3, country, hdicode, region
    ## dbl (1072): hdi_rank_2022, hdi_1990, hdi_1991, hdi_1992, hdi_1993, hdi_1994,...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

## Social Determinants

``` r
crime_df = 
  read_excel(
    path = "data/unodc/data_cts_corruption_and_economic_crime.xlsx",
    skip = 2
  ) |>
  janitor::clean_names() |>
  mutate(year = as.numeric(year), 
         country = countrycode(iso3_code, origin = "iso3c", 
                              destination = "country.name")) |>
  filter(unit_of_measurement == "Rate per 100,000 population",
         between(year, 2015, 2023)) |>
  group_by(country, region, year) |>
  summarize(avg_crime_rate = mean(value))
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `country = countrycode(iso3_code, origin = "iso3c", destination
    ##   = "country.name")`.
    ## Caused by warning:
    ## ! Some values were not matched unambiguously: GBR_E_W, GBR_NI, GBR_S, IRQ_C, XKX

    ## `summarise()` has grouped output by 'country', 'region'. You can override using
    ## the `.groups` argument.

``` r
trafficking_df =
  read_excel(
    path = "data/unodc/data_glotip.xlsx",
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
                              destination = "country.name")) |>
  group_by(country, region, year) |>
  summarize(total_trafficking = sum(txt_value))
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `country = countrycode(iso3_code, origin = "iso3c", destination
    ##   = "country.name")`.
    ## Caused by warning:
    ## ! Some values were not matched unambiguously: CAR, CAS, EEU, MCA, MCN, SAF, SAM

    ## `summarise()` has grouped output by 'country', 'region'. You can override using
    ## the `.groups` argument.

``` r
drugs_2018_2022_df =
  read_excel(
    path = "data/unodc/7.1._Drug_seizures_2018-2022.xlsx",
    skip = 1
  ) |>
  janitor::clean_names() |>
  rename(year = reference_year) |>
  mutate(year = as.numeric(year), 
         country = countrycode(ms_code, origin = "iso3c",  
                              destination = "country.name")) |>
  group_by(country, region, year) |>
  summarize(total_drug_seizures = sum(kilograms))
```

    ## `summarise()` has grouped output by 'country', 'region'. You can override using
    ## the `.groups` argument.

``` r
firearms_df = 
    read_excel(
    path = "data/unodc/data_iafq_firearms_trafficking.xlsx",
    skip = 2
  ) |>
  janitor::clean_names() |>
  mutate(year = as.numeric(year), 
         country = countrycode(iso3_code, origin = "iso3c",  
                              destination = "country.name")) |>
  filter(indicator == "Arms seized",
         between(year, 2015, 2023),
         category == "Total") |>
  group_by(country, region, year) |>
  summarize(total_arms_seized = sum(value))
```

    ## `summarise()` has grouped output by 'country', 'region'. You can override using
    ## the `.groups` argument.

``` r
alcohol_consumption_df =
  read_csv(
    file = "data/who/data.csv", 
    na = ""
  ) |>
  janitor::clean_names() |>
  filter(dim1 == "Both sexes") |>
  select(country = spatial_dim_value_code, year = period, 
         alcohol_consumption = fact_value_numeric) |>
  mutate(year = as.numeric(year),
         country = countrycode(country, origin = "iso3c", 
                              destination = "country.name")) |>
  filter(between(year, 2015, 2023))
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

``` r
justice_personnel_df =
  read_excel(
    path = "data/unodc/data_cts_access_and_functioning_of_justice.xlsx",
    skip = 2
  ) |>
  janitor::clean_names() |>
  mutate(year = as.numeric(year), 
         country = countrycode(iso3_code, origin = "iso3c", 
                              destination = "country.name")) |>
  filter(indicator == "Criminal Justice Personnel",
         unit_of_measurement == "Rate per 100,000 population",
         between(year, 2015, 2023),
         sex == "Total") |>
  group_by(country, region, year) |>
  summarize(avg_personnel_rate = mean(value))
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `country = countrycode(iso3_code, origin = "iso3c", destination
    ##   = "country.name")`.
    ## Caused by warning:
    ## ! Some values were not matched unambiguously: GBR_E_W, GBR_NI, GBR_S, IRQ_C, IRQ_KRI, XKX

    ## `summarise()` has grouped output by 'country', 'region'. You can override using
    ## the `.groups` argument.

## Outcomes of Violence

``` r
homicide_rate_df = 
    readxl::read_excel(
    path = "data/unodc/data_cts_intentional_homicide.xlsx",
    skip = 2
  ) |>
  janitor::clean_names() |>
  filter(indicator == "Victims of intentional homicide",
         unit_of_measurement == "Rate per 100,000 population",
         between(year, 2015, 2023),
         sex == "Total",
         age == "Total") |>
  select(country = iso3_code, region, year, homicide_rate = value) |>
  mutate(year = as.numeric(year),
         country = countrycode(country, origin = "iso3c",
                               destination = "country.name"))
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `country = countrycode(country, origin = "iso3c", destination =
    ##   "country.name")`.
    ## Caused by warning:
    ## ! Some values were not matched unambiguously: GBR_E_W, GBR_NI, GBR_S, IRQ_C, XKX

``` r
violence_rate_df = 
    readxl::read_excel(
    path = "data/unodc/data_cts_violent_and_sexual_crime.xlsx",
    skip = 2
  ) |>
  janitor::clean_names() |>
  filter(indicator == "Violent offences",
         unit_of_measurement == "Rate per 100,000 population",
         between(year, 2015, 2023)) |>
  pivot_wider(
    names_from = indicator,
    values_from = value
  ) |>
  rename(violence_rate = "Violent offences") |>
  mutate(country = countrycode(iso3_code, origin = "iso3c",
                               destination = "country.name")) |>
  group_by(country, year) |>
  summarize(avg_violence_rate = mean(violence_rate)) 
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `country = countrycode(iso3_code, origin = "iso3c", destination
    ##   = "country.name")`.
    ## Caused by warning:
    ## ! Some values were not matched unambiguously: GBR_E_W, GBR_NI, GBR_S, IRQ_C, XKX

    ## `summarise()` has grouped output by 'country'. You can override using the
    ## `.groups` argument.

## Merge Datasets

``` r
merged_violence_df =
  left_join(homicide_rate_df, gdp_df) |>
  left_join(inflation_df) |>
  left_join(unemployment_df) |>
  left_join(hdi_index_df) |>
  left_join(crime_df) |>
  left_join(trafficking_df) |>
  left_join(drugs_2018_2022_df) |>
  left_join(firearms_df) |>
  left_join(justice_personnel_df) |>
  left_join(alcohol_consumption_df) |>
  mutate(country = as.factor(country))
```

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
