P8105 Final Project: Determinants of Global Violence
================
My An Huynh, Jeffrey Lin, Soo Min You, Hyun Kim, Malika Top

## Motivation

## Initial Questions

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

- Intentional homicide:
  - counting unit: individual victim of homicide
  - classification:
    - situational context: organized crime, interpersonal (excluding
      familial/intimate), socio-political
    - relationship to perpetrator: intimate partner, family member,
      friend, colleague, etc.
    - mechanism: firearm, weapon, physical force
- Violent and sexual crimes:
  - counting unit: number of individual offences per 100,000 population
  - classification of offenses:
    - rape
    - serious assault
    - kidnapping
    - sexual violence
  - NOTE: some countries used other counting unit (a series of offenses
    to form a case, or multiple cases to form an investigation)

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
  select(country = 2, "2015":"2023")

inflation_df = 
  read_excel(
    path = "data/worldbank/inflation_rate.xls",
    sheet = "Data",
    skip = 3,
    na = ""
  ) |>
  select(country = 2, "2015":"2023") 

unemployment_df =
  read_excel(
    path = "data/imf/unemployment_rate.xls",
    range = "A1:AY116",
    na = "no data"
  ) |>
  select(country = 1, "2015":"2023") 

human_develop_df =
  read_csv(
    file = "data/undp/human_development_index.csv",
    na = "") |>
  head(-11) |>
  rename_with(gsub, pattern = "^hdi_", replacement = "") |>
  select(country = 1, "2015":"2022")
```

    ## Rows: 206 Columns: 1076
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr    (4): iso3, country, hdicode, region
    ## dbl (1072): hdi_rank_2022, hdi_1990, hdi_1991, hdi_1992, hdi_1993, hdi_1994,...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

### Function to pivot data and clean country names

``` r
pivot_df = function(data, name, value = "iso3c") {
  
  data |>
    pivot_longer(
      cols = -country,
      names_to = "year",
      values_to = paste(name)
    ) |>
    janitor::clean_names() |>
    mutate(year = as.numeric(year),
          country = countrycode(country, 
                                origin = value,
                                destination = "country.name",
                                nomatch = NA)) |>
    drop_na()
}

gdp_df = pivot_df(gdp_df, "gdp")
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `country = countrycode(...)`.
    ## Caused by warning:
    ## ! Some values were not matched unambiguously: AFE, AFW, ARB, CEB, CHI, CSS, EAP, EAR, EAS, ECA, ECS, EMU, EUU, FCS, HIC, HPC, IBD, IBT, IDA, IDB, IDX, INX, LAC, LCN, LDC, LIC, LMC, LMY, LTE, MEA, MIC, MNA, NAC, OED, OSS, PRE, PSS, PST, SAS, SSA, SSF, SST, TEA, TEC, TLA, TMN, TSA, TSS, UMC, WLD, XKX

``` r
inflation_df = pivot_df(inflation_df, "inflation_rate")
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `country = countrycode(...)`.
    ## Caused by warning:
    ## ! Some values were not matched unambiguously: AFE, AFW, ARB, CEB, CHI, CSS, EAP, EAR, EAS, ECA, ECS, EMU, EUU, FCS, HIC, HPC, IBD, IBT, IDA, IDB, IDX, INX, LAC, LCN, LDC, LIC, LMC, LMY, LTE, MEA, MIC, MNA, NAC, OED, OSS, PRE, PSS, PST, SAS, SSA, SSF, SST, TEA, TEC, TLA, TMN, TSA, TSS, UMC, WLD, XKX

``` r
human_develop_index_df = pivot_df(human_develop_df, "hdi")
unemployment_df = pivot_df(unemployment_df, "unemployment_rate", "country.name")
```

### Social Determinants

``` r
econ_crime_df = 
  read_excel(
    path = "data/unodc/corruption_economic_crime.xlsx",
    skip = 2
  ) |>
  janitor::clean_names() |>
  filter(unit_of_measurement == "Rate per 100,000 population")

justice_personnel_df =
  read_excel(
    path = "data/unodc/criminal_justice_personnel.xlsx",
    skip = 2
  ) |>
  janitor::clean_names() |>
  filter(indicator == "Criminal Justice Personnel",
         sex == "Total") |>
      filter(unit_of_measurement == "Rate per 100,000 population")

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
  mutate(value = str_replace_all(txt_value, ",", ""),
         value = as.numeric(value))

alcohol_consumption_df =
  read_csv(
    file = "data/who/alcohol_consumption.csv", 
    na = ""
  ) |>
  janitor::clean_names() |>
  filter(dim1 == "Both sexes") |>
  select(iso3_code = spatial_dim_value_code, 
         year = period, value = fact_value_numeric)
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

### Function to pivot data and clean country names

``` r
clean_country_names = function(data, rate) {
  
  data = 
    data |>
    mutate(year = as.numeric(year),
           iso3_code = str_replace_all(iso3_code, "^GBR.*", "GBR"),
           iso3_code = str_replace_all(iso3_code, "^IRQ.*", "IRQ"),
           country = countrycode(iso3_code, origin = "iso3c",
                                 destination = "country.name",
                                 nomatch = NA))

  if("region" %in% colnames(data)) {
    data |>
      group_by(country, region, year) |>
      summarize(rate = mean(value))
  }
  
  else {
    data |>
      group_by(country, year) |>
      summarize(rate = mean(value))
  }
}

econ_crime_df = clean_country_names(econ_crime_df, "avg_crime_rate")
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `country = countrycode(...)`.
    ## Caused by warning:
    ## ! Some values were not matched unambiguously: XKX

    ## `summarise()` has grouped output by 'country', 'region'. You can override using
    ## the `.groups` argument.

``` r
justice_personnel_df = clean_country_names(justice_personnel_df, "avg_personnel_rate")
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `country = countrycode(...)`.
    ## Caused by warning:
    ## ! Some values were not matched unambiguously: XKX

    ## `summarise()` has grouped output by 'country', 'region'. You can override using
    ## the `.groups` argument.

``` r
trafficking_df = clean_country_names(trafficking_df, "avg_trafficking_rate")
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `country = countrycode(...)`.
    ## Caused by warning:
    ## ! Some values were not matched unambiguously: CAR, CAS, EEU, MCA, MCN, SAF, SAM

    ## `summarise()` has grouped output by 'country', 'region'. You can override using
    ## the `.groups` argument.

``` r
alcohol_consumption_df = clean_country_names(alcohol_consumption_df, "avg_consumption_rate")
```

    ## `summarise()` has grouped output by 'country'. You can override using the
    ## `.groups` argument.

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
         age == "Total")

violence_rate_df = 
    readxl::read_excel(
    path = "data/unodc/violent_sexual_crime.xlsx",
    skip = 2
  ) |>
  janitor::clean_names() |>
  filter(indicator == "Violent offences",
         unit_of_measurement == "Rate per 100,000 population")

homicide_rate_df = clean_country_names(homicide_rate_df, "avg_homicide_rate")
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `country = countrycode(...)`.
    ## Caused by warning:
    ## ! Some values were not matched unambiguously: XKX

    ## `summarise()` has grouped output by 'country', 'region'. You can override using
    ## the `.groups` argument.

``` r
violence_rate_df = clean_country_names(violence_rate_df, "avg_violence_rate")
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `country = countrycode(...)`.
    ## Caused by warning:
    ## ! Some values were not matched unambiguously: XKX

    ## `summarise()` has grouped output by 'country', 'region'. You can override using
    ## the `.groups` argument.

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
  left_join(human_develop_df) |>
  left_join(econ_crime_df) |>
  left_join(justice_personnel_df) |>
  left_join(trafficking_df) |>
  left_join(alcohol_consumption_df) |>
  mutate(country = as.factor(country),
         region = as.factor(region)) |>
  filter(between(year, 2015, 2023)) |>
  drop_na(country, region)
```

    ## Joining with `by = join_by(country, region, year, rate)`
    ## Joining with `by = join_by(country, year)`
    ## Joining with `by = join_by(country, year)`
    ## Joining with `by = join_by(country, year)`
    ## Joining with `by = join_by(country)`
    ## Joining with `by = join_by(country, region, year, rate)`
    ## Joining with `by = join_by(country, region, year, rate)`
    ## Joining with `by = join_by(country, region, year, rate)`
    ## Joining with `by = join_by(country, year, rate)`

``` r
str(merged_violence_df)
```

    ## gropd_df [1,014 × 15] (S3: grouped_df/tbl_df/tbl/data.frame)
    ##  $ country          : Factor w/ 199 levels "Afghanistan",..: 1 1 1 1 1 1 1 2 2 2 ...
    ##  $ region           : Factor w/ 5 levels "Asia","Europe",..: 1 1 1 1 1 1 1 2 2 2 ...
    ##  $ year             : num [1:1014] 2015 2016 2017 2018 2019 ...
    ##  $ rate             : num [1:1014] 9.98 6.69 6.8 6.74 7.18 ...
    ##  $ gdp              : num [1:1014] 1.91e+10 1.81e+10 1.88e+10 1.81e+10 1.88e+10 ...
    ##  $ inflation_rate   : num [1:1014] -0.662 4.384 4.976 0.626 2.302 ...
    ##  $ unemployment_rate: num [1:1014] NA NA NA NA NA NA NA 17.1 15.2 13.7 ...
    ##  $ 2015             : num [1:1014] NA NA NA NA NA NA NA NA NA NA ...
    ##  $ 2016             : num [1:1014] NA NA NA NA NA NA NA NA NA NA ...
    ##  $ 2017             : num [1:1014] NA NA NA NA NA NA NA NA NA NA ...
    ##  $ 2018             : num [1:1014] NA NA NA NA NA NA NA NA NA NA ...
    ##  $ 2019             : num [1:1014] NA NA NA NA NA NA NA NA NA NA ...
    ##  $ 2020             : num [1:1014] NA NA NA NA NA NA NA NA NA NA ...
    ##  $ 2021             : num [1:1014] NA NA NA NA NA NA NA NA NA NA ...
    ##  $ 2022             : num [1:1014] NA NA NA NA NA NA NA NA NA NA ...
    ##  - attr(*, "groups")= tibble [163 × 3] (S3: tbl_df/tbl/data.frame)
    ##   ..$ country: Factor w/ 199 levels "Afghanistan",..: 1 2 3 4 5 6 8 9 10 12 ...
    ##   ..$ region : Factor w/ 5 levels "Asia","Europe",..: 1 2 3 4 2 3 5 5 1 4 ...
    ##   ..$ .rows  : list<int> [1:163] 
    ##   .. ..$ : int [1:7] 1 2 3 4 5 6 7
    ##   .. ..$ : int [1:8] 8 9 10 11 12 13 14 15
    ##   .. ..$ : int [1:8] 16 17 18 19 20 21 22 23
    ##   .. ..$ : int [1:5] 24 25 26 27 28
    ##   .. ..$ : int [1:2] 29 30
    ##   .. ..$ : int [1:2] 31 32
    ##   .. ..$ : int [1:6] 33 34 35 36 37 38
    ##   .. ..$ : int [1:8] 39 40 41 42 43 44 45 46
    ##   .. ..$ : int [1:7] 47 48 49 50 51 52 53
    ##   .. ..$ : int [1:8] 54 55 56 57 58 59 60 61
    ##   .. ..$ : int [1:8] 62 63 64 65 66 67 68 69
    ##   .. ..$ : int [1:8] 70 71 72 73 74 75 76 77
    ##   .. ..$ : int [1:8] 78 79 80 81 82 83 84 85
    ##   .. ..$ : int [1:8] 86 87 88 89 90 91 92 93
    ##   .. ..$ : int [1:4] 94 95 96 97
    ##   .. ..$ : int [1:8] 98 99 100 101 102 103 104 105
    ##   .. ..$ : int [1:5] 106 107 108 109 110
    ##   .. ..$ : int 111
    ##   .. ..$ : int [1:8] 112 113 114 115 116 117 118 119
    ##   .. ..$ : int [1:9] 120 121 122 123 124 125 126 127 128
    ##   .. ..$ : int [1:6] 129 130 131 132 133 134
    ##   .. ..$ : int [1:9] 135 136 137 138 139 140 141 142 143
    ##   .. ..$ : int [1:8] 144 145 146 147 148 149 150 151
    ##   .. ..$ : int [1:2] 152 153
    ##   .. ..$ : int [1:8] 154 155 156 157 158 159 160 161
    ##   .. ..$ : int [1:8] 162 163 164 165 166 167 168 169
    ##   .. ..$ : int [1:2] 170 171
    ##   .. ..$ : int [1:4] 172 173 174 175
    ##   .. ..$ : int [1:8] 176 177 178 179 180 181 182 183
    ##   .. ..$ : int [1:6] 184 185 186 187 188 189
    ##   .. ..$ : int [1:4] 190 191 192 193
    ##   .. ..$ : int [1:8] 194 195 196 197 198 199 200 201
    ##   .. ..$ : int [1:6] 202 203 204 205 206 207
    ##   .. ..$ : int [1:8] 208 209 210 211 212 213 214 215
    ##   .. ..$ : int [1:9] 216 217 218 219 220 221 222 223 224
    ##   .. ..$ : int [1:8] 225 226 227 228 229 230 231 232
    ##   .. ..$ : int [1:5] 233 234 235 236 237
    ##   .. ..$ : int [1:9] 238 239 240 241 242 243 244 245 246
    ##   .. ..$ : int [1:7] 247 248 249 250 251 252 253
    ##   .. ..$ : int [1:8] 254 255 256 257 258 259 260 261
    ##   .. ..$ : int [1:8] 262 263 264 265 266 267 268 269
    ##   .. ..$ : int [1:7] 270 271 272 273 274 275 276
    ##   .. ..$ : int [1:8] 277 278 279 280 281 282 283 284
    ##   .. ..$ : int [1:3] 285 286 287
    ##   .. ..$ : int [1:8] 288 289 290 291 292 293 294 295
    ##   .. ..$ : int [1:8] 296 297 298 299 300 301 302 303
    ##   .. ..$ : int [1:4] 304 305 306 307
    ##   .. ..$ : int 308
    ##   .. ..$ : int [1:8] 309 310 311 312 313 314 315 316
    ##   .. ..$ : int [1:9] 317 318 319 320 321 322 323 324 325
    ##   .. ..$ : int [1:3] 326 327 328
    ##   .. ..$ : int [1:4] 329 330 331 332
    ##   .. ..$ : int [1:8] 333 334 335 336 337 338 339 340
    ##   .. ..$ : int [1:8] 341 342 343 344 345 346 347 348
    ##   .. ..$ : int [1:8] 349 350 351 352 353 354 355 356
    ##   .. ..$ : int [1:2] 357 358
    ##   .. ..$ : int [1:8] 359 360 361 362 363 364 365 366
    ##   .. ..$ : int [1:2] 367 368
    ##   .. ..$ : int 369
    ##   .. ..$ : int [1:5] 370 371 372 373 374
    ##   .. ..$ : int [1:2] 375 376
    ##   .. ..$ : int [1:8] 377 378 379 380 381 382 383 384
    ##   .. ..$ : int [1:7] 385 386 387 388 389 390 391
    ##   .. ..$ : int [1:8] 392 393 394 395 396 397 398 399
    ##   .. ..$ : int [1:8] 400 401 402 403 404 405 406 407
    ##   .. ..$ : int [1:8] 408 409 410 411 412 413 414 415
    ##   .. ..$ : int [1:8] 416 417 418 419 420 421 422 423
    ##   .. ..$ : int [1:8] 424 425 426 427 428 429 430 431
    ##   .. ..$ : int 432
    ##   .. ..$ : int [1:4] 433 434 435 436
    ##   .. ..$ : int [1:8] 437 438 439 440 441 442 443 444
    ##   .. ..$ : int [1:2] 445 446
    ##   .. ..$ : int [1:8] 447 448 449 450 451 452 453 454
    ##   .. ..$ : int [1:8] 455 456 457 458 459 460 461 462
    ##   .. ..$ : int [1:8] 463 464 465 466 467 468 469 470
    ##   .. ..$ : int [1:8] 471 472 473 474 475 476 477 478
    ##   .. ..$ : int [1:9] 479 480 481 482 483 484 485 486 487
    ##   .. ..$ : int [1:5] 488 489 490 491 492
    ##   .. ..$ : int [1:8] 493 494 495 496 497 498 499 500
    ##   .. ..$ : int [1:4] 501 502 503 504
    ##   .. ..$ : int [1:8] 505 506 507 508 509 510 511 512
    ##   .. ..$ : int [1:6] 513 514 515 516 517 518
    ##   .. ..$ : int [1:7] 519 520 521 522 523 524 525
    ##   .. ..$ : int [1:8] 526 527 528 529 530 531 532 533
    ##   .. ..$ : int [1:8] 534 535 536 537 538 539 540 541
    ##   .. ..$ : int [1:9] 542 543 544 545 546 547 548 549 550
    ##   .. ..$ : int [1:7] 551 552 553 554 555 556 557
    ##   .. ..$ : int [1:2] 558 559
    ##   .. ..$ : int [1:9] 560 561 562 563 564 565 566 567 568
    ##   .. ..$ : int [1:3] 569 570 571
    ##   .. ..$ : int [1:8] 572 573 574 575 576 577 578 579
    ##   .. ..$ : int [1:8] 580 581 582 583 584 585 586 587
    ##   .. ..$ : int 588
    ##   .. ..$ : int [1:9] 589 590 591 592 593 594 595 596 597
    ##   .. ..$ : int [1:8] 598 599 600 601 602 603 604 605
    ##   .. ..$ : int [1:8] 606 607 608 609 610 611 612 613
    ##   .. ..$ : int [1:2] 614 615
    ##   .. ..$ : int [1:9] 616 617 618 619 620 621 622 623 624
    ##   .. ..$ : int [1:8] 625 626 627 628 629 630 631 632
    ##   .. .. [list output truncated]
    ##   .. ..@ ptype: int(0) 
    ##   ..- attr(*, ".drop")= logi TRUE

The final merged dataset includes 1014 rows and 15 columns, including
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
homicide_visual_df = 
    readxl::read_excel(
    path = "data/unodc/intentional_homicide.xlsx",
    skip = 2
  ) |>
  janitor::clean_names() |>
  filter(indicator == "Victims of intentional homicide",
         unit_of_measurement == "Rate per 100,000 population",
         category != "Total",
         sex != "Total",
         between(year, 2015, 2023)) |>
  select(country, region, category, sex, year, homicide_rate = value)
```

``` r
homicide_visual_df |>
  group_by(region, year) |>
  summarize(avg_homicide_rate = mean(homicide_rate)) |>
  ggplot(aes(y = avg_homicide_rate, x = as.factor(year), fill = region)) +
  geom_col(position = "dodge", bin = 3.0) + 
  labs(x = "Year",
       y = "Average Homicide Rate",
       Title = "Average Homicide Rate Across Region")
```

    ## `summarise()` has grouped output by 'region'. You can override using the
    ## `.groups` argument.

    ## Warning in geom_col(position = "dodge", bin = 3): Ignoring unknown parameters:
    ## `bin`

![](p8105_fp_report_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
homicide_visual_df |>
  group_by(region, sex, year) |>
  summarize(avg_homicide_rate = mean(homicide_rate)) |>
  ggplot(aes(y = avg_homicide_rate, x = year, color = sex)) +
  geom_smooth(se = FALSE) +
  facet_grid(~region) + 
  labs(x = "Year",
       y = "Average Homicide Rate",
       Title = "Average Homicide Rate Trend")
```

    ## `summarise()` has grouped output by 'region', 'sex'. You can override using the
    ## `.groups` argument.
    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](p8105_fp_report_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

## Data Transformation

## Regression
