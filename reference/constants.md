# Package constants

Constant values used in *vimcheck*. See the **Examples** section for the
constant values.

## Usage

``` r
file_dict_colnames

scenario_data_colnames

burden_outcome_names

colnames_plot_demog_compare

colnames_df_missing_cols

COLNAMES_KEY_PRESSURE_TEST

COLNAMES_INTEREST_PRESSURE_TEST

IMPACT_OUTCOMES

EXCLUDED_DISEASES

N_TS_MIN_CHARS

N_TS_YEAR_CHARS

MIN_TS_YEAR

MAX_TS_YEAR

MIN_TS_MONTH

MAX_TS_MONTH

DEF_TOUCHSTONE_OLD

DEF_TOUCHSTONE_NEW

DEF_TOUCHSTONE_OLD_OLD

COLOUR_VIMC

PINE
```

## Format

An object of class `character` of length 5.

An object of class `character` of length 4.

An object of class `character` of length 10.

An object of class `character` of length 7.

An object of class `character` of length 5.

An object of class `character` of length 7.

An object of class `character` of length 14.

An object of class `character` of length 2.

An object of class `character` of length 4.

An object of class `integer` of length 1.

An object of class `integer` of length 1.

An object of class `numeric` of length 1.

An object of class `numeric` of length 1.

An object of class `numeric` of length 1.

An object of class `numeric` of length 1.

An object of class `character` of length 1.

An object of class `character` of length 1.

An object of class `character` of length 1.

An object of class `character` of length 1.

An object of class `character` of length 4.

## Examples

``` r
file_dict_colnames
#> [1] "scenario_type"             "scenario_type_description"
#> [3] "scenario"                  "scenario_description"     
#> [5] "file"                     

scenario_data_colnames
#> [1] "scenario_type"             "scenario_type_description"
#> [3] "scenario"                  "scenario_description"     

burden_outcome_names
#>  [1] "cases"                     "deaths"                   
#>  [3] "dalys"                     "yll"                      
#>  [5] "deaths_cwyx"               "cases_cwyx"               
#>  [7] "dalys_cwyx"                "yll_cwyx"                 
#>  [9] "rubella_deaths_congenital" "rubella_cases_congenital" 

colnames_plot_demog_compare
#> [1] "variable"       "scenario"       "year"           "age"           
#> [5] "country"        "value"          "value_millions"

colnames_df_missing_cols
#> [1] "country_name"    "vaccine"         "activity_type"   "year"           
#> [5] "modelling_group"

COLNAMES_KEY_PRESSURE_TEST
#> [1] "country"         "country_name"    "vaccine"         "activity_type"  
#> [5] "year"            "disease"         "modelling_group"

COLNAMES_INTEREST_PRESSURE_TEST
#>  [1] "country"             "country_name"        "vaccine"            
#>  [4] "activity_type"       "year"                "disease"            
#>  [7] "modelling_group"     "fvps"                "target_population"  
#> [10] "coverage"            "deaths_averted"      "dalys_averted"      
#> [13] "deaths_averted_rate" "dalys_averted_rate" 

IMPACT_OUTCOMES
#> [1] "deaths_averted" "dalys_averted" 

EXCLUDED_DISEASES
#> [1] "Hib"  "PCV"  "Rota" "JE"  

N_TS_MIN_CHARS
#> [1] 6

N_TS_YEAR_CHARS
#> [1] 4

MIN_TS_YEAR
#> [1] 2000

MAX_TS_YEAR
#> [1] 2100

MIN_TS_MONTH
#> [1] 1

MAX_TS_MONTH
#> [1] 12

DEF_TOUCHSTONE_OLD
#> [1] "201910"

DEF_TOUCHSTONE_NEW
#> [1] "202310"

DEF_TOUCHSTONE_OLD_OLD
#> [1] "202110"

COLOUR_VIMC
#> [1] "#008080"

PINE
#> [1] "PAK" "IND" "NGA" "ETH"
```
