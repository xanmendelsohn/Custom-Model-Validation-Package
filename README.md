Review of Estimates Function Package
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

[## ‘CredRoE’ <img src="man/figures/Coba_Logo.PNG" align="right" width="144" /> ]: #

The ‘CredRoE’ package includes functions to perform model performance
analyses and review of estimates for reg. LGD models.

The model performance considers the aspects: discriminatory power,
accuracy, concentration and model complexity. Excluding model
complexity, the performance criteria are inspected on each relevant
model level. The relevant model levels include:

  - The overall model
  - Sub-models/Segments: relevant if the overall model is split into
    sub-models based on product information or is segmented with respect
    to a risk driver
  - Grade or pool level

The package also includes the calibration/estimation function of the
Retail Construction LGD Model.

## Installation

The package can be installed from a local source file by:

``` r
# package installation ----
remotes::install_local(path = here::here('CredRoE_0.0.0.9000.tar.gz'), dependencies = TRUE)
```

The package repository can also be cloned from bitbucket and installed
by:

``` r
pkg_path <- 'C:/some_directory/CredRoE'

# clone package repository ----
gert::git_clone(url = 'https://bitbucket.intranet.commerzbank.com:8443/scm/rmc/review-of-estimates-functions.git',
                path = pkg_path, verbose = TRUE)

# package installation ----
devtools::install(pkg = pkg_path)

# open package project in a new session ----
usethis::proj_activate(path = pkg_path)
```

## Workflow

This is a basic workflow example of the core functionalities:

``` r
library("CredRoE")
library("tidyverse")
```

# Discriminatory Power

The discriminatory power is measured by the Somers‘ D statistic (or
comparable measures, e.g. Kendall’s Tau, Spearman’s Correlation
Coefficient) with a bootstrap confidence interval. There is no absolute
bench-mark for the Somers’ D statistic which has to be achieved. There
is simply a preference for models with higher discriminatory power,
i.e. higher Somers’ D.

In addition, a graphical analysis in the form of Lorentz curves or the
cumulative accuracy profile can be made. The objective of the graphical
analysis is to investigate whether the discriminatory power is
concentrated in a part of the model’s range of realized values or
whether the model is discriminate over the full range of values.

``` r
#calculate Somers' D and Kendall's Tau
disc.power <- calc_somersd_stats(x = df_lgd %>% dplyr::pull(REAL_LGD), y= df_lgd %>% dplyr::pull(LGD_PREDICTION))

disc.power$SUMMARY_TAU_B
disc.power$SUMMARY_SOMERSD

#Plot CAP curve with title
CAP_Curve(df_lgd, LGD_PREDICTION, REAL_LGD, title="-- Meaningless Plot")
```

# Heterogeneity

For grade or pool models the heterogeneity between grades and pools is
also assessed. Model grades/pools exhibit high heterogeneity if the
realized LGDs of the grades/pools are significantly differ from each
other in terms of the average realized LGD. To this end a post-hoc
pairwise Wilcoxon-Test or a pairwise Welch-Test is performed. All
neighboring model grades/pools are compared in pairs.

For each neighboring model grade/pool pair:

  - H0: The mean ranks of the realized LGDs are the same between the two
    grades.
  - We can reject the hypothesis in case of a p-value below 5%

The paired samples Wilcoxon-Test (also known as Wilcoxon signed-rank
test) is a non-parametric alternative to the paired Welch-Test used to
compare paired data. It’s used when your data are not normally
distributed.

``` r
#Perform Welch Test for grades/pools (here LTV buckets)
matrix.welch <- calc_pairwise_welchtest(data_df=df_lgd,lgd_pred_var=LGD_PREDICTION,lgd_real_var=REAL_LGD,grade_var=LTV_BIN)

matrix.welch

#Perform Wilcoxon Signed Rank Test for grades/pools (here LTV buckets)
matrix.wilcox <- calc_pairwise_wilcox(data_df=df_lgd, lgd_pred_var=LGD_PREDICTION, lgd_real_var=REAL_LGD, grade_var=LTV_BIN)

matrix.wilcox
```

# Predictive Power

The predictive power of the estimated LGDs is assessed using T-Tests.
Additionally, a graph is provided to compare the confidence intervals of
the predicted LGDs with those of the realized LGDs. Testing is performed
using a paired Welch T-Test on bucket and portfolio level. The
significance level used is 5%.

The T-Tests are performed on two levels:

  - the realized LGD and predicted LGDs are compared as is
  - the realized LGD and predicted LGDs are compared with the predicted
    LGDs shifted by a certain tolerance level. The tolerance level used
    is 2.5%.

<!-- end list -->

``` r
#Perform T-Tests
pred.power <- calc_predictive_power_backtest(df_lgd,lgd_pred_var=LGD_PREDICTION,lgd_real_var=REAL_LGD)

pred.power
```

# Homogeneity and Concentration

To further investigate the stability and adequacy of the grade/pool
structure, two aspects are considered:

  - Homogeneity: observations should show a homogeneously distribution
    of realized LGDs within each grade/pool. The standard deviation and
    quantiles are determined to assess the homogeneity within a model
    grade/pool. Graphical assessment of homogeneity is also performed
    with violin plots and boxplots.
  - Concentration: observations should not be overly concentrated in a
    few grades/pools. Ideally, all grades/pools are populated to a
    similar degree. The level of concentration is investigated with the
    normalized Herfindahl Index (H). A threshold value for the
    normalized Herfindahl Index is included in the review of estimates
    framework.

<!-- end list -->

``` r
#Perform T-Tests
conhom <- calc_concentation_homogeneity(data_df=df_lgd,lgd_pred_var=LGD_PREDICTION, lgd_real_var=REAL_LGD,grade_var=LTV_BIN, alpha = 0.1)

conhom[["SUMMARY_TABLE"]]
conhom[["HERFINDAHL_INDEX"]]
conhom[["BOXPLOT"]]
conhom[["VIOLIN_PLOT"]]
conhom[["HISTOGRAM"]]
```

# Functions for HTML Presentations

Included are functions to format result tables in HTML markdowns. There
is a separate function for:

  - Predictive Power backtesting results
    (‘fct\_custom\_color\_tile\_backtesting’)
  - Matrix containing percentages (‘fct\_format\_perc\_matrix’)
  - Tables (‘fct\_table\_format’)

<!-- end list -->

``` r
pred.power %>% fct_custom_color_tile_backtesting()

matrix.welch %>% fct_format_perc_matrix()

conhom[["SUMMARY_TABLE"]] %>% fct_format_perc_matrix(title = "Distribution Across Grades")
```

# Retail Construction Calibration/Estimation Function

Included are the Retail Construction specific functions:

  - ‘fct\_rc\_grades’ - to perform missing treatment on risk drivers and
    create factor variables defining buckets and grades
  - ‘fct\_rc\_estimate’ - to estimate/calibrate given the grades and
    realized LGD

<!-- end list -->

``` r
calibration.sample %>% 
  select(DEFAULT_ID, FACILITY_ID, COLL_QUOTA, SEASON_REL, LGD_REAL, EAD_RDS) %>%
  #perform missing treatment; define buckets and grades 
  fct_rc_grades() %>%
  #aggregate to obligor x grade level
  group_by_at(vars(c("DEFAULT_ID", "GRADES"))) %>%
  mutate(LGD_REAL = weighted.mean(LGD_REAL, EAD_RDS, na.rm =T)) %>%
  ungroup() %>%
  #calibrate
  fct_rc_estimate()
```
