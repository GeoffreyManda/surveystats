# surveystats

`surveystats` builds survey designs and calculates weighted estimates from an indicator register using base R.

## Design inputs

Each data row represents an observation. Create a design by naming the columns that hold case weights, strata and primary sampling units. PSU identifiers are read within strata, so a PSU label may recur in different strata. Set `weight = NULL` for equal weights, `strata = NULL` for one stratum, and `psu = NULL` when each row is a separate sampling unit.

The design uses a first-stage, with-replacement variance estimator. It has no finite population correction. When a stratum contains one PSU, standard errors and confidence intervals are returned as missing for estimates that depend on that stratum.

## Register fields

Each register row defines one indicator.

| Column | Meaning |
|---|---|
| `indicator_id` | Unique identifier retained in every result row. |
| `indicator_label` | Display label returned in `Question` and `indicator_label`. |
| `variable` | Data column analysed. For select-multiple binary columns, use the question stem. |
| `kobo_type` | `select_one`, `select_multiple`, or `integer`. |
| `aggregation_method` | Integer summary: `mean`, `median`, `sum`, `firstq`, `thirdq`, `min`, or `max`. Set to `NA` for choice indicators. |
| `disaggregation` | A data column, `all`, or `NA`. |

Optional columns are `disaggregation_label`, `repeat_for`, `group`, `unit`, `numerator`, and `denominator`. The last three are returned as indicator metadata.

A select-one indicator produces a weighted proportion for each observed response. A select-multiple indicator accepts a delimited text column or binary columns named `variable__option`. In binary columns, missing option cells count as unselected when another option cell is observed for that respondent. Respondents with no recorded option cells are omitted from the denominator. Integer summaries use case weights; weighted quantiles use the first value at which cumulative weight reaches the requested probability.

Standard errors for proportions, means and totals use first-order Taylor linearization. PSU contributions are centred within strata. Confidence intervals use a normal 95% interval, with proportion limits bounded between zero and one. Quantiles and extrema have point estimates without standard errors or confidence intervals. Design effects compare the estimated variance with an unweighted simple-random-sample variance.

## Example

```r
survey_data <- data.frame(
  weight = rep(1, 8),
  stratum = rep(c("north", "south"), each = 4),
  cluster = rep(c(1, 1, 2, 2), 2),
  sex = c("female", "male", "female", "male", "female", "female", "male", "male"),
  age = c(2, 3, 4, 5, 2, 4, 6, 8)
)

design <- surveystats::make_survey_design(
  survey_data,
  weight = "weight",
  strata = "stratum",
  psu = "cluster"
)

register <- data.frame(
  indicator_id = c("female", "age_mean"),
  indicator_label = c("Female respondents", "Mean age"),
  variable = c("sex", "age"),
  kobo_type = c("select_one", "integer"),
  aggregation_method = c(NA_character_, "mean"),
  disaggregation = c("all", "all"),
  unit = c("proportion", "years")
)

results <- surveystats::run_indicator_register(
  design,
  register,
  result_format = "percent"
)
```

## Licence

The package is released under GPL-3. See `LICENSE` for the complete terms.
