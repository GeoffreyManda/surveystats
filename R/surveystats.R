# surveystats: indicator-register survey analysis using base R
# Copyright (C) 2026 Geoffrey Manda
# This program is free software under the GNU General Public License
# version 3. See LICENSE for the full license text.

.required_register_columns <- c(
  "indicator_id", "indicator_label", "variable", "kobo_type",
  "aggregation_method", "disaggregation"
)

#' Create a stratified cluster survey design
#'
#' Stores the analysis data with case weights, strata and primary sampling
#' units. PSU identifiers are interpreted within strata. When `weight` is
#' `NULL`, observations receive equal weights. When `psu` is `NULL`, each row
#' is treated as its own primary sampling unit.
#'
#' @param data A non-empty data frame containing the analysis variables.
#' @param weight A single column name containing finite, non-negative numeric
#'   case weights, or `NULL` for equal weights.
#' @param strata A single column name identifying sampling strata, or `NULL`
#'   for one stratum.
#' @param psu A single column name identifying primary sampling units, or
#'   `NULL` to treat each observation as a primary sampling unit.
#'
#' @return An object of class `surveystats_design` for use with
#'   [run_indicator_register()].
#' @export
make_survey_design <- function(data, weight = NULL, strata = NULL, psu = NULL) {
  if (!is.data.frame(data) || nrow(data) < 1L) {
    stop("`data` must be a non-empty data frame.", call. = FALSE)
  }
  if (anyDuplicated(names(data))) {
    stop("`data` must have unique column names.", call. = FALSE)
  }
  .check_column_name(weight, "weight", data, optional = TRUE)
  .check_column_name(strata, "strata", data, optional = TRUE)
  .check_column_name(psu, "psu", data, optional = TRUE)

  weights <- if (is.null(weight)) {
    rep(1, nrow(data))
  } else {
    data[[weight]]
  }
  if (!is.numeric(weights) || anyNA(weights) || any(!is.finite(weights)) ||
      any(weights < 0) || !is.finite(sum(weights)) || sum(weights) <= 0) {
    stop("Weights must be finite, non-negative numeric values with a positive sum.",
         call. = FALSE)
  }
  strata_id <- if (is.null(strata)) rep("all", nrow(data)) else
    as.character(data[[strata]])
  psu_id <- if (is.null(psu)) as.character(seq_len(nrow(data))) else
    as.character(data[[psu]])
  if (anyNA(strata_id) || anyNA(psu_id)) {
    stop("Strata and PSU identifiers must not be missing.", call. = FALSE)
  }
  structure(
    list(data = data, weights = as.numeric(weights), strata = strata_id,
         psu = psu_id, weight_column = weight, strata_column = strata,
         psu_column = psu),
    class = "surveystats_design"
  )
}

.check_column_name <- function(value, argument, data, optional = FALSE) {
  if (is.null(value) && optional) return(invisible(NULL))
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !nzchar(value)) {
    stop("`", argument, "` must be one column name", if (optional) " or NULL" else "",
         ".", call. = FALSE)
  }
  if (!value %in% names(data)) {
    stop("Column `", value, "` supplied for `", argument, "` is absent from `data`.",
         call. = FALSE)
  }
  invisible(NULL)
}

.validate_register <- function(register, data) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  if (anyDuplicated(names(data))) {
    stop("`data` must have unique column names.", call. = FALSE)
  }
  if (!is.data.frame(register)) {
    stop("`register` must be a data frame.", call. = FALSE)
  }
  if (anyDuplicated(names(register))) {
    stop("`register` must have unique column names.", call. = FALSE)
  }
  missing_columns <- setdiff(.required_register_columns, names(register))
  if (length(missing_columns)) {
    stop("`register` is missing required columns: ",
         paste(missing_columns, collapse = ", "), call. = FALSE)
  }
  if (nrow(register) == 0L) {
    stop("`register` must contain at least one indicator row.", call. = FALSE)
  }

  ids <- as.character(register$indicator_id)
  labels <- as.character(register$indicator_label)
  variables <- as.character(register$variable)
  types <- as.character(register$kobo_type)
  methods <- as.character(register$aggregation_method)
  if (anyNA(ids) || any(!nzchar(trimws(ids))) || anyDuplicated(ids)) {
    stop("`indicator_id` values must be present, non-empty and unique.", call. = FALSE)
  }
  if (anyNA(labels) || any(!nzchar(trimws(labels)))) {
    stop("`indicator_label` values must be present and non-empty.", call. = FALSE)
  }
  if (anyNA(variables) || any(!nzchar(trimws(variables)))) {
    stop("`variable` values must be present and non-empty.", call. = FALSE)
  }
  if (anyNA(types) || any(!types %in% c("select_one", "select_multiple", "integer"))) {
    stop("`kobo_type` must be `select_one`, `select_multiple`, or `integer`.",
         call. = FALSE)
  }
  integer_methods <- c("mean", "median", "sum", "firstq", "thirdq", "min", "max")
  integer_rows <- types == "integer"
  if (any(integer_rows & (is.na(methods) | !methods %in% integer_methods))) {
    stop("Integer indicators need a supported `aggregation_method`.", call. = FALSE)
  }
  if (any(!integer_rows & !is.na(methods) & nzchar(methods))) {
    stop("Choice indicators must have a missing `aggregation_method`.", call. = FALSE)
  }

  for (column in unique(variables)) {
    rows <- which(variables == column)
    required_types <- unique(types[rows])
    if ("integer" %in% required_types || "select_one" %in% required_types) {
      if (!column %in% names(data)) {
        stop("Analysis variable `", column, "` is absent from `data`.", call. = FALSE)
      }
    }
    if ("select_multiple" %in% required_types) {
      source_is_text <- column %in% names(data) &&
        (is.character(data[[column]]) || is.factor(data[[column]]))
      option_columns <- names(data)[startsWith(names(data), paste0(column, "__"))]
      if (!source_is_text && !length(option_columns)) {
        stop("Select-multiple variable `", column,
             "` needs delimited text or `variable__option` columns.",
             call. = FALSE)
      }
    }
  }
  for (column in c(as.character(register$disaggregation),
                   if ("repeat_for" %in% names(register)) as.character(register$repeat_for))) {
    if (!is.na(column) && nzchar(column) && column != "all" &&
        !column %in% names(data)) {
      stop("Grouping column `", column, "` is absent from `data`.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' Validate an indicator register against analysis data
#'
#' Checks indicator identifiers, required fields, analysis types, supported
#' summaries and referenced data columns.
#'
#' @param register A data frame with one row per indicator. Required columns
#'   are `indicator_id`, `indicator_label`, `variable`, `kobo_type`,
#'   `aggregation_method`, and `disaggregation`. Optional columns are
#'   `disaggregation_label`, `repeat_for`, `group`, `unit`, `numerator`, and
#'   `denominator`.
#' @param data A data frame containing the variables referenced by the register.
#'
#' @return `TRUE`, invisibly, when validation succeeds.
#' @export
validate_indicator_register <- function(register, data) {
  .validate_register(register, data)
  invisible(TRUE)
}

.domain_groups <- function(data, column) {
  if (is.na(column) || !nzchar(column) || column == "all") {
    return(list(list(value = "all", label = "all", mask = rep(TRUE, nrow(data)))))
  }
  values <- as.character(data[[column]])
  levels <- unique(values)
  lapply(levels, function(value) {
    mask <- if (is.na(value)) {
      is.na(values)
    } else {
      !is.na(values) & values == value
    }
    list(value = value, label = if (is.na(value)) NA_character_ else as.character(value),
         mask = mask)
  })
}

.taylor_variance <- function(design, linearized) {
  total <- 0
  strata_values <- unique(design$strata)
  for (stratum in strata_values) {
    in_stratum <- design$strata == stratum
    psus <- unique(design$psu[in_stratum])
    n_psu <- length(psus)
    if (n_psu < 2L) return(NA_real_)
    totals <- numeric(n_psu)
    for (j in seq_len(n_psu)) {
      in_psu <- in_stratum & design$psu == psus[j]
      totals[j] <- sum(linearized[in_psu])
    }
    center <- sum(totals) / n_psu
    total <- total + (n_psu / (n_psu - 1)) * sum((totals - center)^2)
  }
  total
}

.ratio_estimate <- function(design, eligible, values) {
  denominator <- sum(design$weights[eligible])
  if (denominator <= 0) return(c(estimate = NA_real_, se = NA_real_))
  estimate <- sum(design$weights[eligible] * values[eligible]) / denominator
  linearized <- numeric(length(eligible))
  linearized[eligible] <- design$weights[eligible] *
    (values[eligible] - estimate) / denominator
  variance <- .taylor_variance(design, linearized)
  c(estimate = estimate,
    se = if (is.na(variance) || variance < 0) NA_real_ else sqrt(variance))
}

.weighted_quantile <- function(values, weights, probability) {
  keep <- !is.na(values) & weights > 0
  values <- as.numeric(values[keep])
  weights <- weights[keep]
  if (!length(values) || sum(weights) <= 0) return(NA_real_)
  order_index <- order(values)
  values <- values[order_index]
  weights <- weights[order_index]
  target <- probability * sum(weights)
  index <- which(cumsum(weights) >= target)[1L]
  values[index]
}

.normal_interval <- function(estimate, se, proportion = FALSE) {
  if (is.na(estimate) || is.na(se)) return(c(low = NA_real_, high = NA_real_))
  low <- estimate - 1.959963984540054 * se
  high <- estimate + 1.959963984540054 * se
  if (proportion) {
    low <- max(0, low)
    high <- min(1, high)
  }
  c(low = low, high = high)
}

.binary_values <- function(column, name) {
  if (is.logical(column)) return(as.numeric(column))
  if (is.numeric(column)) {
    observed <- column[!is.na(column)]
    if (any(!observed %in% c(0, 1))) {
      stop("Select-multiple column `", name, "` must contain only 0, 1, or missing values.",
           call. = FALSE)
    }
    return(as.numeric(column))
  }
  text <- tolower(trimws(as.character(column)))
  out <- rep(NA_real_, length(text))
  out[text %in% c("1", "true", "yes", "selected")] <- 1
  out[text %in% c("0", "false", "no", "")] <- 0
  bad <- !is.na(text) & is.na(out)
  if (any(bad)) {
    stop("Select-multiple column `", name,
         "` must contain binary values or missing values.", call. = FALSE)
  }
  out
}

.multiple_options <- function(data, variable, separator) {
  if (variable %in% names(data) &&
      (is.character(data[[variable]]) || is.factor(data[[variable]]))) {
    source <- data[[variable]]
    if (!is.character(source) && !is.factor(source)) {
      stop("Select-multiple variable `", variable,
           "` must be delimited text or have sibling binary columns.", call. = FALSE)
    }
    text <- as.character(source)
    responses <- lapply(text, function(value) {
      if (is.na(value) || !nzchar(trimws(value))) return(character())
      parts <- trimws(strsplit(value, separator, fixed = TRUE)[[1L]])
      parts[nzchar(parts)]
    })
    options <- unique(unlist(responses, use.names = FALSE))
    if (!length(options)) {
      stop("Select-multiple variable `", variable,
           "` has no observed response options.", call. = FALSE)
    }
    selected <- matrix(FALSE, nrow = nrow(data), ncol = length(options))
    if (length(options)) {
      for (j in seq_along(options)) {
        selected[, j] <- vapply(responses, function(value) options[j] %in% value,
                                logical(1))
      }
    }
    return(list(options = options, selected = selected,
                valid = !is.na(text) & nzchar(trimws(text))))
  }

  prefix <- paste0(variable, "__")
  columns <- names(data)[startsWith(names(data), prefix)]
  if (!length(columns)) {
    stop("Select-multiple variable `", variable,
         "` has no delimited response column or `variable__option` columns.",
         call. = FALSE)
  }
  matrix_values <- matrix(NA_real_, nrow = nrow(data), ncol = length(columns))
  for (j in seq_along(columns)) {
    matrix_values[, j] <- .binary_values(data[[columns[j]]], columns[j])
  }
  valid <- rowSums(!is.na(matrix_values)) > 0L
  matrix_values[is.na(matrix_values)] <- 0
  list(options = substring(columns, nchar(prefix) + 1L),
       selected = matrix_values == 1, valid = valid)
}

.summary_value <- function(method, values, weights) {
  if (!length(values) || sum(weights) <= 0) return(NA_real_)
  if (method == "sum") return(sum(values * weights))
  if (method == "mean") return(sum(values * weights) / sum(weights))
  probability <- switch(method, median = 0.5, firstq = 0.25,
                        thirdq = 0.75, min = 0, max = 1)
  .weighted_quantile(values, weights, probability)
}

.sample_variance <- function(values) {
  n <- length(values)
  if (n < 2L) return(NA_real_)
  center <- sum(values) / n
  sum((values - center)^2) / (n - 1)
}

.make_result_row <- function(id, label, unit, numerator, denominator_definition,
                             question, response, method, estimate, se, interval,
                             count, denominator, disaggregation, disaggregation_level,
                             repeat_value, group, deff, n_eff) {
  data.frame(
    indicator_id = id,
    indicator_label = label,
    indicator_unit = unit,
    numerator_definition = numerator,
    denominator_definition = denominator_definition,
    Question = question,
    Response = response,
    Aggregation_method = method,
    Result = estimate,
    SE = se,
    CI_low = interval[1L],
    CI_high = interval[2L],
    Count = count,
    Denominator = denominator,
    Disaggregation = disaggregation,
    Disaggregation_level = disaggregation_level,
    repeat_for = repeat_value,
    Group = group,
    DEFF = deff,
    n_eff = n_eff,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

#' Analyse a survey using an indicator register
#'
#' Computes weighted estimates from a [make_survey_design()] object. Choice
#' indicators return a row for each response category. Integer indicators
#' return the requested summary. Register disaggregation and `repeat_for`
#' columns divide estimates into domains.
#'
#' @param design A `surveystats_design` object.
#' @param register A validated indicator register. Optional metadata columns
#'   `unit`, `numerator`, and `denominator` are copied to each result row.
#' @param data The same data frame used to create `design`; defaults to the
#'   design data.
#' @param result_format Either `proportion` (0 to 1) or `percent` (0 to 100)
#'   for choice indicators.
#' @param digits Number of decimal places used when `result_format` is
#'   `percent`.
#' @param multi_response_sep Separator used for delimited select-multiple
#'   response columns.
#' @param deff If `TRUE`, add design-effect and effective-sample-size columns.
#' @param ... Reserved for future arguments. Supplying values raises an error.
#'
#' @return A data frame with one row per indicator response or numeric summary
#'   and domain. Proportion, mean and total standard errors use stratified PSU
#'   Taylor linearization. Their intervals are normal 95% intervals. Weighted
#'   quantiles and extrema have point estimates only. A singleton-PSU stratum
#'   yields missing standard errors and intervals.
#' @export
run_indicator_register <- function(design, register, data = design$data,
                                   result_format = "proportion", digits = 1,
                                   multi_response_sep = ";", deff = FALSE, ...) {
  if (length(list(...))) {
    stop("Additional arguments are not supported.", call. = FALSE)
  }
  if (!inherits(design, "surveystats_design")) {
    stop("`design` must be created by `make_survey_design()`.", call. = FALSE)
  }
  if (!identical(data, design$data)) {
    stop("`data` must match the data stored in `design`.", call. = FALSE)
  }
  validate_indicator_register(register, data)
  if (length(result_format) != 1L || is.na(result_format) ||
      !result_format %in% c("proportion", "percent")) {
    stop("`result_format` must be `proportion` or `percent`.", call. = FALSE)
  }
  if (!is.numeric(digits) || length(digits) != 1L || is.na(digits) ||
      !is.finite(digits) || digits < 0 || digits != as.integer(digits)) {
    stop("`digits` must be a non-negative integer.", call. = FALSE)
  }
  if (!is.character(multi_response_sep) || length(multi_response_sep) != 1L ||
      is.na(multi_response_sep) || !nzchar(multi_response_sep)) {
    stop("`multi_response_sep` must be one non-empty string.", call. = FALSE)
  }
  if (!is.logical(deff) || length(deff) != 1L || is.na(deff)) {
    stop("`deff` must be TRUE or FALSE.", call. = FALSE)
  }

  result_rows <- list()
  next_row <- 0L
  for (i in seq_len(nrow(register))) {
    indicator <- register[i, , drop = FALSE]
    variable <- as.character(indicator$variable)
    type <- as.character(indicator$kobo_type)
    method <- as.character(indicator$aggregation_method)
    disaggregation <- as.character(indicator$disaggregation)
    if (is.na(disaggregation)) disaggregation <- "all"
    repeat_column <- if ("repeat_for" %in% names(indicator))
      as.character(indicator$repeat_for) else NA_character_
    repeat_groups <- .domain_groups(data, repeat_column)
    disaggregation_groups <- .domain_groups(data, disaggregation)

    multiple <- if (type == "select_multiple")
      .multiple_options(data, variable, multi_response_sep) else NULL
    response_levels <- if (type == "select_one")
      unique(as.character(data[[variable]][!is.na(data[[variable]])])) else NULL
    if (type == "select_one" && !length(response_levels)) {
      stop("Select-one variable `", variable,
           "` has no observed response categories.", call. = FALSE)
    }
    if (type == "integer" && !is.numeric(data[[variable]])) {
      stop("Integer indicator `", variable, "` must reference a numeric column.",
           call. = FALSE)
    }
    if (type == "integer" && any(!is.finite(data[[variable]][!is.na(data[[variable]])]))) {
      stop("Integer indicator `", variable,
           "` must contain finite values or missing values.", call. = FALSE)
    }

    for (repeat_group in repeat_groups) {
      for (domain in disaggregation_groups) {
        in_domain <- repeat_group$mask & domain$mask
        if (type == "select_one") {
          values <- as.character(data[[variable]])
          valid <- in_domain & !is.na(values)
          for (response in response_levels) {
            selected <- !is.na(values) & values == response
            estimate <- .ratio_estimate(design, valid, as.numeric(selected))
            scale <- if (result_format == "percent") 100 else 1
            result <- estimate["estimate"] * scale
            se <- estimate["se"] * scale
            interval <- .normal_interval(estimate["estimate"], estimate["se"], TRUE) * scale
            if (result_format == "percent") {
              result <- round(result, digits)
              se <- round(se, digits)
              interval <- round(interval, digits)
            }
            deff_value <- NA_real_
            effective_n <- NA_real_
            if (deff && !is.na(estimate["se"])) {
              srs <- .sample_variance(as.numeric(selected[valid])) / sum(valid)
              deff_value <- if (!is.na(srs) && srs > 0) estimate["se"]^2 / srs else NA_real_
              effective_n <- if (!is.na(deff_value) && deff_value > 0)
                sum(valid) / deff_value else NA_real_
            }
            next_row <- next_row + 1L
            result_rows[[next_row]] <- .make_result_row(
              as.character(indicator$indicator_id), as.character(indicator$indicator_label),
              if ("unit" %in% names(indicator)) as.character(indicator$unit) else NA_character_,
              if ("numerator" %in% names(indicator)) as.character(indicator$numerator) else NA_character_,
              if ("denominator" %in% names(indicator)) as.character(indicator$denominator) else NA_character_,
              as.character(indicator$indicator_label), response, "proportion", result, se,
              interval, sum(valid & selected), sum(valid),
              if (disaggregation == "all") "all" else if ("disaggregation_label" %in% names(indicator) &&
                !is.na(indicator$disaggregation_label)) as.character(indicator$disaggregation_label) else disaggregation,
              domain$label, repeat_group$label, if ("group" %in% names(indicator)) as.character(indicator$group) else NA_character_,
              deff_value, effective_n
            )
          }
        } else if (type == "select_multiple") {
          valid <- in_domain & multiple$valid
          for (j in seq_along(multiple$options)) {
            selected <- multiple$selected[, j]
            estimate <- .ratio_estimate(design, valid, as.numeric(selected))
            scale <- if (result_format == "percent") 100 else 1
            result <- estimate["estimate"] * scale
            se <- estimate["se"] * scale
            interval <- .normal_interval(estimate["estimate"], estimate["se"], TRUE) * scale
            if (result_format == "percent") {
              result <- round(result, digits)
              se <- round(se, digits)
              interval <- round(interval, digits)
            }
            deff_value <- NA_real_
            effective_n <- NA_real_
            if (deff && !is.na(estimate["se"])) {
              srs <- .sample_variance(as.numeric(selected[valid])) / sum(valid)
              deff_value <- if (!is.na(srs) && srs > 0) estimate["se"]^2 / srs else NA_real_
              effective_n <- if (!is.na(deff_value) && deff_value > 0)
                sum(valid) / deff_value else NA_real_
            }
            next_row <- next_row + 1L
            result_rows[[next_row]] <- .make_result_row(
              as.character(indicator$indicator_id), as.character(indicator$indicator_label),
              if ("unit" %in% names(indicator)) as.character(indicator$unit) else NA_character_,
              if ("numerator" %in% names(indicator)) as.character(indicator$numerator) else NA_character_,
              if ("denominator" %in% names(indicator)) as.character(indicator$denominator) else NA_character_,
              as.character(indicator$indicator_label), multiple$options[j], "proportion", result, se,
              interval, sum(valid & selected), sum(valid),
              if (disaggregation == "all") "all" else if ("disaggregation_label" %in% names(indicator) &&
                !is.na(indicator$disaggregation_label)) as.character(indicator$disaggregation_label) else disaggregation,
              domain$label, repeat_group$label, if ("group" %in% names(indicator)) as.character(indicator$group) else NA_character_,
              deff_value, effective_n
            )
          }
        } else {
          values <- data[[variable]]
          valid <- in_domain & !is.na(values)
          observed <- as.numeric(values[valid])
          observed_weights <- design$weights[valid]
          value <- .summary_value(method, observed, observed_weights)
          se <- NA_real_
          interval <- c(low = NA_real_, high = NA_real_)
          deff_value <- NA_real_
          effective_n <- NA_real_
          if (method == "mean" && length(observed) && sum(observed_weights) > 0) {
            estimate <- .ratio_estimate(design, valid, as.numeric(values))
            value <- estimate["estimate"]
            se <- estimate["se"]
            interval <- .normal_interval(value, se)
          } else if (method == "sum" && length(observed) && sum(observed_weights) > 0) {
            linearized <- numeric(nrow(data))
            linearized[valid] <- observed_weights * observed
            variance <- .taylor_variance(design, linearized)
            se <- if (is.na(variance) || variance < 0) NA_real_ else sqrt(variance)
            interval <- .normal_interval(value, se)
          }
          if (deff && length(observed) > 1L && !is.na(se)) {
            sample_var <- .sample_variance(observed)
            srs <- if (method == "mean") sample_var / length(observed) else
              if (method == "sum") sample_var * length(observed) else NA_real_
            deff_value <- if (!is.na(srs) && srs > 0) se^2 / srs else NA_real_
            effective_n <- if (!is.na(deff_value) && deff_value > 0)
              length(observed) / deff_value else NA_real_
          }
          next_row <- next_row + 1L
          result_rows[[next_row]] <- .make_result_row(
            as.character(indicator$indicator_id), as.character(indicator$indicator_label),
            if ("unit" %in% names(indicator)) as.character(indicator$unit) else NA_character_,
            if ("numerator" %in% names(indicator)) as.character(indicator$numerator) else NA_character_,
            if ("denominator" %in% names(indicator)) as.character(indicator$denominator) else NA_character_,
            as.character(indicator$indicator_label), NA_character_, method, value, se,
            interval, sum(valid), sum(valid),
            if (disaggregation == "all") "all" else if ("disaggregation_label" %in% names(indicator) &&
              !is.na(indicator$disaggregation_label)) as.character(indicator$disaggregation_label) else disaggregation,
            domain$label, repeat_group$label, if ("group" %in% names(indicator)) as.character(indicator$group) else NA_character_,
            deff_value, effective_n
          )
        }
      }
    }
  }
  if (!length(result_rows)) {
    return(data.frame())
  }
  do.call(rbind, result_rows)
}
