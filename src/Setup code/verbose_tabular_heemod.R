## Modifications to heemod code to keep individual runs, if necessary.

## Currently this file is not needed, but is kept in case we need to analyse
## all the population groupings.

library(heemod)
library(tidyverse)

run_model_tabular_verbose <-
  function(location, reference = "REFERENCE.csv", run_dsa = TRUE,
             run_psa = TRUE, run_demo = TRUE, save = FALSE, overwrite = FALSE) {
    inputs <- heemod:::gather_model_info(location, reference)

    outputs <- eval_models_from_tabular_verbose(inputs,
      run_dsa = run_dsa,
      run_psa = run_psa, run_demo = run_demo
    )
    output_dir <- inputs$output_dir
    if (save) {
      save_outputs(outputs, output_dir, overwrite)
    }
    outputs
  }


## eval model
eval_models_from_tabular_verbose <- function(inputs, run_dsa = TRUE, run_psa = TRUE, run_demo = TRUE) {
  if (options()$heemod.verbose) {
    message("* Running files...")
  }

  list_args <- c(inputs$models, list(
    parameters = inputs$param_info$params,
    init = inputs$model_options$init, cost = inputs$model_options$cost,
    effect = inputs$model_options$effect, base_model = inputs$model_options$base_model,
    method = inputs$model_options$method, cycles = inputs$model_options$cycles
  ))
  list_args <- Filter(function(x) !is.null(x), list_args)
  if (!is.null(inputs$model_options$num_cores)) {
    heemod:::use_cluster(inputs$model_options$num_cores)
    on.exit(close_cluster())
  }
  if (options()$heemod.verbose) {
    message("** Running models...")
  }
  model_runs <- do.call(heemod:::run_model, list_args)
  model_dsa <- NULL
  if (run_dsa & !is.null(inputs$param_info$dsa)) {
    if (options()$heemod.verbose) {
      message("** Running DSA...")
    }
    model_dsa <- heemod:::run_dsa(model_runs, inputs$param_info$dsa_params)
  }
  model_psa <- NULL
  if (!is.null(inputs$param_info$psa_params) & run_psa) {
    if (options()$heemod.verbose) {
      message("** Running PSA...")
    }
    model_psa <- heemod:::run_psa(model_runs,
      psa = inputs$param_info$psa_params,
      N = inputs$model_options$n
    )
  }
  demo_res <- NULL
  if (!is.null(inputs$demographic_file) & run_demo) {
    if (options()$heemod.verbose) {
      message("** Running demographic analysis...")
    }
    demo_res <- verbose_update(model_runs, inputs$demographic_file)
  }
  return(demo_res)
}


## update.run_mode
verbose_update <- function(object, newdata, ...) {
  if (!any(class(object) %in% "run_model")) {
    stop("'object' must be the result of 'run_model()'.")
  }
  has_weights <- ".weights" %in% names(newdata)
  if (has_weights) {
    weights <- newdata$.weights
    newdata <- dplyr::select_(newdata, ~ -.weights)
  }
  else {
    message("No weights specified in update, using equal weights.")
    weights <- rep(1, nrow(newdata))
  }
  ce <- heemod:::get_ce(object)
  list_res <- list()
  for (n in heemod:::get_strategy_names(object)) {
    message(sprintf("Updating strategy '%s'...", n))
    suppressMessages({
      list_res <- c(list_res, list(heemod:::eval_strategy_newdata(object,
        strategy = n, newdata = newdata
      )))
    })
  }
  names(list_res) <- heemod:::get_strategy_names(object)
  for (n in names(list_res)) {
    list_res[[n]]$.strategy_names <- n
    list_res[[n]]$.index <- seq_len(nrow(newdata))
  }
  res <- dplyr::bind_rows(list_res)
  suppressMessages({
    res_total <- res %>%
      dplyr::rowwise() %>%
      dplyr::do_(~ heemod:::get_total_state_values(.$.mod)) %>%
      dplyr::bind_cols(res %>% dplyr::select_(~ -.mod)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate_(.dots = ce) %>%
      dplyr::left_join(dplyr::data_frame(
        .index = seq_len(nrow(newdata)),
        .weights = weights
      )) %>%
      dplyr::ungroup()
  })

  return(list_res)
}
model <- run_model_tabular_verbose("heemod data/")

indices <- model$standard$.index

all_counts <- map_dfr(indices, function(i) {
  bind_rows(
    # standard
    get_counts(model$standard[[i, 1]]) %>%
      rownames_to_column("markov_cycle") %>%
      mutate(
        initial_age = model$standard[[i, 2]],
        sex = model$standard[[i, 3]],
        cut_bmi = model$standard[[i, 4]],
        strategy = "standard",
        index = model$standard[[i, 6]]
      ),
    # policy
    get_counts(model$new[[i, 1]]) %>%
      rownames_to_column("markov_cycle") %>%
      mutate(
        initial_age = model$new[[i, 2]],
        sex = model$new[[i, 3]],
        cut_bmi = model$new[[i, 4]],
        strategy = "new",
        index = model$new[[i, 6]]
      )
  ) %>%
    mutate(as.numeric(markov_cycle))
})


all_values <- map_dfr(indices, function(i) {
  bind_rows(
    # standard
    get_values(model$standard[[i, 1]]) %>%
      mutate(
        initial_age = model$standard[[i, 2]],
        sex = model$standard[[i, 3]],
        cut_bmi = model$standard[[i, 4]],
        strategy = "standard",
        index = model$standard[[i, 6]]
      ),
    # policy
    get_values(model$new[[i, 1]]) %>%
      mutate(
        initial_age = model$new[[i, 2]],
        sex = model$new[[i, 3]],
        cut_bmi = model$new[[i, 4]],
        strategy = "new",
        index = model$new[[i, 6]]
      )
  )
})
