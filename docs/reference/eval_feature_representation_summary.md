# Evaluate feature representation by solution

Calculate how well features are represented by a solution to a
conservation planning problem. These summary statistics are reported for
each and every feature, and each and every zone, within a conservation
planning problem.

## Usage

``` r
eval_feature_representation_summary(x, solution)
```

## Arguments

- x:

  [`problem()`](https://prioritizr.net/reference/problem.md) object.

- solution:

  `numeric`, `matrix`, `data.frame`,
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html),
  or [`sf::sf()`](https://r-spatial.github.io/sf/reference/sf.html)
  object. The argument should be in the same format as the planning unit
  cost data in the argument to `x`. See the Solution format section for
  more information.

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
object describing feature representation. Here, each row describes a
specific summary statistic (e.g., different management zone) for a
specific feature. It contains the following columns:

- summary:

  `character` description of the summary statistic. The statistics
  associated with the `"overall"` value in this column are calculated
  using all planning unit values. For problems with multiple management
  zones, this means that all calculations are completed by summing
  together all planning unit values across all zones. For example, if
  there are two zones, a single planning unit, and a feature has a value
  of one in the single planning unit for both zones, then `total_amount`
  will contain a value of two (even though it would not be possible to
  to achieve a value of two because the planning unit could not
  simultaneously be allocated to both zones). Additionally, if multiple
  management zones are present, then summary statistics are also
  provided for each zone separately (indicated using zone names).

- feature:

  `character` name of the feature.

- total_amount:

  `numeric` total amount of each feature available in the entire
  conservation planning problem (not just planning units selected within
  the solution). It is calculated as the sum of the feature data,
  supplied when creating a
  [`problem()`](https://prioritizr.net/reference/problem.md) object
  (e.g., presence/absence values).

- absolute_held:

  `numeric` total amount of each feature secured within the solution. It
  is calculated as the sum of the feature data, supplied when creating a
  [`problem()`](https://prioritizr.net/reference/problem.md) object
  (e.g., presence/absence values), weighted by the status of each
  planning unit in the solution (e.g., selected or not for
  prioritization).

- relative_held:

  `numeric` proportion of each feature secured within the solution. It
  is calculated by dividing values in the `"absolute_held"` column by
  those in the `"total_amount"` column.

## Solution format

Broadly speaking, the argument to `solution` must be in the same format
as the planning unit data in the argument to `x`. Further details on the
correct format are listed separately for each of the different planning
unit data formats:

- `x` has `numeric` planning units:

  The argument to `solution` must be a `numeric` vector with each
  element corresponding to a different planning unit. It should have the
  same number of planning units as those in the argument to `x`.
  Additionally, any planning units missing cost (`NA`) values should
  also have missing (`NA`) values in the argument to `solution`.

- `x` has `matrix` planning units:

  The argument to `solution` must be a `matrix` vector with each row
  corresponding to a different planning unit, and each column correspond
  to a different management zone. It should have the same number of
  planning units and zones as those in the argument to `x`.
  Additionally, any planning units missing cost (`NA`) values for a
  particular zone should also have a missing (`NA`) values in the
  argument to `solution`.

- `x` has
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  planning units:

  The argument to `solution` be a
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  object where different cells correspond to different planning units
  and layers correspond to a different management zones. It should have
  the same dimensionality (rows, columns, layers), resolution, extent,
  and coordinate reference system as the planning units in the argument
  to `x`. Additionally, any planning units missing cost (`NA`) values
  for a particular zone should also have missing (`NA`) values in the
  argument to `solution`.

- `x` has `data.frame` planning units:

  The argument to `solution` must be a `data.frame` with each column
  corresponding to a different zone, each row corresponding to a
  different planning unit, and cell values corresponding to the solution
  value. This means that if a `data.frame` object containing the
  solution also contains additional columns, then these columns will
  need to be subsetted prior to using this function (see below for
  example with
  [`sf::sf()`](https://r-spatial.github.io/sf/reference/sf.html) data).
  Additionally, any planning units missing cost (`NA`) values for a
  particular zone should also have missing (`NA`) values in the argument
  to `solution`.

- `x` has [`sf::sf()`](https://r-spatial.github.io/sf/reference/sf.html)
  planning units:

  The argument to `solution` must be a
  [`sf::sf()`](https://r-spatial.github.io/sf/reference/sf.html) object
  with each column corresponding to a different zone, each row
  corresponding to a different planning unit, and cell values
  corresponding to the solution value. This means that if the
  [`sf::sf()`](https://r-spatial.github.io/sf/reference/sf.html) object
  containing the solution also contains additional columns, then these
  columns will need to be subsetted prior to using this function (see
  below for example). Additionally, the argument to `solution` must also
  have the same coordinate reference system as the planning unit data.
  Furthermore, any planning units missing cost (`NA`) values for a
  particular zone should also have missing (`NA`) values in the argument
  to `solution`.

## See also

See [summaries](https://prioritizr.net/reference/summaries.md) for an
overview of all functions for summarizing solutions.

Other functions for summarizing solutions:
[`eval_asym_connectivity_summary()`](https://prioritizr.net/reference/eval_asym_connectivity_summary.md),
[`eval_boundary_summary()`](https://prioritizr.net/reference/eval_boundary_summary.md),
[`eval_connectivity_summary()`](https://prioritizr.net/reference/eval_connectivity_summary.md),
[`eval_cost_summary()`](https://prioritizr.net/reference/eval_cost_summary.md),
[`eval_n_summary()`](https://prioritizr.net/reference/eval_n_summary.md),
[`eval_target_coverage_summary()`](https://prioritizr.net/reference/eval_target_coverage_summary.md)

## Examples

``` r
# \dontrun{
# set seed for reproducibility
set.seed(500)

# load data
sim_pu_raster <- get_sim_pu_raster()
sim_pu_polygons <- get_sim_pu_polygons()
sim_features <- get_sim_features()
sim_zones_pu_raster <- get_sim_zones_pu_raster()
sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
sim_zones_features <- get_sim_zones_features()

# create a simple conservation planning dataset so we can see exactly
# how feature representation is calculated
pu <- data.frame(
  id = seq_len(10),
  cost = c(0.2, NA, runif(8)),
  spp1 = runif(10),
  spp2 = c(rpois(9, 4), NA)
)

# create problem
p1 <-
  problem(pu, c("spp1", "spp2"), cost_column = "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# create a solution
# specifically, a data.frame with a single column that contains
# binary values indicating if each planning units was selected or not
s1 <- data.frame(s = c(1, NA, rep(c(1, 0), 4)))
print(s1)
#>     s
#> 1   1
#> 2  NA
#> 3   1
#> 4   0
#> 5   1
#> 6   0
#> 7   1
#> 8   0
#> 9   1
#> 10  0

# calculate feature representation
r1 <- eval_feature_representation_summary(p1, s1)
print(r1)
#> # A tibble: 2 × 5
#>   summary feature total_amount absolute_held relative_held
#>   <chr>   <chr>          <dbl>         <dbl>         <dbl>
#> 1 overall spp1            5.76          3.12         0.541
#> 2 overall spp2           33            14            0.424

# let's verify that feature representation calculations are correct
# by manually performing the calculations and compare the results with r1
## calculate total amount for each feature
print(
  setNames(
    c(sum(pu$spp1, na.rm = TRUE), sum(pu$spp2, na.rm = TRUE)),
    c("spp1", "spp2")
  )
)
#>      spp1      spp2 
#>  5.755739 33.000000 

## calculate absolute amount held for each feature
print(
  setNames(
    c(sum(pu$spp1 * s1$s, na.rm = TRUE), sum(pu$spp2 * s1$s, na.rm = TRUE)),
    c("spp1", "spp2")
  )
)
#>      spp1      spp2 
#>  3.116052 14.000000 

## calculate relative amount held for each feature
print(
  setNames(
    c(
      sum(pu$spp1 * s1$s, na.rm = TRUE) / sum(pu$spp1, na.rm = TRUE),
      sum(pu$spp2 * s1$s, na.rm = TRUE) / sum(pu$spp2, na.rm = TRUE)
    ),
    c("spp1", "spp2")
  )
)
#>      spp1      spp2 
#> 0.5413818 0.4242424 

# solve problem using an exact algorithm solver
s1_2 <- solve(p1)
#> Error: Error 10009: HostID mismatch (licensed to 2890c3bc, hostid is d03f011a)
#> Timing stopped at: 0.003 0 0.013
print(s1_2)
#> Error: object 's1_2' not found

# calculate feature representation in this solution
r1_2 <- eval_feature_representation_summary(
  p1, s1_2[, "solution_1", drop = FALSE]
)
#> Error in eval_feature_representation_summary(p1, s1_2[, "solution_1",     drop = FALSE]): ℹ In argument to `solution`.
#> Caused by error:
#> ! object 's1_2' not found
print(r1_2)
#> Error: object 'r1_2' not found

# build minimal conservation problem with raster data
p2 <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# solve problem
s2 <- solve(p2)
#> Error: Error 10009: HostID mismatch (licensed to 2890c3bc, hostid is d03f011a)
#> Timing stopped at: 0.003 0 0.013

# print solution
print(s2)
#> Error: object 's2' not found

# calculate feature representation in the solution
r2 <- eval_feature_representation_summary(p2, s2)
#> Error in eval_feature_representation_summary(p2, s2): ℹ In argument to `solution`.
#> Caused by error:
#> ! object 's2' not found
print(r2)
#> Error: object 'r2' not found

# plot solution
plot(s2, main = "solution", axes = FALSE)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'plot': object 's2' not found

# build minimal conservation problem with polygon data
p3 <-
  problem(sim_pu_polygons, sim_features, cost_column = "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# solve problem
s3 <- solve(p3)
#> Error: Error 10009: HostID mismatch (licensed to 2890c3bc, hostid is d03f011a)
#> Timing stopped at: 0.002 0 0.013

# print first six rows of the attribute table
print(head(s3))
#> Error: object 's3' not found

# calculate feature representation in the solution
r3 <- eval_feature_representation_summary(p3, s3[, "solution_1"])
#> Error in eval_feature_representation_summary(p3, s3[, "solution_1"]): ℹ In argument to `solution`.
#> Caused by error:
#> ! object 's3' not found
print(r3)
#> Error: object 'r3' not found

# plot solution
plot(s3[, "solution_1"], main = "solution", axes = FALSE)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'plot': object 's3' not found

# build multi-zone conservation problem with raster data
p4 <-
  problem(sim_zones_pu_raster, sim_zones_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(matrix(runif(15, 0.1, 0.2), nrow = 5, ncol = 3)) %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# solve problem
s4 <- solve(p4)
#> Error: Error 10009: HostID mismatch (licensed to 2890c3bc, hostid is d03f011a)
#> Timing stopped at: 0.003 0 0.013

# print solution
print(s4)
#> Error: object 's4' not found

# calculate feature representation in the solution
r4 <- eval_feature_representation_summary(p4, s4)
#> Error in eval_feature_representation_summary(p4, s4): ℹ In argument to `solution`.
#> Caused by error:
#> ! object 's4' not found
print(r4)
#> Error: object 'r4' not found

# plot solution
plot(category_layer(s4), main = "solution", axes = FALSE)
#> Error in (function (cond) .Internal(C_tryCatchHelper(addr, 1L, cond)))(structure(list(message = c(i = "In argument to `x`."),     trace = structure(list(call = list(base::tryCatch(base::withCallingHandlers({        NULL        base::saveRDS(base::do.call(base::do.call, base::c(base::readRDS("/tmp/Rtmp3PZNI5/callr-fun-512f27d8719f9"),             base::list(envir = .GlobalEnv, quote = TRUE)), envir = .GlobalEnv,             quote = TRUE), file = "/tmp/Rtmp3PZNI5/callr-res-512f257a0de91",             compress = FALSE)        base::flush(base::stdout())        base::flush(base::stderr())        NULL        base::invisible()    }, error = function(e) {        {            callr_data <- base::as.environment("tools:callr")$`__callr_data__`            err <- callr_data$err            if (FALSE) {                base::assign(".Traceback", base::.traceback(4),                   envir = callr_data)                utils::dump.frames("__callr_dump__")                base::assign(".Last.dump", .GlobalEnv$`__callr_dump__`,                   envir = callr_data)                base::rm("__callr_dump__", envir = .GlobalEnv)            }            e <- err$process_call(e)            e2 <- err$new_error("error in callr subprocess")            class <- base::class            class(e2) <- base::c("callr_remote_error", class(e2))            e2 <- err$add_trace_back(e2)            cut <- base::which(e2$trace$scope == "global")[1]            if (!base::is.na(cut)) {                e2$trace <- e2$trace[-(1:cut), ]            }            base::saveRDS(base::list("error", e2, e), file = base::paste0("/tmp/Rtmp3PZNI5/callr-res-512f257a0de91",                 ".error"))        }    }, interrupt = function(e) {        {            callr_data <- base::as.environment("tools:callr")$`__callr_data__`            err <- callr_data$err            if (FALSE) {                base::assign(".Traceback", base::.traceback(4),                   envir = callr_data)                utils::dump.frames("__callr_dump__")                base::assign(".Last.dump", .GlobalEnv$`__callr_dump__`,                   envir = callr_data)                base::rm("__callr_dump__", envir = .GlobalEnv)            }            e <- err$process_call(e)            e2 <- err$new_error("error in callr subprocess")            class <- base::class            class(e2) <- base::c("callr_remote_error", class(e2))            e2 <- err$add_trace_back(e2)            cut <- base::which(e2$trace$scope == "global")[1]            if (!base::is.na(cut)) {                e2$trace <- e2$trace[-(1:cut), ]            }            base::saveRDS(base::list("error", e2, e), file = base::paste0("/tmp/Rtmp3PZNI5/callr-res-512f257a0de91",                 ".error"))        }    }, callr_message = function(e) {        base::try(base::signalCondition(e))    }), error = function(e) {        NULL        if (FALSE) {            base::try(base::stop(e))        }        else {            base::invisible()        }    }, interrupt = function(e) {        NULL        if (FALSE) {            e        }        else {            base::invisible()        }    }), tryCatchList(expr, classes, parentenv, handlers), tryCatchOne(tryCatchList(expr,         names[-nh], parentenv, handlers[-nh]), names[nh], parentenv,         handlers[[nh]]), doTryCatch(return(expr), name, parentenv,         handler), tryCatchList(expr, names[-nh], parentenv, handlers[-nh]),         tryCatchOne(expr, names, parentenv, handlers[[1L]]),         doTryCatch(return(expr), name, parentenv, handler), base::withCallingHandlers({            NULL            base::saveRDS(base::do.call(base::do.call, base::c(base::readRDS("/tmp/Rtmp3PZNI5/callr-fun-512f27d8719f9"),                 base::list(envir = .GlobalEnv, quote = TRUE)),                 envir = .GlobalEnv, quote = TRUE), file = "/tmp/Rtmp3PZNI5/callr-res-512f257a0de91",                 compress = FALSE)            base::flush(base::stdout())            base::flush(base::stderr())            NULL            base::invisible()        }, error = function(e) {            {                callr_data <- base::as.environment("tools:callr")$`__callr_data__`                err <- callr_data$err                if (FALSE) {                  base::assign(".Traceback", base::.traceback(4),                     envir = callr_data)                  utils::dump.frames("__callr_dump__")                  base::assign(".Last.dump", .GlobalEnv$`__callr_dump__`,                     envir = callr_data)                  base::rm("__callr_dump__", envir = .GlobalEnv)                }                e <- err$process_call(e)                e2 <- err$new_error("error in callr subprocess")                class <- base::class                class(e2) <- base::c("callr_remote_error", class(e2))                e2 <- err$add_trace_back(e2)                cut <- base::which(e2$trace$scope == "global")[1]                if (!base::is.na(cut)) {                  e2$trace <- e2$trace[-(1:cut), ]                }                base::saveRDS(base::list("error", e2, e), file = base::paste0("/tmp/Rtmp3PZNI5/callr-res-512f257a0de91",                   ".error"))            }        }, interrupt = function(e) {            {                callr_data <- base::as.environment("tools:callr")$`__callr_data__`                err <- callr_data$err                if (FALSE) {                  base::assign(".Traceback", base::.traceback(4),                     envir = callr_data)                  utils::dump.frames("__callr_dump__")                  base::assign(".Last.dump", .GlobalEnv$`__callr_dump__`,                     envir = callr_data)                  base::rm("__callr_dump__", envir = .GlobalEnv)                }                e <- err$process_call(e)                e2 <- err$new_error("error in callr subprocess")                class <- base::class                class(e2) <- base::c("callr_remote_error", class(e2))                e2 <- err$add_trace_back(e2)                cut <- base::which(e2$trace$scope == "global")[1]                if (!base::is.na(cut)) {                  e2$trace <- e2$trace[-(1:cut), ]                }                base::saveRDS(base::list("error", e2, e), file = base::paste0("/tmp/Rtmp3PZNI5/callr-res-512f257a0de91",                   ".error"))            }        }, callr_message = function(e) {            base::try(base::signalCondition(e))        }), base::saveRDS(base::do.call(base::do.call, base::c(base::readRDS("/tmp/Rtmp3PZNI5/callr-fun-512f27d8719f9"),             base::list(envir = .GlobalEnv, quote = TRUE)), envir = .GlobalEnv,             quote = TRUE), file = "/tmp/Rtmp3PZNI5/callr-res-512f257a0de91",             compress = FALSE), base::do.call(base::do.call, base::c(base::readRDS("/tmp/Rtmp3PZNI5/callr-fun-512f27d8719f9"),             base::list(envir = .GlobalEnv, quote = TRUE)), envir = .GlobalEnv,             quote = TRUE), `<fn>`(base::quote(`<fn>`), base::quote(`<named list>`),             envir = base::quote(`<env>`), quote = base::quote(TRUE)),         `<fn>`(pkg = base::quote(`<pkgdown>`), examples = base::quote(TRUE),             run_dont_run = base::quote(TRUE), seed = base::quote(1014L),             lazy = base::quote(TRUE), override = base::quote(`<list>`),             install = base::quote(FALSE), preview = base::quote(FALSE),             new_process = base::quote(FALSE), devel = base::quote(FALSE),             quiet = base::quote(TRUE), cli_colors = base::quote(16777216L),             hyperlinks = base::quote(FALSE)), pkgdown::build_site(...),         build_site_local(pkg = pkg, examples = examples, run_dont_run = run_dont_run,             seed = seed, lazy = lazy, override = override, preview = preview,             devel = devel, quiet = quiet), build_reference(pkg,             lazy = lazy, examples = examples, run_dont_run = run_dont_run,             seed = seed, override = override, preview = FALSE,             devel = devel), unwrap_purrr_error(purrr::map(topics,             build_reference_topic, pkg = pkg, lazy = lazy, examples_env = examples_env,             run_dont_run = run_dont_run)), withCallingHandlers(code,             purrr_error_indexed = function(err) {                cnd_signal(err$parent)            }), purrr::map(topics, build_reference_topic, pkg = pkg,             lazy = lazy, examples_env = examples_env, run_dont_run = run_dont_run),         map_("list", .x, .f, ..., .progress = .progress), with_indexed_errors(i = i,             names = names, error_call = .purrr_error_call, call_with_cleanup(map_impl,                 environment(), .type, .progress, n, names, i)),         withCallingHandlers(expr, error = function(cnd) {            if (i == 0L) {            }            else {                message <- c(i = "In index: {i}.")                if (!is.null(names) && !is.na(names[[i]]) &&                   names[[i]] != "") {                  name <- names[[i]]                  message <- c(message, i = "With name: {name}.")                }                else {                  name <- NULL                }                cli::cli_abort(message, location = i, name = name,                   parent = cnd, call = error_call, class = "purrr_error_indexed")            }        }), call_with_cleanup(map_impl, environment(), .type,             .progress, n, names, i), .f(.x[[i]], ...), withCallingHandlers(data_reference_topic(topic,             pkg, examples_env = examples_env, run_dont_run = run_dont_run),             error = function(err) {                cli::cli_abort("Failed to parse Rd in {.file {topic$file_in}}",                   parent = err, call = quote(build_reference()))            }), data_reference_topic(topic, pkg, examples_env = examples_env,             run_dont_run = run_dont_run), run_examples(tags$tag_examples[[1]],             env = if (is.null(examples_env)) NULL else new.env(parent = examples_env),             topic = tools::file_path_sans_ext(topic$file_in),             run_dont_run = run_dont_run), highlight_examples(code,             topic, env = env), downlit::evaluate_and_highlight(code,             fig_save = fig_save_topic, env = eval_env, output_handler = handler),         evaluate::evaluate(code, child_env(env), new_device = TRUE,             output_handler = output_handler), withRestarts(with_handlers({            for (expr in tle$exprs) {                ev <- withVisible(eval(expr, envir))                watcher$capture_plot_and_output()                watcher$print_value(ev$value, ev$visible, envir)            }            TRUE        }, handlers), eval_continue = function() TRUE, eval_stop = function() FALSE),         withRestartList(expr, restarts), withOneRestart(withRestartList(expr,             restarts[-nr]), restarts[[nr]]), doWithOneRestart(return(expr),             restart), withRestartList(expr, restarts[-nr]), withOneRestart(expr,             restarts[[1L]]), doWithOneRestart(return(expr), restart),         with_handlers({            for (expr in tle$exprs) {                ev <- withVisible(eval(expr, envir))                watcher$capture_plot_and_output()                watcher$print_value(ev$value, ev$visible, envir)            }            TRUE        }, handlers), eval(call), eval(call), withCallingHandlers(code,             message = `<fn>`, warning = `<fn>`, error = `<fn>`),         withVisible(eval(expr, envir)), eval(expr, envir), eval(expr,             envir), plot(category_layer(s4), main = "solution",             axes = FALSE), category_layer(s4), assert_required(x),         cli::cli_abort(message = err_msg, parent = attr(res,             "condition")$parent, call = call, .internal = .internal),         rlang::abort(message, ..., call = call, use_cli_format = TRUE,             .frame = .frame)), parent = c(0L, 1L, 2L, 3L, 2L,     5L, 6L, 0L, 0L, 0L, 0L, 0L, 12L, 13L, 14L, 15L, 16L, 15L,     18L, 19L, 20L, 19L, 19L, 23L, 23L, 25L, 26L, 27L, 28L, 29L,     30L, 31L, 32L, 31L, 34L, 35L, 29L, 37L, 38L, 37L, 29L, 29L,     42L, 43L, 43L, 45L, 46L, 47L), visible = c(TRUE, TRUE, TRUE,     TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,     TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,     TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,     TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,     TRUE, TRUE, FALSE, FALSE, FALSE), namespace = c("base", "base",     "base", "base", "base", "base", "base", "base", "base", "base",     "base", NA, "pkgdown", "pkgdown", "pkgdown", "pkgdown", "base",     "purrr", "purrr", "purrr", "base", "purrr", "pkgdown", "base",     "pkgdown", "pkgdown", "pkgdown", "downlit", "evaluate", "base",     "base", "base", "base", "base", "base", "base", "evaluate",     "base", "base", "base", "base", "base", "base", "base", "prioritizr",     "prioritizr", "cli", "rlang"), scope = c("::", "local", "local",     "local", "local", "local", "local", "::", "::", "::", "local",     "global", "::", ":::", "::", ":::", "::", "::", ":::", ":::",     "::", ":::", "local", "::", ":::", ":::", ":::", "::", "::",     "::", "local", "local", "local", "local", "local", "local",     ":::", "::", "::", "::", "::", "::", "::", "::", "::", ":::",     "::", "::"), error_frame = c(FALSE, FALSE, FALSE, FALSE,     FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,     FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,     FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,     FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,     FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)), row.names = c(NA,     -48L), version = 2L, class = c("rlang_trace", "rlib_trace",     "tbl", "data.frame")), parent = NULL, body = c("\033[1mCaused by error:\033[22m",     `!` = "object 's4' not found"), rlang = list(inherit = TRUE),     call = category_layer(s4), use_cli_format = TRUE), class = c("rlang_error", "error", "condition"))): error in evaluating the argument 'x' in selecting a method for function 'plot': ℹ In argument to `x`.
#> Caused by error:
#> ! object 's4' not found

# build multi-zone conservation problem with polygon data
p5 <-
  problem(
    sim_zones_pu_polygons, sim_zones_features,
    cost_column = c("cost_1", "cost_2", "cost_3")
  ) %>%
  add_min_set_objective() %>%
  add_relative_targets(matrix(runif(15, 0.1, 0.2), nrow = 5, ncol = 3)) %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# solve problem
s5 <- solve(p5)
#> Error: Error 10009: HostID mismatch (licensed to 2890c3bc, hostid is d03f011a)
#> Timing stopped at: 0.002 0 0.013

# print first six rows of the attribute table
print(head(s5))
#> Error: object 's5' not found

# calculate feature representation in the solution
r5 <- eval_feature_representation_summary(
  p5, s5[, c("solution_1_zone_1", "solution_1_zone_2", "solution_1_zone_3")]
)
#> Error in eval_feature_representation_summary(p5, s5[, c("solution_1_zone_1",     "solution_1_zone_2", "solution_1_zone_3")]): ℹ In argument to `solution`.
#> Caused by error:
#> ! object 's5' not found
print(r5)
#> Error: object 'r5' not found

# create new column representing the zone id that each planning unit
# was allocated to in the solution
s5$solution <- category_vector(
  s5[, c("solution_1_zone_1", "solution_1_zone_2", "solution_1_zone_3")]
)
#> Error in category_vector(s5[, c("solution_1_zone_1", "solution_1_zone_2",     "solution_1_zone_3")]): ℹ In argument to `x`.
#> Caused by error:
#> ! object 's5' not found
s5$solution <- factor(s5$solution)
#> Error: object 's5' not found

# plot solution
plot(s5[, "solution"])
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'plot': object 's5' not found
# }
```
