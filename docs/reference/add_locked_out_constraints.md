# Add locked out constraints

Add constraints to a conservation planning problem to ensure that
specific planning units are not selected (or allocated to a specific
zone) in the solution. For example, it may be useful to lock out
planning units that have been degraded and are not suitable for
conserving species. If specific planning units should be locked in to
the solution, use
[`add_locked_in_constraints()`](https://prioritizr.net/reference/add_locked_in_constraints.md).
For problems with non-binary planning unit allocations (e.g.,
proportions), the
[`add_manual_locked_constraints()`](https://prioritizr.net/reference/add_manual_locked_constraints.md)
function can be used to lock planning unit allocations to a specific
value.

## Usage

``` r
add_locked_out_constraints(x, locked_out)

# S4 method for class 'ConservationProblem,numeric'
add_locked_out_constraints(x, locked_out)

# S4 method for class 'ConservationProblem,logical'
add_locked_out_constraints(x, locked_out)

# S4 method for class 'ConservationProblem,matrix'
add_locked_out_constraints(x, locked_out)

# S4 method for class 'ConservationProblem,character'
add_locked_out_constraints(x, locked_out)

# S4 method for class 'ConservationProblem,Spatial'
add_locked_out_constraints(x, locked_out)

# S4 method for class 'ConservationProblem,sf'
add_locked_out_constraints(x, locked_out)

# S4 method for class 'ConservationProblem,Raster'
add_locked_out_constraints(x, locked_out)

# S4 method for class 'ConservationProblem,SpatRaster'
add_locked_out_constraints(x, locked_out)
```

## Arguments

- x:

  [`problem()`](https://prioritizr.net/reference/problem.md) object.

- locked_out:

  Object that determines which planning units that should be locked out.
  See the Data format section for more information.

## Value

An updated [`problem()`](https://prioritizr.net/reference/problem.md)
object with the constraints added to it.

## Data format

The following formats can be used to lock in planning units.

- `locked_out` as a `numeric` vector:

  containing `numeric` values that indicate which planning units should
  be locked for the solution. If `x` has `data.frame` planning units,
  then these values must refer to values in the `id` column of the
  planning unit data. Alternatively, if `x` has
  [`sf::st_sf()`](https://r-spatial.github.io/sf/reference/sf.html) or
  `matrix` planning units, then these values must refer to the row
  numbers of the planning unit data. Additionally, if `x` has `numeric`
  vector planning units, then these values must refer to the element
  indices of the planning unit data. Finally, if `x` has
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  planning units, then these values must refer to cell indices. Note
  that this format is available for problems that contain a single zone.

- `locked_out` as a `logical` vector:

  containing `TRUE` and/or `FALSE` values that indicate each if planning
  units should be locked in the solution. Note that the vector should
  have a `TRUE` or `FALSE` value for each and every planning unit in the
  argument to `x`. This argument is only compatible with problems that
  contain a single zone.

- `locked_out` as a `matrix` object:

  containing `logical` (i.e., `TRUE` or `FALSE`) values that indicate if
  certain planning units should be locked to a specific zone in the
  solution. Each row corresponds to a planning unit, each column
  corresponds to a zone, and each cell indicates if the planning unit
  should be locked to a given zone.

- `locked_out` as a `character` vector:

  containing column name(s) for the planning unit data in `x` that
  indicate if planning units should be locked for the solution. This
  format is only compatible if the argument to `x` has
  [`sf::st_sf()`](https://r-spatial.github.io/sf/reference/sf.html) or
  `data.frame` planning units. The columns must have `logical` (i.e.,
  `TRUE` or `FALSE`) values indicating if planning units should be
  locked for the solution. For problems that contain a single zone, the
  argument to `data` must contain a single column name. Otherwise, for
  problems that contain multiple zones, the argument to `data` must
  contain a column name for each zone.

- `locked_out` as a
  [`sf::sf()`](https://r-spatial.github.io/sf/reference/sf.html) object:

  containing geometries that will be used to lock planning units for the
  solution. Specifically, planning units in `x` that spatially intersect
  with `y` will be locked (per
  [`intersecting_units()`](https://prioritizr.net/reference/intersecting_units.md)).
  Note that this option is only available for problems that contain a
  single management zone.

- `locked_out` as a
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  object:

  containing cells used to lock planning units for the solution.
  Specifically, planning units in `x` that intersect with cells that
  have non-zero and non-`NA` values are locked. For problems that
  contain multiple zones, the `data` object must contain a layer for
  each zone. Note that for multi-band arguments, each cell must only
  contain a non-zero value in a single band. Additionally, if the cost
  data in `x` is a
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  object, we recommend standardizing `NA` values in this dataset with
  the cost data. In other words, the cells in `x` that have `NA` values
  should also have `NA` values in the locked data.

## See also

See [constraints](https://prioritizr.net/reference/constraints.md) for
an overview of all functions for adding constraints.

Other functions for adding constraints:
[`add_contiguity_constraints()`](https://prioritizr.net/reference/add_contiguity_constraints.md),
[`add_feature_contiguity_constraints()`](https://prioritizr.net/reference/add_feature_contiguity_constraints.md),
[`add_linear_constraints()`](https://prioritizr.net/reference/add_linear_constraints.md),
[`add_locked_in_constraints()`](https://prioritizr.net/reference/add_locked_in_constraints.md),
[`add_mandatory_allocation_constraints()`](https://prioritizr.net/reference/add_mandatory_allocation_constraints.md),
[`add_manual_bounded_constraints()`](https://prioritizr.net/reference/add_manual_bounded_constraints.md),
[`add_manual_locked_constraints()`](https://prioritizr.net/reference/add_manual_locked_constraints.md),
[`add_neighbor_constraints()`](https://prioritizr.net/reference/add_neighbor_constraints.md)

## Examples

``` r
# \dontrun{
# set seed for reproducibility
set.seed(500)

# load data
sim_pu_polygons <- get_sim_pu_polygons()
sim_features <- get_sim_features()
sim_locked_out_raster <- get_sim_locked_out_raster()
sim_zones_pu_raster <- get_sim_zones_pu_raster()
sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
sim_zones_features <- get_sim_zones_features()

# create minimal problem
p1 <-
  problem(sim_pu_polygons, sim_features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.2) %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# create problem with added locked out constraints using integers
p2 <- p1 %>% add_locked_out_constraints(which(sim_pu_polygons$locked_out))

# create problem with added locked out constraints using a column name
p3 <- p1 %>% add_locked_out_constraints("locked_out")

# create problem with added locked out constraints using raster data
p4 <- p1 %>% add_locked_out_constraints(sim_locked_out_raster)

# create problem with added locked out constraints using spatial polygon data
locked_out <- sim_pu_polygons[sim_pu_polygons$locked_out == 1, ]
p5 <- p1 %>% add_locked_out_constraints(locked_out)

# solve problems
s1 <- solve(p1)
#> Error: Error 10009: HostID mismatch (licensed to 2890c3bc, hostid is d03f011a)
#> Timing stopped at: 0.003 0 0.013
s2 <- solve(p2)
#> Error: Error 10009: HostID mismatch (licensed to 2890c3bc, hostid is d03f011a)
#> Timing stopped at: 0.002 0 0.012
s3 <- solve(p3)
#> Error: Error 10009: HostID mismatch (licensed to 2890c3bc, hostid is d03f011a)
#> Timing stopped at: 0.001 0.001 0.012
s4 <- solve(p4)
#> Error: Error 10009: HostID mismatch (licensed to 2890c3bc, hostid is d03f011a)
#> Timing stopped at: 0.002 0 0.012
s5 <- solve(p5)
#> Error: Error 10009: HostID mismatch (licensed to 2890c3bc, hostid is d03f011a)
#> Timing stopped at: 0.001 0.001 0.012

# create single object with all solutions
s6 <- sf::st_sf(
  tibble::tibble(
    s1 = s1$solution_1,
    s2 = s2$solution_1,
    s3 = s3$solution_1,
    s4 = s4$solution_1,
    s5 = s5$solution_1
  ),
  geometry = sf::st_geometry(s1)
)
#> Error: object 's1' not found

# plot solutions
plot(
  s6,
  main = c(
    "none locked out", "locked out (integer input)",
    "locked out (character input)", "locked out (raster input)",
    "locked out (polygon input)"
  )
)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'plot': object 's6' not found

# reset plot
par(mfrow = c(1, 1))

# create minimal multi-zone problem with spatial data
p7 <-
  problem(
    sim_zones_pu_polygons, sim_zones_features,
    cost_column = c("cost_1", "cost_2", "cost_3")
  ) %>%
  add_min_set_objective() %>%
  add_absolute_targets(matrix(rpois(15, 1), nrow = 5, ncol = 3)) %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# create multi-zone problem with locked out constraints using matrix data
locked_matrix <- as.matrix(sf::st_drop_geometry(
  sim_zones_pu_polygons[, c("locked_1", "locked_2", "locked_3")]
))

p8 <- p7 %>% add_locked_out_constraints(locked_matrix)

# solve problem
s8 <- solve(p8)
#> Error: Error 10009: HostID mismatch (licensed to 2890c3bc, hostid is d03f011a)
#> Timing stopped at: 0.001 0.001 0.013

# create new column representing the zone id that each planning unit
# was allocated to in the solution
s8$solution <- category_vector(sf::st_drop_geometry(
  s8[, c("solution_1_zone_1", "solution_1_zone_2", "solution_1_zone_3")]
))
#> Error in category_vector(sf::st_drop_geometry(s8[, c("solution_1_zone_1",     "solution_1_zone_2", "solution_1_zone_3")])): ℹ In argument to `x`.
#> Caused by error:
#> ! object 's8' not found
s8$solution <- factor(s8$solution)
#> Error: object 's8' not found

# plot solution
plot(s8[, "solution"], main = "solution", axes = FALSE)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'plot': object 's8' not found

# create multi-zone problem with locked out constraints using column names
p9 <-
  p7 %>%
  add_locked_out_constraints(c("locked_1", "locked_2", "locked_3"))

# solve problem
s9 <- solve(p9)
#> Error: Error 10009: HostID mismatch (licensed to 2890c3bc, hostid is d03f011a)
#> Timing stopped at: 0.003 0 0.013

# create new column in s8 representing the zone id that each planning unit
# was allocated to in the solution
s9$solution <- category_vector(sf::st_drop_geometry(
  s9[, c("solution_1_zone_1", "solution_1_zone_2", "solution_1_zone_3")]
))
#> Error in category_vector(sf::st_drop_geometry(s9[, c("solution_1_zone_1",     "solution_1_zone_2", "solution_1_zone_3")])): ℹ In argument to `x`.
#> Caused by error:
#> ! object 's9' not found
s9$solution[s9$solution == 1 & s9$solution_1_zone_1 == 0] <- 0
#> Error: object 's9' not found
s9$solution <- factor(s9$solution)
#> Error: object 's9' not found

# plot solution
plot(s9[, "solution"], main = "solution", axes = FALSE)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'plot': object 's9' not found

# create multi-zone problem with raster planning units
p10 <-
  problem(sim_zones_pu_raster, sim_zones_features) %>%
  add_min_set_objective() %>%
  add_absolute_targets(matrix(rpois(15, 1), nrow = 5, ncol = 3)) %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# create multi-layer raster with locked out units
locked_out_raster <- sim_zones_pu_raster[[1]]
locked_out_raster[!is.na(locked_out_raster)] <- 0
locked_out_raster <- locked_out_raster[[c(1, 1, 1)]]
names(locked_out_raster) <- c("zones_1", "zones_2", "zones_3")
locked_out_raster[[1]][1] <- 1
locked_out_raster[[2]][2] <- 1
locked_out_raster[[3]][3] <- 1

# plot locked out raster
plot(locked_out_raster)


# add locked out raster units to problem
p10 <- p10 %>% add_locked_out_constraints(locked_out_raster)

# solve problem
s10 <- solve(p10)
#> Error: Error 10009: HostID mismatch (licensed to 2890c3bc, hostid is d03f011a)
#> Timing stopped at: 0.002 0.001 0.013

# plot solution
plot(category_layer(s10), main = "solution", axes = FALSE)
#> Error in (function (cond) .Internal(C_tryCatchHelper(addr, 1L, cond)))(structure(list(message = c(i = "In argument to `x`."),     trace = structure(list(call = list(base::tryCatch(base::withCallingHandlers({        NULL        base::saveRDS(base::do.call(base::do.call, base::c(base::readRDS("/tmp/Rtmp3PZNI5/callr-fun-512f27d8719f9"),             base::list(envir = .GlobalEnv, quote = TRUE)), envir = .GlobalEnv,             quote = TRUE), file = "/tmp/Rtmp3PZNI5/callr-res-512f257a0de91",             compress = FALSE)        base::flush(base::stdout())        base::flush(base::stderr())        NULL        base::invisible()    }, error = function(e) {        {            callr_data <- base::as.environment("tools:callr")$`__callr_data__`            err <- callr_data$err            if (FALSE) {                base::assign(".Traceback", base::.traceback(4),                   envir = callr_data)                utils::dump.frames("__callr_dump__")                base::assign(".Last.dump", .GlobalEnv$`__callr_dump__`,                   envir = callr_data)                base::rm("__callr_dump__", envir = .GlobalEnv)            }            e <- err$process_call(e)            e2 <- err$new_error("error in callr subprocess")            class <- base::class            class(e2) <- base::c("callr_remote_error", class(e2))            e2 <- err$add_trace_back(e2)            cut <- base::which(e2$trace$scope == "global")[1]            if (!base::is.na(cut)) {                e2$trace <- e2$trace[-(1:cut), ]            }            base::saveRDS(base::list("error", e2, e), file = base::paste0("/tmp/Rtmp3PZNI5/callr-res-512f257a0de91",                 ".error"))        }    }, interrupt = function(e) {        {            callr_data <- base::as.environment("tools:callr")$`__callr_data__`            err <- callr_data$err            if (FALSE) {                base::assign(".Traceback", base::.traceback(4),                   envir = callr_data)                utils::dump.frames("__callr_dump__")                base::assign(".Last.dump", .GlobalEnv$`__callr_dump__`,                   envir = callr_data)                base::rm("__callr_dump__", envir = .GlobalEnv)            }            e <- err$process_call(e)            e2 <- err$new_error("error in callr subprocess")            class <- base::class            class(e2) <- base::c("callr_remote_error", class(e2))            e2 <- err$add_trace_back(e2)            cut <- base::which(e2$trace$scope == "global")[1]            if (!base::is.na(cut)) {                e2$trace <- e2$trace[-(1:cut), ]            }            base::saveRDS(base::list("error", e2, e), file = base::paste0("/tmp/Rtmp3PZNI5/callr-res-512f257a0de91",                 ".error"))        }    }, callr_message = function(e) {        base::try(base::signalCondition(e))    }), error = function(e) {        NULL        if (FALSE) {            base::try(base::stop(e))        }        else {            base::invisible()        }    }, interrupt = function(e) {        NULL        if (FALSE) {            e        }        else {            base::invisible()        }    }), tryCatchList(expr, classes, parentenv, handlers), tryCatchOne(tryCatchList(expr,         names[-nh], parentenv, handlers[-nh]), names[nh], parentenv,         handlers[[nh]]), doTryCatch(return(expr), name, parentenv,         handler), tryCatchList(expr, names[-nh], parentenv, handlers[-nh]),         tryCatchOne(expr, names, parentenv, handlers[[1L]]),         doTryCatch(return(expr), name, parentenv, handler), base::withCallingHandlers({            NULL            base::saveRDS(base::do.call(base::do.call, base::c(base::readRDS("/tmp/Rtmp3PZNI5/callr-fun-512f27d8719f9"),                 base::list(envir = .GlobalEnv, quote = TRUE)),                 envir = .GlobalEnv, quote = TRUE), file = "/tmp/Rtmp3PZNI5/callr-res-512f257a0de91",                 compress = FALSE)            base::flush(base::stdout())            base::flush(base::stderr())            NULL            base::invisible()        }, error = function(e) {            {                callr_data <- base::as.environment("tools:callr")$`__callr_data__`                err <- callr_data$err                if (FALSE) {                  base::assign(".Traceback", base::.traceback(4),                     envir = callr_data)                  utils::dump.frames("__callr_dump__")                  base::assign(".Last.dump", .GlobalEnv$`__callr_dump__`,                     envir = callr_data)                  base::rm("__callr_dump__", envir = .GlobalEnv)                }                e <- err$process_call(e)                e2 <- err$new_error("error in callr subprocess")                class <- base::class                class(e2) <- base::c("callr_remote_error", class(e2))                e2 <- err$add_trace_back(e2)                cut <- base::which(e2$trace$scope == "global")[1]                if (!base::is.na(cut)) {                  e2$trace <- e2$trace[-(1:cut), ]                }                base::saveRDS(base::list("error", e2, e), file = base::paste0("/tmp/Rtmp3PZNI5/callr-res-512f257a0de91",                   ".error"))            }        }, interrupt = function(e) {            {                callr_data <- base::as.environment("tools:callr")$`__callr_data__`                err <- callr_data$err                if (FALSE) {                  base::assign(".Traceback", base::.traceback(4),                     envir = callr_data)                  utils::dump.frames("__callr_dump__")                  base::assign(".Last.dump", .GlobalEnv$`__callr_dump__`,                     envir = callr_data)                  base::rm("__callr_dump__", envir = .GlobalEnv)                }                e <- err$process_call(e)                e2 <- err$new_error("error in callr subprocess")                class <- base::class                class(e2) <- base::c("callr_remote_error", class(e2))                e2 <- err$add_trace_back(e2)                cut <- base::which(e2$trace$scope == "global")[1]                if (!base::is.na(cut)) {                  e2$trace <- e2$trace[-(1:cut), ]                }                base::saveRDS(base::list("error", e2, e), file = base::paste0("/tmp/Rtmp3PZNI5/callr-res-512f257a0de91",                   ".error"))            }        }, callr_message = function(e) {            base::try(base::signalCondition(e))        }), base::saveRDS(base::do.call(base::do.call, base::c(base::readRDS("/tmp/Rtmp3PZNI5/callr-fun-512f27d8719f9"),             base::list(envir = .GlobalEnv, quote = TRUE)), envir = .GlobalEnv,             quote = TRUE), file = "/tmp/Rtmp3PZNI5/callr-res-512f257a0de91",             compress = FALSE), base::do.call(base::do.call, base::c(base::readRDS("/tmp/Rtmp3PZNI5/callr-fun-512f27d8719f9"),             base::list(envir = .GlobalEnv, quote = TRUE)), envir = .GlobalEnv,             quote = TRUE), `<fn>`(base::quote(`<fn>`), base::quote(`<named list>`),             envir = base::quote(`<env>`), quote = base::quote(TRUE)),         `<fn>`(pkg = base::quote(`<pkgdown>`), examples = base::quote(TRUE),             run_dont_run = base::quote(TRUE), seed = base::quote(1014L),             lazy = base::quote(TRUE), override = base::quote(`<list>`),             install = base::quote(FALSE), preview = base::quote(FALSE),             new_process = base::quote(FALSE), devel = base::quote(FALSE),             quiet = base::quote(TRUE), cli_colors = base::quote(16777216L),             hyperlinks = base::quote(FALSE)), pkgdown::build_site(...),         build_site_local(pkg = pkg, examples = examples, run_dont_run = run_dont_run,             seed = seed, lazy = lazy, override = override, preview = preview,             devel = devel, quiet = quiet), build_reference(pkg,             lazy = lazy, examples = examples, run_dont_run = run_dont_run,             seed = seed, override = override, preview = FALSE,             devel = devel), unwrap_purrr_error(purrr::map(topics,             build_reference_topic, pkg = pkg, lazy = lazy, examples_env = examples_env,             run_dont_run = run_dont_run)), withCallingHandlers(code,             purrr_error_indexed = function(err) {                cnd_signal(err$parent)            }), purrr::map(topics, build_reference_topic, pkg = pkg,             lazy = lazy, examples_env = examples_env, run_dont_run = run_dont_run),         map_("list", .x, .f, ..., .progress = .progress), with_indexed_errors(i = i,             names = names, error_call = .purrr_error_call, call_with_cleanup(map_impl,                 environment(), .type, .progress, n, names, i)),         withCallingHandlers(expr, error = function(cnd) {            if (i == 0L) {            }            else {                message <- c(i = "In index: {i}.")                if (!is.null(names) && !is.na(names[[i]]) &&                   names[[i]] != "") {                  name <- names[[i]]                  message <- c(message, i = "With name: {name}.")                }                else {                  name <- NULL                }                cli::cli_abort(message, location = i, name = name,                   parent = cnd, call = error_call, class = "purrr_error_indexed")            }        }), call_with_cleanup(map_impl, environment(), .type,             .progress, n, names, i), .f(.x[[i]], ...), withCallingHandlers(data_reference_topic(topic,             pkg, examples_env = examples_env, run_dont_run = run_dont_run),             error = function(err) {                cli::cli_abort("Failed to parse Rd in {.file {topic$file_in}}",                   parent = err, call = quote(build_reference()))            }), data_reference_topic(topic, pkg, examples_env = examples_env,             run_dont_run = run_dont_run), run_examples(tags$tag_examples[[1]],             env = if (is.null(examples_env)) NULL else new.env(parent = examples_env),             topic = tools::file_path_sans_ext(topic$file_in),             run_dont_run = run_dont_run), highlight_examples(code,             topic, env = env), downlit::evaluate_and_highlight(code,             fig_save = fig_save_topic, env = eval_env, output_handler = handler),         evaluate::evaluate(code, child_env(env), new_device = TRUE,             output_handler = output_handler), withRestarts(with_handlers({            for (expr in tle$exprs) {                ev <- withVisible(eval(expr, envir))                watcher$capture_plot_and_output()                watcher$print_value(ev$value, ev$visible, envir)            }            TRUE        }, handlers), eval_continue = function() TRUE, eval_stop = function() FALSE),         withRestartList(expr, restarts), withOneRestart(withRestartList(expr,             restarts[-nr]), restarts[[nr]]), doWithOneRestart(return(expr),             restart), withRestartList(expr, restarts[-nr]), withOneRestart(expr,             restarts[[1L]]), doWithOneRestart(return(expr), restart),         with_handlers({            for (expr in tle$exprs) {                ev <- withVisible(eval(expr, envir))                watcher$capture_plot_and_output()                watcher$print_value(ev$value, ev$visible, envir)            }            TRUE        }, handlers), eval(call), eval(call), withCallingHandlers(code,             message = `<fn>`, warning = `<fn>`, error = `<fn>`),         withVisible(eval(expr, envir)), eval(expr, envir), eval(expr,             envir), plot(category_layer(s10), main = "solution",             axes = FALSE), category_layer(s10), assert_required(x),         cli::cli_abort(message = err_msg, parent = attr(res,             "condition")$parent, call = call, .internal = .internal),         rlang::abort(message, ..., call = call, use_cli_format = TRUE,             .frame = .frame)), parent = c(0L, 1L, 2L, 3L, 2L,     5L, 6L, 0L, 0L, 0L, 0L, 0L, 12L, 13L, 14L, 15L, 16L, 15L,     18L, 19L, 20L, 19L, 19L, 23L, 23L, 25L, 26L, 27L, 28L, 29L,     30L, 31L, 32L, 31L, 34L, 35L, 29L, 37L, 38L, 37L, 29L, 29L,     42L, 43L, 43L, 45L, 46L, 47L), visible = c(TRUE, TRUE, TRUE,     TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,     TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,     TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,     TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,     TRUE, TRUE, FALSE, FALSE, FALSE), namespace = c("base", "base",     "base", "base", "base", "base", "base", "base", "base", "base",     "base", NA, "pkgdown", "pkgdown", "pkgdown", "pkgdown", "base",     "purrr", "purrr", "purrr", "base", "purrr", "pkgdown", "base",     "pkgdown", "pkgdown", "pkgdown", "downlit", "evaluate", "base",     "base", "base", "base", "base", "base", "base", "evaluate",     "base", "base", "base", "base", "base", "base", "base", "prioritizr",     "prioritizr", "cli", "rlang"), scope = c("::", "local", "local",     "local", "local", "local", "local", "::", "::", "::", "local",     "global", "::", ":::", "::", ":::", "::", "::", ":::", ":::",     "::", ":::", "local", "::", ":::", ":::", ":::", "::", "::",     "::", "local", "local", "local", "local", "local", "local",     ":::", "::", "::", "::", "::", "::", "::", "::", "::", ":::",     "::", "::"), error_frame = c(FALSE, FALSE, FALSE, FALSE,     FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,     FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,     FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,     FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,     FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)), row.names = c(NA,     -48L), version = 2L, class = c("rlang_trace", "rlib_trace",     "tbl", "data.frame")), parent = NULL, body = c("\033[1mCaused by error:\033[22m",     `!` = "object 's10' not found"), rlang = list(inherit = TRUE),     call = category_layer(s10), use_cli_format = TRUE), class = c("rlang_error", "error", "condition"))): error in evaluating the argument 'x' in selecting a method for function 'plot': ℹ In argument to `x`.
#> Caused by error:
#> ! object 's10' not found
# }
```
