# Add relative targets

Add targets to a conservation planning problem expressed as a proportion
(between 0 and 1) of the maximum level of representation of each feature
in the study area. Please note that proportions are scaled according to
the features' total abundances in the study area (including any locked
out planning units, or planning units with `NA` cost values) using the
[`feature_abundances()`](https://prioritizr.net/reference/feature_abundances.md)
function.

## Usage

``` r
add_relative_targets(x, targets)

# S4 method for class 'ConservationProblem,numeric'
add_relative_targets(x, targets)

# S4 method for class 'ConservationProblem,matrix'
add_relative_targets(x, targets)

# S4 method for class 'ConservationProblem,character'
add_relative_targets(x, targets)
```

## Arguments

- x:

  [`problem()`](https://prioritizr.net/reference/problem.md) object.

- targets:

  Object that specifies the targets for each feature. See the Targets
  format section for more information.

## Value

An updated [`problem()`](https://prioritizr.net/reference/problem.md)
object with the targets added to it.

## Details

This function is used to set targets for each feature (separately). For
problems associated with a single management zone, this function may be
useful to specify individual targets for each feature. For problems
associated with multiple management zones, this function can also be
used to specify a target for each feature within each zone (separately).
For example, this may be useful in planning exercises where it is
important to ensure that some of the features are adequately represented
by multiple zones. For example, in a marine spatial planning exercise,
it may be important for some features (e.g., commercial important fish
species) to be adequately represented by a conservation zone for
ensuring their long-term persistence, and also by a fishing zone to for
ensure food security. For greater flexibility in target setting (such as
setting targets that can be met through the allocation of multiple
zones), see the
[`add_manual_targets()`](https://prioritizr.net/reference/add_manual_targets.md)
function.

## Target setting

Many conservation planning problems require targets. Targets are used to
specify the minimum amount, or proportion, of a feature's spatial
distribution that should ideally be protected. This is important so that
the optimization process can weigh the merits and trade-offs between
improving the representation of one feature over another feature.
Although it can be challenging to set meaningful targets, this is a
critical step for ensuring that prioritizations meet the stakeholder
objectives that underpin a prioritization exercise (Carwardine *et al.*
2009). In other words, targets play an important role in ensuring that a
priority setting process is properly tuned according to stakeholder
requirements. For example, targets provide a mechanism for ensuring that
a prioritization secures enough habitat to promote the long-term
persistence of each threatened species, culturally important species, or
economically important ecosystem services under consideration. Since
there is often uncertainty regarding stakeholder objectives (e.g., how
much habitat should be protected for a given species) or the influence
of particular target on a prioritization (e.g., how would setting a 90%
or 100% for a threatened species alter priorities), it is often useful
to generate and compare a suite of prioritizations based on different
target scenarios.

## Targets format

The `targets` for a problem can be specified using the following
formats.

- `targets` as a `numeric` vector:

  containing target values for each feature. Additionally, for
  convenience, this format can be a single value to assign the same
  target to each feature. Note that this format cannot be used to
  specify targets for problems with multiple zones.

- `targets` as a `matrix` object:

  containing a target for each feature in each zone. Here, each row
  corresponds to a different feature in argument to `x`, each column
  corresponds to a different zone in argument to `x`, and each cell
  contains the target value for a given feature that the solution needs
  to secure in a given zone.

- `targets` as a `character` vector:

  containing the column name(s) in the feature data associated with the
  argument to `x` that contain targets. This format can only be used
  when the feature data associated with `x` is a
  [`sf::st_sf()`](https://r-spatial.github.io/sf/reference/sf.html) or
  `data.frame`. For problems that contain a single zone, the argument to
  `targets` must contain a single column name. Otherwise, for problems
  that contain multiple zones, the argument to `targets` must contain a
  column name for each zone.

## References

Carwardine J, Klein CJ, Wilson KA, Pressey RL, Possingham HP (2009)
Hitting the target and missing the point: target‐based conservation
planning in context. *Conservation Letters*, 2: 4–11.

## See also

Other functions for adding targets:
[`add_absolute_targets()`](https://prioritizr.net/reference/add_absolute_targets.md),
[`add_auto_targets()`](https://prioritizr.net/reference/add_auto_targets.md),
[`add_group_targets()`](https://prioritizr.net/reference/add_group_targets.md),
[`add_manual_targets()`](https://prioritizr.net/reference/add_manual_targets.md)

## Examples

``` r
# \dontrun{
# set seed for reproducibility
set.seed(500)

# load data
sim_pu_raster <- get_sim_pu_raster()
sim_features <- get_sim_features()
sim_zones_pu_raster <- get_sim_zones_pu_raster()
sim_zones_features <- get_sim_zones_features()

# create base problem
p <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# create problem with 10% targets
p1 <- p %>% add_relative_targets(0.1)

# create problem with varying targets for each feature
targets <- c(0.1, 0.2, 0.3, 0.4, 0.5)
p2 <- p %>% add_relative_targets(targets)

# solve problem
s3 <- c(solve(p1), solve(p2))
#> Error: Error 10009: HostID mismatch (licensed to 2890c3bc, hostid is d03f011a)
#> Timing stopped at: 0.003 0 0.012
names(s3) <- c("10% targets", "varying targets")
#> Error: object 's3' not found

# plot solution
plot(s3, axes = FALSE)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'plot': object 's3' not found

# create a problem with multiple management zones
p4 <-
  problem(sim_zones_pu_raster, sim_zones_features) %>%
  add_min_set_objective() %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# create a problem with targets that specify an equal amount of each feature
# to be represented in each zone
p4_targets <- matrix(
  0.1, nrow = 5, ncol = 3, dimnames = list(
    feature_names(sim_zones_features), zone_names(sim_zones_features)
  )
)
print(p4_targets)
#>           zone_1 zone_2 zone_3
#> feature_1    0.1    0.1    0.1
#> feature_2    0.1    0.1    0.1
#> feature_3    0.1    0.1    0.1
#> feature_4    0.1    0.1    0.1
#> feature_5    0.1    0.1    0.1

p5 <- p4 %>% add_relative_targets(p4_targets)

# solve problem
s5 <- solve(p5)
#> Error: Error 10009: HostID mismatch (licensed to 2890c3bc, hostid is d03f011a)
#> Timing stopped at: 0.004 0 0.014

# plot solution (cell values correspond to zone identifiers)
plot(category_layer(s5), main = "equal targets")
#> Error in (function (cond) .Internal(C_tryCatchHelper(addr, 1L, cond)))(structure(list(message = c(i = "In argument to `x`."),     trace = structure(list(call = list(base::tryCatch(base::withCallingHandlers({        NULL        base::saveRDS(base::do.call(base::do.call, base::c(base::readRDS("/tmp/Rtmp3PZNI5/callr-fun-512f27d8719f9"),             base::list(envir = .GlobalEnv, quote = TRUE)), envir = .GlobalEnv,             quote = TRUE), file = "/tmp/Rtmp3PZNI5/callr-res-512f257a0de91",             compress = FALSE)        base::flush(base::stdout())        base::flush(base::stderr())        NULL        base::invisible()    }, error = function(e) {        {            callr_data <- base::as.environment("tools:callr")$`__callr_data__`            err <- callr_data$err            if (FALSE) {                base::assign(".Traceback", base::.traceback(4),                   envir = callr_data)                utils::dump.frames("__callr_dump__")                base::assign(".Last.dump", .GlobalEnv$`__callr_dump__`,                   envir = callr_data)                base::rm("__callr_dump__", envir = .GlobalEnv)            }            e <- err$process_call(e)            e2 <- err$new_error("error in callr subprocess")            class <- base::class            class(e2) <- base::c("callr_remote_error", class(e2))            e2 <- err$add_trace_back(e2)            cut <- base::which(e2$trace$scope == "global")[1]            if (!base::is.na(cut)) {                e2$trace <- e2$trace[-(1:cut), ]            }            base::saveRDS(base::list("error", e2, e), file = base::paste0("/tmp/Rtmp3PZNI5/callr-res-512f257a0de91",                 ".error"))        }    }, interrupt = function(e) {        {            callr_data <- base::as.environment("tools:callr")$`__callr_data__`            err <- callr_data$err            if (FALSE) {                base::assign(".Traceback", base::.traceback(4),                   envir = callr_data)                utils::dump.frames("__callr_dump__")                base::assign(".Last.dump", .GlobalEnv$`__callr_dump__`,                   envir = callr_data)                base::rm("__callr_dump__", envir = .GlobalEnv)            }            e <- err$process_call(e)            e2 <- err$new_error("error in callr subprocess")            class <- base::class            class(e2) <- base::c("callr_remote_error", class(e2))            e2 <- err$add_trace_back(e2)            cut <- base::which(e2$trace$scope == "global")[1]            if (!base::is.na(cut)) {                e2$trace <- e2$trace[-(1:cut), ]            }            base::saveRDS(base::list("error", e2, e), file = base::paste0("/tmp/Rtmp3PZNI5/callr-res-512f257a0de91",                 ".error"))        }    }, callr_message = function(e) {        base::try(base::signalCondition(e))    }), error = function(e) {        NULL        if (FALSE) {            base::try(base::stop(e))        }        else {            base::invisible()        }    }, interrupt = function(e) {        NULL        if (FALSE) {            e        }        else {            base::invisible()        }    }), tryCatchList(expr, classes, parentenv, handlers), tryCatchOne(tryCatchList(expr,         names[-nh], parentenv, handlers[-nh]), names[nh], parentenv,         handlers[[nh]]), doTryCatch(return(expr), name, parentenv,         handler), tryCatchList(expr, names[-nh], parentenv, handlers[-nh]),         tryCatchOne(expr, names, parentenv, handlers[[1L]]),         doTryCatch(return(expr), name, parentenv, handler), base::withCallingHandlers({            NULL            base::saveRDS(base::do.call(base::do.call, base::c(base::readRDS("/tmp/Rtmp3PZNI5/callr-fun-512f27d8719f9"),                 base::list(envir = .GlobalEnv, quote = TRUE)),                 envir = .GlobalEnv, quote = TRUE), file = "/tmp/Rtmp3PZNI5/callr-res-512f257a0de91",                 compress = FALSE)            base::flush(base::stdout())            base::flush(base::stderr())            NULL            base::invisible()        }, error = function(e) {            {                callr_data <- base::as.environment("tools:callr")$`__callr_data__`                err <- callr_data$err                if (FALSE) {                  base::assign(".Traceback", base::.traceback(4),                     envir = callr_data)                  utils::dump.frames("__callr_dump__")                  base::assign(".Last.dump", .GlobalEnv$`__callr_dump__`,                     envir = callr_data)                  base::rm("__callr_dump__", envir = .GlobalEnv)                }                e <- err$process_call(e)                e2 <- err$new_error("error in callr subprocess")                class <- base::class                class(e2) <- base::c("callr_remote_error", class(e2))                e2 <- err$add_trace_back(e2)                cut <- base::which(e2$trace$scope == "global")[1]                if (!base::is.na(cut)) {                  e2$trace <- e2$trace[-(1:cut), ]                }                base::saveRDS(base::list("error", e2, e), file = base::paste0("/tmp/Rtmp3PZNI5/callr-res-512f257a0de91",                   ".error"))            }        }, interrupt = function(e) {            {                callr_data <- base::as.environment("tools:callr")$`__callr_data__`                err <- callr_data$err                if (FALSE) {                  base::assign(".Traceback", base::.traceback(4),                     envir = callr_data)                  utils::dump.frames("__callr_dump__")                  base::assign(".Last.dump", .GlobalEnv$`__callr_dump__`,                     envir = callr_data)                  base::rm("__callr_dump__", envir = .GlobalEnv)                }                e <- err$process_call(e)                e2 <- err$new_error("error in callr subprocess")                class <- base::class                class(e2) <- base::c("callr_remote_error", class(e2))                e2 <- err$add_trace_back(e2)                cut <- base::which(e2$trace$scope == "global")[1]                if (!base::is.na(cut)) {                  e2$trace <- e2$trace[-(1:cut), ]                }                base::saveRDS(base::list("error", e2, e), file = base::paste0("/tmp/Rtmp3PZNI5/callr-res-512f257a0de91",                   ".error"))            }        }, callr_message = function(e) {            base::try(base::signalCondition(e))        }), base::saveRDS(base::do.call(base::do.call, base::c(base::readRDS("/tmp/Rtmp3PZNI5/callr-fun-512f27d8719f9"),             base::list(envir = .GlobalEnv, quote = TRUE)), envir = .GlobalEnv,             quote = TRUE), file = "/tmp/Rtmp3PZNI5/callr-res-512f257a0de91",             compress = FALSE), base::do.call(base::do.call, base::c(base::readRDS("/tmp/Rtmp3PZNI5/callr-fun-512f27d8719f9"),             base::list(envir = .GlobalEnv, quote = TRUE)), envir = .GlobalEnv,             quote = TRUE), `<fn>`(base::quote(`<fn>`), base::quote(`<named list>`),             envir = base::quote(`<env>`), quote = base::quote(TRUE)),         `<fn>`(pkg = base::quote(`<pkgdown>`), examples = base::quote(TRUE),             run_dont_run = base::quote(TRUE), seed = base::quote(1014L),             lazy = base::quote(TRUE), override = base::quote(`<list>`),             install = base::quote(FALSE), preview = base::quote(FALSE),             new_process = base::quote(FALSE), devel = base::quote(FALSE),             quiet = base::quote(TRUE), cli_colors = base::quote(16777216L),             hyperlinks = base::quote(FALSE)), pkgdown::build_site(...),         build_site_local(pkg = pkg, examples = examples, run_dont_run = run_dont_run,             seed = seed, lazy = lazy, override = override, preview = preview,             devel = devel, quiet = quiet), build_reference(pkg,             lazy = lazy, examples = examples, run_dont_run = run_dont_run,             seed = seed, override = override, preview = FALSE,             devel = devel), unwrap_purrr_error(purrr::map(topics,             build_reference_topic, pkg = pkg, lazy = lazy, examples_env = examples_env,             run_dont_run = run_dont_run)), withCallingHandlers(code,             purrr_error_indexed = function(err) {                cnd_signal(err$parent)            }), purrr::map(topics, build_reference_topic, pkg = pkg,             lazy = lazy, examples_env = examples_env, run_dont_run = run_dont_run),         map_("list", .x, .f, ..., .progress = .progress), with_indexed_errors(i = i,             names = names, error_call = .purrr_error_call, call_with_cleanup(map_impl,                 environment(), .type, .progress, n, names, i)),         withCallingHandlers(expr, error = function(cnd) {            if (i == 0L) {            }            else {                message <- c(i = "In index: {i}.")                if (!is.null(names) && !is.na(names[[i]]) &&                   names[[i]] != "") {                  name <- names[[i]]                  message <- c(message, i = "With name: {name}.")                }                else {                  name <- NULL                }                cli::cli_abort(message, location = i, name = name,                   parent = cnd, call = error_call, class = "purrr_error_indexed")            }        }), call_with_cleanup(map_impl, environment(), .type,             .progress, n, names, i), .f(.x[[i]], ...), withCallingHandlers(data_reference_topic(topic,             pkg, examples_env = examples_env, run_dont_run = run_dont_run),             error = function(err) {                cli::cli_abort("Failed to parse Rd in {.file {topic$file_in}}",                   parent = err, call = quote(build_reference()))            }), data_reference_topic(topic, pkg, examples_env = examples_env,             run_dont_run = run_dont_run), run_examples(tags$tag_examples[[1]],             env = if (is.null(examples_env)) NULL else new.env(parent = examples_env),             topic = tools::file_path_sans_ext(topic$file_in),             run_dont_run = run_dont_run), highlight_examples(code,             topic, env = env), downlit::evaluate_and_highlight(code,             fig_save = fig_save_topic, env = eval_env, output_handler = handler),         evaluate::evaluate(code, child_env(env), new_device = TRUE,             output_handler = output_handler), withRestarts(with_handlers({            for (expr in tle$exprs) {                ev <- withVisible(eval(expr, envir))                watcher$capture_plot_and_output()                watcher$print_value(ev$value, ev$visible, envir)            }            TRUE        }, handlers), eval_continue = function() TRUE, eval_stop = function() FALSE),         withRestartList(expr, restarts), withOneRestart(withRestartList(expr,             restarts[-nr]), restarts[[nr]]), doWithOneRestart(return(expr),             restart), withRestartList(expr, restarts[-nr]), withOneRestart(expr,             restarts[[1L]]), doWithOneRestart(return(expr), restart),         with_handlers({            for (expr in tle$exprs) {                ev <- withVisible(eval(expr, envir))                watcher$capture_plot_and_output()                watcher$print_value(ev$value, ev$visible, envir)            }            TRUE        }, handlers), eval(call), eval(call), withCallingHandlers(code,             message = `<fn>`, warning = `<fn>`, error = `<fn>`),         withVisible(eval(expr, envir)), eval(expr, envir), eval(expr,             envir), plot(category_layer(s5), main = "equal targets"),         category_layer(s5), assert_required(x), cli::cli_abort(message = err_msg,             parent = attr(res, "condition")$parent, call = call,             .internal = .internal), rlang::abort(message, ...,             call = call, use_cli_format = TRUE, .frame = .frame)),         parent = c(0L, 1L, 2L, 3L, 2L, 5L, 6L, 0L, 0L, 0L, 0L,         0L, 12L, 13L, 14L, 15L, 16L, 15L, 18L, 19L, 20L, 19L,         19L, 23L, 23L, 25L, 26L, 27L, 28L, 29L, 30L, 31L, 32L,         31L, 34L, 35L, 29L, 37L, 38L, 37L, 29L, 29L, 42L, 43L,         43L, 45L, 46L, 47L), visible = c(TRUE, TRUE, TRUE, TRUE,         TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,         TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,         TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,         TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,         TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE), namespace = c("base",         "base", "base", "base", "base", "base", "base", "base",         "base", "base", "base", NA, "pkgdown", "pkgdown", "pkgdown",         "pkgdown", "base", "purrr", "purrr", "purrr", "base",         "purrr", "pkgdown", "base", "pkgdown", "pkgdown", "pkgdown",         "downlit", "evaluate", "base", "base", "base", "base",         "base", "base", "base", "evaluate", "base", "base", "base",         "base", "base", "base", "base", "prioritizr", "prioritizr",         "cli", "rlang"), scope = c("::", "local", "local", "local",         "local", "local", "local", "::", "::", "::", "local",         "global", "::", ":::", "::", ":::", "::", "::", ":::",         ":::", "::", ":::", "local", "::", ":::", ":::", ":::",         "::", "::", "::", "local", "local", "local", "local",         "local", "local", ":::", "::", "::", "::", "::", "::",         "::", "::", "::", ":::", "::", "::"), error_frame = c(FALSE,         FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,         FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,         FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,         FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,         FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,         FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)), row.names = c(NA,     -48L), version = 2L, class = c("rlang_trace", "rlib_trace",     "tbl", "data.frame")), parent = NULL, body = c("\033[1mCaused by error:\033[22m",     `!` = "object 's5' not found"), rlang = list(inherit = TRUE),     call = category_layer(s5), use_cli_format = TRUE), class = c("rlang_error", "error", "condition"))): error in evaluating the argument 'x' in selecting a method for function 'plot': ℹ In argument to `x`.
#> Caused by error:
#> ! object 's5' not found

# create a problem with targets that require a varying amount of each
# feature to be represented in each zone
p6_targets <- matrix(
  runif(15, 0.01, 0.2), nrow = 5, ncol = 3, dimnames = list(
    feature_names(sim_zones_features), zone_names(sim_zones_features)
  )
)
print(p6_targets)
#>               zone_1     zone_2     zone_3
#> feature_1 0.16838399 0.04908221 0.06359158
#> feature_2 0.14775224 0.10731456 0.17964011
#> feature_3 0.19530969 0.18583855 0.15529418
#> feature_4 0.09884473 0.16747797 0.04122594
#> feature_5 0.16433284 0.14519964 0.14909414

p6 <- p4 %>% add_relative_targets(p6_targets)

# solve problem
s6 <- solve(p6)
#> Error: Error 10009: HostID mismatch (licensed to 2890c3bc, hostid is d03f011a)
#> Timing stopped at: 0.001 0.001 0.012

# plot solution (cell values correspond to zone identifiers)
plot(category_layer(s6), main = "varying targets")
#> Error in (function (cond) .Internal(C_tryCatchHelper(addr, 1L, cond)))(structure(list(message = c(i = "In argument to `x`."),     trace = structure(list(call = list(base::tryCatch(base::withCallingHandlers({        NULL        base::saveRDS(base::do.call(base::do.call, base::c(base::readRDS("/tmp/Rtmp3PZNI5/callr-fun-512f27d8719f9"),             base::list(envir = .GlobalEnv, quote = TRUE)), envir = .GlobalEnv,             quote = TRUE), file = "/tmp/Rtmp3PZNI5/callr-res-512f257a0de91",             compress = FALSE)        base::flush(base::stdout())        base::flush(base::stderr())        NULL        base::invisible()    }, error = function(e) {        {            callr_data <- base::as.environment("tools:callr")$`__callr_data__`            err <- callr_data$err            if (FALSE) {                base::assign(".Traceback", base::.traceback(4),                   envir = callr_data)                utils::dump.frames("__callr_dump__")                base::assign(".Last.dump", .GlobalEnv$`__callr_dump__`,                   envir = callr_data)                base::rm("__callr_dump__", envir = .GlobalEnv)            }            e <- err$process_call(e)            e2 <- err$new_error("error in callr subprocess")            class <- base::class            class(e2) <- base::c("callr_remote_error", class(e2))            e2 <- err$add_trace_back(e2)            cut <- base::which(e2$trace$scope == "global")[1]            if (!base::is.na(cut)) {                e2$trace <- e2$trace[-(1:cut), ]            }            base::saveRDS(base::list("error", e2, e), file = base::paste0("/tmp/Rtmp3PZNI5/callr-res-512f257a0de91",                 ".error"))        }    }, interrupt = function(e) {        {            callr_data <- base::as.environment("tools:callr")$`__callr_data__`            err <- callr_data$err            if (FALSE) {                base::assign(".Traceback", base::.traceback(4),                   envir = callr_data)                utils::dump.frames("__callr_dump__")                base::assign(".Last.dump", .GlobalEnv$`__callr_dump__`,                   envir = callr_data)                base::rm("__callr_dump__", envir = .GlobalEnv)            }            e <- err$process_call(e)            e2 <- err$new_error("error in callr subprocess")            class <- base::class            class(e2) <- base::c("callr_remote_error", class(e2))            e2 <- err$add_trace_back(e2)            cut <- base::which(e2$trace$scope == "global")[1]            if (!base::is.na(cut)) {                e2$trace <- e2$trace[-(1:cut), ]            }            base::saveRDS(base::list("error", e2, e), file = base::paste0("/tmp/Rtmp3PZNI5/callr-res-512f257a0de91",                 ".error"))        }    }, callr_message = function(e) {        base::try(base::signalCondition(e))    }), error = function(e) {        NULL        if (FALSE) {            base::try(base::stop(e))        }        else {            base::invisible()        }    }, interrupt = function(e) {        NULL        if (FALSE) {            e        }        else {            base::invisible()        }    }), tryCatchList(expr, classes, parentenv, handlers), tryCatchOne(tryCatchList(expr,         names[-nh], parentenv, handlers[-nh]), names[nh], parentenv,         handlers[[nh]]), doTryCatch(return(expr), name, parentenv,         handler), tryCatchList(expr, names[-nh], parentenv, handlers[-nh]),         tryCatchOne(expr, names, parentenv, handlers[[1L]]),         doTryCatch(return(expr), name, parentenv, handler), base::withCallingHandlers({            NULL            base::saveRDS(base::do.call(base::do.call, base::c(base::readRDS("/tmp/Rtmp3PZNI5/callr-fun-512f27d8719f9"),                 base::list(envir = .GlobalEnv, quote = TRUE)),                 envir = .GlobalEnv, quote = TRUE), file = "/tmp/Rtmp3PZNI5/callr-res-512f257a0de91",                 compress = FALSE)            base::flush(base::stdout())            base::flush(base::stderr())            NULL            base::invisible()        }, error = function(e) {            {                callr_data <- base::as.environment("tools:callr")$`__callr_data__`                err <- callr_data$err                if (FALSE) {                  base::assign(".Traceback", base::.traceback(4),                     envir = callr_data)                  utils::dump.frames("__callr_dump__")                  base::assign(".Last.dump", .GlobalEnv$`__callr_dump__`,                     envir = callr_data)                  base::rm("__callr_dump__", envir = .GlobalEnv)                }                e <- err$process_call(e)                e2 <- err$new_error("error in callr subprocess")                class <- base::class                class(e2) <- base::c("callr_remote_error", class(e2))                e2 <- err$add_trace_back(e2)                cut <- base::which(e2$trace$scope == "global")[1]                if (!base::is.na(cut)) {                  e2$trace <- e2$trace[-(1:cut), ]                }                base::saveRDS(base::list("error", e2, e), file = base::paste0("/tmp/Rtmp3PZNI5/callr-res-512f257a0de91",                   ".error"))            }        }, interrupt = function(e) {            {                callr_data <- base::as.environment("tools:callr")$`__callr_data__`                err <- callr_data$err                if (FALSE) {                  base::assign(".Traceback", base::.traceback(4),                     envir = callr_data)                  utils::dump.frames("__callr_dump__")                  base::assign(".Last.dump", .GlobalEnv$`__callr_dump__`,                     envir = callr_data)                  base::rm("__callr_dump__", envir = .GlobalEnv)                }                e <- err$process_call(e)                e2 <- err$new_error("error in callr subprocess")                class <- base::class                class(e2) <- base::c("callr_remote_error", class(e2))                e2 <- err$add_trace_back(e2)                cut <- base::which(e2$trace$scope == "global")[1]                if (!base::is.na(cut)) {                  e2$trace <- e2$trace[-(1:cut), ]                }                base::saveRDS(base::list("error", e2, e), file = base::paste0("/tmp/Rtmp3PZNI5/callr-res-512f257a0de91",                   ".error"))            }        }, callr_message = function(e) {            base::try(base::signalCondition(e))        }), base::saveRDS(base::do.call(base::do.call, base::c(base::readRDS("/tmp/Rtmp3PZNI5/callr-fun-512f27d8719f9"),             base::list(envir = .GlobalEnv, quote = TRUE)), envir = .GlobalEnv,             quote = TRUE), file = "/tmp/Rtmp3PZNI5/callr-res-512f257a0de91",             compress = FALSE), base::do.call(base::do.call, base::c(base::readRDS("/tmp/Rtmp3PZNI5/callr-fun-512f27d8719f9"),             base::list(envir = .GlobalEnv, quote = TRUE)), envir = .GlobalEnv,             quote = TRUE), `<fn>`(base::quote(`<fn>`), base::quote(`<named list>`),             envir = base::quote(`<env>`), quote = base::quote(TRUE)),         `<fn>`(pkg = base::quote(`<pkgdown>`), examples = base::quote(TRUE),             run_dont_run = base::quote(TRUE), seed = base::quote(1014L),             lazy = base::quote(TRUE), override = base::quote(`<list>`),             install = base::quote(FALSE), preview = base::quote(FALSE),             new_process = base::quote(FALSE), devel = base::quote(FALSE),             quiet = base::quote(TRUE), cli_colors = base::quote(16777216L),             hyperlinks = base::quote(FALSE)), pkgdown::build_site(...),         build_site_local(pkg = pkg, examples = examples, run_dont_run = run_dont_run,             seed = seed, lazy = lazy, override = override, preview = preview,             devel = devel, quiet = quiet), build_reference(pkg,             lazy = lazy, examples = examples, run_dont_run = run_dont_run,             seed = seed, override = override, preview = FALSE,             devel = devel), unwrap_purrr_error(purrr::map(topics,             build_reference_topic, pkg = pkg, lazy = lazy, examples_env = examples_env,             run_dont_run = run_dont_run)), withCallingHandlers(code,             purrr_error_indexed = function(err) {                cnd_signal(err$parent)            }), purrr::map(topics, build_reference_topic, pkg = pkg,             lazy = lazy, examples_env = examples_env, run_dont_run = run_dont_run),         map_("list", .x, .f, ..., .progress = .progress), with_indexed_errors(i = i,             names = names, error_call = .purrr_error_call, call_with_cleanup(map_impl,                 environment(), .type, .progress, n, names, i)),         withCallingHandlers(expr, error = function(cnd) {            if (i == 0L) {            }            else {                message <- c(i = "In index: {i}.")                if (!is.null(names) && !is.na(names[[i]]) &&                   names[[i]] != "") {                  name <- names[[i]]                  message <- c(message, i = "With name: {name}.")                }                else {                  name <- NULL                }                cli::cli_abort(message, location = i, name = name,                   parent = cnd, call = error_call, class = "purrr_error_indexed")            }        }), call_with_cleanup(map_impl, environment(), .type,             .progress, n, names, i), .f(.x[[i]], ...), withCallingHandlers(data_reference_topic(topic,             pkg, examples_env = examples_env, run_dont_run = run_dont_run),             error = function(err) {                cli::cli_abort("Failed to parse Rd in {.file {topic$file_in}}",                   parent = err, call = quote(build_reference()))            }), data_reference_topic(topic, pkg, examples_env = examples_env,             run_dont_run = run_dont_run), run_examples(tags$tag_examples[[1]],             env = if (is.null(examples_env)) NULL else new.env(parent = examples_env),             topic = tools::file_path_sans_ext(topic$file_in),             run_dont_run = run_dont_run), highlight_examples(code,             topic, env = env), downlit::evaluate_and_highlight(code,             fig_save = fig_save_topic, env = eval_env, output_handler = handler),         evaluate::evaluate(code, child_env(env), new_device = TRUE,             output_handler = output_handler), withRestarts(with_handlers({            for (expr in tle$exprs) {                ev <- withVisible(eval(expr, envir))                watcher$capture_plot_and_output()                watcher$print_value(ev$value, ev$visible, envir)            }            TRUE        }, handlers), eval_continue = function() TRUE, eval_stop = function() FALSE),         withRestartList(expr, restarts), withOneRestart(withRestartList(expr,             restarts[-nr]), restarts[[nr]]), doWithOneRestart(return(expr),             restart), withRestartList(expr, restarts[-nr]), withOneRestart(expr,             restarts[[1L]]), doWithOneRestart(return(expr), restart),         with_handlers({            for (expr in tle$exprs) {                ev <- withVisible(eval(expr, envir))                watcher$capture_plot_and_output()                watcher$print_value(ev$value, ev$visible, envir)            }            TRUE        }, handlers), eval(call), eval(call), withCallingHandlers(code,             message = `<fn>`, warning = `<fn>`, error = `<fn>`),         withVisible(eval(expr, envir)), eval(expr, envir), eval(expr,             envir), plot(category_layer(s6), main = "varying targets"),         category_layer(s6), assert_required(x), cli::cli_abort(message = err_msg,             parent = attr(res, "condition")$parent, call = call,             .internal = .internal), rlang::abort(message, ...,             call = call, use_cli_format = TRUE, .frame = .frame)),         parent = c(0L, 1L, 2L, 3L, 2L, 5L, 6L, 0L, 0L, 0L, 0L,         0L, 12L, 13L, 14L, 15L, 16L, 15L, 18L, 19L, 20L, 19L,         19L, 23L, 23L, 25L, 26L, 27L, 28L, 29L, 30L, 31L, 32L,         31L, 34L, 35L, 29L, 37L, 38L, 37L, 29L, 29L, 42L, 43L,         43L, 45L, 46L, 47L), visible = c(TRUE, TRUE, TRUE, TRUE,         TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,         TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,         TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,         TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,         TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE), namespace = c("base",         "base", "base", "base", "base", "base", "base", "base",         "base", "base", "base", NA, "pkgdown", "pkgdown", "pkgdown",         "pkgdown", "base", "purrr", "purrr", "purrr", "base",         "purrr", "pkgdown", "base", "pkgdown", "pkgdown", "pkgdown",         "downlit", "evaluate", "base", "base", "base", "base",         "base", "base", "base", "evaluate", "base", "base", "base",         "base", "base", "base", "base", "prioritizr", "prioritizr",         "cli", "rlang"), scope = c("::", "local", "local", "local",         "local", "local", "local", "::", "::", "::", "local",         "global", "::", ":::", "::", ":::", "::", "::", ":::",         ":::", "::", ":::", "local", "::", ":::", ":::", ":::",         "::", "::", "::", "local", "local", "local", "local",         "local", "local", ":::", "::", "::", "::", "::", "::",         "::", "::", "::", ":::", "::", "::"), error_frame = c(FALSE,         FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,         FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,         FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,         FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,         FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,         FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)), row.names = c(NA,     -48L), version = 2L, class = c("rlang_trace", "rlib_trace",     "tbl", "data.frame")), parent = NULL, body = c("\033[1mCaused by error:\033[22m",     `!` = "object 's6' not found"), rlang = list(inherit = TRUE),     call = category_layer(s6), use_cli_format = TRUE), class = c("rlang_error", "error", "condition"))): error in evaluating the argument 'x' in selecting a method for function 'plot': ℹ In argument to `x`.
#> Caused by error:
#> ! object 's6' not found
# }
```
