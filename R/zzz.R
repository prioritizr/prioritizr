.pkgenv <- new.env(parent = emptyenv())

# register knitr print methods
# see https://cran.r-project.org/web/packages/knitr/vignettes/knit_print.html
register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg), length(pkg) == 1)
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}

.onLoad <- function(...) {
  register_s3_method("knitr", "knit_print", "ConservationProblem")
  .pkgenv[["missing_error_call"]] <- deparse(
    attr(
      eval(
        expression(try(identical(x, 1), silent = TRUE)),
        envir = baseenv()
      ),
      "condition"
    )$call
  )
}

.onAttach <- function(libname, pkgname) {
  # define message generator function
  msg <- function() {
    packageStartupMessage(paste(rep("-", 30), collapse = ""))
    packageStartupMessage(
      "You have loaded both oppr and prioritizr - ",
      "this is likely to cause serious issues.\n",
      "You should only have one of these packages loaded at a time,\n",
      "please unload the oppr or prioritizr using one of the commands below:\n",
      "  detach(\"package:oppr\", unload = TRUE) # unload oppr package\n",
      "  detach(\"package:prioritizr\", unload = TRUE) # unload prioritizr",
      "package\n",
      "and then reload the desired package."
    )
    packageStartupMessage(paste(rep("-", 30), collapse = ""))
  }
  # print message if prioritizr already loaded
  if ("oppr" %in% .packages())
   msg()
  # set hook to print message if oppr is loaded later on
  setHook(packageEvent("oppr", "attach"), function(...) {
    msg()
  })
}

.onUnload <- function(libpath) {
  library.dynam.unload("prioritizr", libpath)
}
