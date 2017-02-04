
.pkgenv <- new.env(parent=emptyenv())

.onAttach <- function(libname, pkgname) {
  if (!'gurobi' %in% rownames(installed.packages()))
    packageStartupMessage('warning: the "gurobi" R package is not installed.',
                          'Once installed, gurobi can be used solve problems very quickly.',
                          'Academics can obtain a license at no cost.',
                          'See here for instructions: www.gurobi.com/documentation')
}

.onUnload <- function(libpath) {
  library.dynam.unload('prioritizr', libpath)
}
