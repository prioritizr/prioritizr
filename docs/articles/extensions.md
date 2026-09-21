# Extensions

## Introduction

A myriad of software is available to aid with systematic conservation
planning. These software include *R* packages that can enhance the
functionality of the *prioritizr R* package. For example, such *R*
packages can be used to prepare data for generating prioritizations,
enhance problem formulations, streamline workflows, and improve
accessibility. Although not an exhaustive list, below are some notable
examples that may be of interest.

## Packages

### Packages to enhance problem formulation

- [*moec.prioritizr*](https://aboozarm.github.io/moec.prioritizr/)
  provides the \epsilon-constraint approach for multi-objective
  optimization (Eichfelder 2008). This approach can be used to
  automatically generate multiple solutions to characterize the full
  range of trade-offs between multiple objectives.
- [*MultiscaleSCP*](https://cran.r-project.org/package=MultiscaleSCP)
  provides penalties and functions to support prioritization at multiple
  scales, including cross-scale connectivity. By leveraging the H3
  hierarchical hexagonal grid system (Uber Technologies, Inc. 2024), it
  enables the formulation and optimization of conservation problems
  across nested resolutions – based on parent-child relationships – with
  resolution-specific features, costs, and management attributes.
  Additionally, it provides functions to evaluate solutions using
  multiscale-aware diagnostics and to post-process optimization outputs
  into alternative area-targeted conservation scenarios.
- [*patchwise*](https://emlab-ucsb.github.io/patchwise/) provides
  functions to account for biodiversity features that need to
  represented by selecting planning units that form a contiguous patch.
  For example, a seamount can encompass multiple planning units and it
  might be desirable to ensure that entire seamounts are protected
  rather than just a portion of several seamounts. This package provides
  the functionality to represent whole seamounts, and ensure that
  representation targets are met by representing whole patches of
  features.
- [*priorCON*](https://cadam00.github.io/priorCON/) provides functions
  to account for areas with high ecological connectivity. By leveraging
  graph community detection methods, this package focuses on
  representing clusters of features that exhibit strong ecological
  linkages. It provides functions to help prepare connectivity data,
  generate prioritizations based on different scenarios for
  connectivity, and evaluate prioritizations.
- [*prior3D*](https://cadam00.github.io/prior3D/) provides functions to
  account for three dimensions in prioritization analyses (Doxa *et al.*
  2025). It provides a structured workflow to prepare data, generate
  prioritizations, and evaluate prioritizations based on planning units
  and features that span multiple depth levels.
- [*robust.prioritizr*](https://frankiecho.github.io/robust.prioritizr/)
  provides objectives and constraints to generate prioritizations that
  account of uncertainty in the expected outcome and constraints
  associated with implementing conservation actions. For example, it can
  be used to account for uncertainty in climate change scenario
  projections, species distribution models, ecosystem service models,
  and measurement errors when generating prioritizations. It uses robust
  optimization techniques based on the chance-constrained programming
  and the Conditional Value-at-Risk (CVaR) problems (Charnes & Cooper
  1959; Rockafellar & Uryasev 2000).

### Packages to streamline workflows and access data

- [*oceandatr*](https://emlab-ucsb.github.io/oceandatr/) provides
  functions to acquiring, processing and gridding ocean data (Flower *et
  al.* 2026). It provides access to a broad range of data sources,
  including bathymetric, geomorphologic, ecological, and human use data.
  It also provides functions to prepare these data as spatial grids so
  that they can be used to define costs or features for prioritization.
- [*spatialplanr*](https://spatialplanning.github.io/spatialplanr/)
  provides functions to streamline and enhance spatial conservation
  prioritization efforts. For example, it provides functions to aid with
  generating planning units, obtaining data (e.g., climate layers and
  fishing data), integrating climate change considerations (based on
  Buenafe *et al.* 2023), assigning representation targets, and
  visualizing the performance of prioritizations.
- [*wdpar*](https://prioritizr.github.io/wdpar/) provides functions to
  obtain and prepare data for conserved areas in conservation planning
  analyses (Hanson 2022). In particular, provides automated routines to
  obtain data from the [World Database on Protected Areas (WDPA) and the
  World Database on Other Effective Area-Based Conservation Measures
  (WDOECM)](https://www.protectedplanet.net/en) and then clean them
  following best practices (e.g., repairing invalid geometries,
  excluding boundaries for protected areas that have not been
  implemented, and accounting for spatial overlaps).

### Packages to improve accessibility

- [*shinyplanr*](https://spatialplanning.github.io/shinyplanr/) provides
  a ready-to-deploy web application for spatial conservation planning.
  It gives ecologists, planners, and stakeholders an accessible
  browser-based interface for running spatial prioritization analyses—no
  *R* experience required!
- [*wheretowork*](https://ncc-cnc.github.io/wheretowork/) provides a
  web-based application for solving complex conservation scenarios using
  concepts of systematic conservation planning. It offers a user
  friendly interface for developing conservation scenarios.

## References

Buenafe, K.C.V., Dunn, D.C., Everett, J.D., Brito‐Morales, I., Schoeman,
D.S., Hanson, J.O., Dabalà, A., Neubert, S., Cannicci, S., Kaschner, K.
& Richardson, A.J. (2023). A metric‐based framework for climate‐smart
conservation planning. *Ecological Applications*, *33*, e2852.

Charnes, A. & Cooper, W.W. (1959). Chance-constrained programming.
*Management Science*, *6*, 73–79.

Doxa, A., Adam, C., Nagkoulis, N., Mazaris, A.D. & Katsanevakis, S.
(2025). prior3D: An R package for three-dimensional conservation
prioritization. *Ecological Modelling*, *499*, 110919.

Eichfelder, G. (2008). *Adaptive scalarization methods in multiobjective
optimization*. Springer, Berlin, Heidelberg.

Flower, J., Burns, E.S., Dunn, D.C., Estep, A., Everett, J.D., Hanson,
J.O., Lester, S.E. & Richardson, A.J. (2026). oceandatr: An R package to
acquire and process geospatial ocean data. *Ecology and Evolution*,
*16*.

Hanson, J.O. (2022). wdpar: Interface to the World Database on Protected
Areas. *Journal of Open Source Software*, *7*, 4594.

Rockafellar, R.T. & Uryasev, S. (2000). Optimization of conditional
value-at-risk. *The Journal of Risk*, *2*, 21–41.

Uber Technologies, Inc. (2024). H3: A hexagonal hierarchical geospatial
indexing system. Available at https://h3geo.org/.
