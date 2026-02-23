test_that("all_comparable_problem (single zone, TRUE)", {
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  
  p1 <- problem(sim_pu_raster, sim_features[[1:3]]) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decisions()
  
  p2 <- problem(sim_pu_raster, sim_features[[4:5]]) %>%
    add_min_shortfall_objective(
      budget = 0.2 * terra::global(sim_pu_raster, sum, na.rm = TRUE)[[1]]) %>%
    add_relative_targets(0.2) %>%
    add_binary_decisions()
  
  expect_true(all_comparable_problem(p1, p2))
  expect_no_failure(multi_problem(p1, p2))
})

test_that(
  "all_comparable_problem (single zone, different planning unit class)", {
    sim_pu_raster <- get_sim_pu_raster()
    sim_features <- get_sim_features()
    
    p1 <- problem(sim_pu_raster, sim_features[[1:3]]) %>%
      add_min_set_objective() %>%
      add_relative_targets(0.1) %>%
      add_binary_decisions()
    
    pu2 <- data.frame(
      id = seq(20,29), cost = c(runif(1), NA, runif(8)),
      spp1 = runif(10), spp2 = c(rpois(9, 4), NA)
    )
    
    p2 <- problem(pu2, c("spp1", "spp2"), "cost")
    
    expect_false(all_comparable_problem(p1, p2))
    expect_error(
      multi_problem(p1, p2),
      regexp = "planning unit classes"
    ) 
})

test_that(
  "all_comparable_problem (single zone, different planning unit ids)", {
  
    # create data
    pu <- data.frame(
      id = seq_len(10), cost = c(runif(1), NA, runif(8)),
      spp1 = runif(10), spp2 = c(rpois(9, 4), NA)
    )
    
    # total unit ids mismatch
    pu2 <- data.frame(
      id = seq(20,28), cost = c(runif(1), NA, runif(7)),
      spp1 = runif(9), spp2 = c(rpois(8, 4), NA)
    )
    
    # create problems
    p1 <- problem(pu, c("spp1", "spp2"), "cost")
    p2 <- problem(pu2, c("spp1", "spp2"), "cost")
    
    expect_false(all_comparable_problem(p1, p2))
    expect_error(
      multi_problem(p1, p2) ,
      regexp = "planning unit indices"
    )
})

test_that(
  "all_comparable_problem (multiple zones, TRUE)", {
  
    sim_zones_pu_raster <- get_sim_zones_pu_raster()
    sim_zones_features <- get_sim_zones_features()
    
    p1 <- problem(sim_zones_pu_raster, sim_zones_features) %>%
      add_min_set_objective() %>%
      add_relative_targets(matrix(0.2, ncol = 3, nrow = 5)) %>%
      add_binary_decisions()
    
    p2 <- problem(sim_zones_pu_raster, sim_zones_features) %>%
      add_min_set_objective() %>%
      add_relative_targets(matrix(0.1, ncol = 3, nrow = 5)) %>%
      add_binary_decisions()
    
    expect_true(all_comparable_problem(p1, p2))
    expect_no_failure(multi_problem(p1, p2))
})

test_that(
  "all_comparable_problem (multiple zones, different planning unit class)", {
  
    sim_zones_pu_raster <- get_sim_zones_pu_raster()
    sim_zones_features <- get_sim_zones_features()
    
    p1 <- problem(sim_zones_pu_raster, sim_zones_features) %>%
      add_min_set_objective() %>%
      add_relative_targets(matrix(0.2, ncol = 3, nrow = 5)) %>%
      add_binary_decisions()
    
    pu2 <- data.frame(
      id = seq_len(9),
      cost_1 = c(NA, NA, runif(7)), cost_2 = c(0.3, NA, runif(7)),
      spp1_1 = runif(9), spp2_1 = c(rpois(8, 4), NA),
      spp1_2 = runif(9), spp2_2 = runif(9)
    )
    
    p2 <- problem(
      pu2, zones(c("spp1_1", "spp2_1"), c("spp1_2", "spp2_2")),
      c("cost_1", "cost_2")
    )
    
    expect_false(all_comparable_problem(p1, p2))
    expect_error(
      multi_problem(p1, p2),
      regexp = "planning unit class"
    )
})

test_that("all_comparable_problem (multiple zones, different zone names)", {
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  
  p1 <- problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(matrix(0.2, ncol = 3, nrow = 5)) %>%
    add_binary_decisions()
  
  attr(sim_zones_features, "zone_names") <- c("zone1", "zone2", "zone3")
  
  p2 <- problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(matrix(0.1, ncol = 3, nrow = 5)) %>%
    add_binary_decisions()
  
  expect_false(all_comparable_problem(p1, p2))
  expect_error(
    multi_problem(p1, p2),
    regexp = "zone names"
  )
})

test_that(
  "all_comparable_problem (multiple zones, different planning unit ids)", {
    # create data
    pu1 <- data.frame(
      id = seq_len(10),
      cost_1 = c(NA, NA, runif(8)), cost_2 = c(0.3, NA, runif(8)),
      spp1_1 = runif(10), spp2_1 = c(rpois(9, 4), NA),
      spp1_2 = runif(10), spp2_2 = runif(10)
    )
    
    pu2 <- data.frame(
      id = seq_len(9),
      cost_1 = c(NA, NA, runif(7)), cost_2 = c(0.3, NA, runif(7)),
      spp1_1 = runif(9), spp2_1 = c(rpois(8, 4), NA),
      spp1_2 = runif(9), spp2_2 = runif(9)
    )
    
    # create problem
    p1 <- problem(
      pu1, zones(c("spp1_1", "spp2_1"), c("spp1_2", "spp2_2")),
      c("cost_1", "cost_2")
    )
    
    p2 <- problem(
      pu2, zones(c("spp1_1", "spp2_1"), c("spp1_2", "spp2_2")),
      c("cost_1", "cost_2")
    )
    
    expect_false(all_comparable_problem(p1, p2))
    expect_error(
      multi_problem(p1, p2),
      regexp = "planning unit indices"
    )
})

test_that("assert_pass_presolve_check (TRUE)", {
  stop("TODO")
})

test_that("assert_pass_presolve_check (error)", {
  stop("TODO")
})

test_that("verify_pass_presolve_check (TRUE)", {
  stop("TODO")
})

test_that("verify_pass_presolve_check (error)", {
  stop("TODO")
})
