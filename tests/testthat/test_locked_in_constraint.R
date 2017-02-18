context('locked in constraint')

test_that('integer locked in data', {
  # create problem
  data(sim_pu_raster, sim_features)
  p <- problem(sim_pu_raster, sim_features) %>%
    add_minimum_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decision() %>%
    add_locked_in_constraint(seq_len(raster::ncell(sim_pu_raster)))
  o <- compile(p)
  # check that constraints added correctly
  expect_true(isTRUE(all(o$lb()[seq_len(p$number_of_planning_units())] == 1)))
  # check that the solution obeys constraints as expected
  s <- solve(p)
  expect_true(all(raster::Which(is.na(s), cells=TRUE) == 
      raster::Which(is.na(sim_pu_raster), cells=TRUE)))
  expect_true(isTRUE(all(raster::values(s)[raster::Which(!is.na(s),
    cells=TRUE)] == 1)))
  # invalid inputs
  p <- problem(sim_pu_raster, sim_features) %>%
    add_minimum_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decision()
  expect_error(p %>% add_locked_in_constraint(-1))
  expect_error(p %>% add_locked_in_constraint(9.6))
  expect_error(p %>% add_locked_in_constraint(raster::ncell(sim_pu_raster)+1))
})

test_that('character locked in data', {
  # create problem
  data(sim_pu_polygons, sim_features)
  p <- problem(sim_pu_polygons, sim_features) %>%
    add_minimum_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decision() %>%
    add_locked_in_constraint('locked_in')
  o <- compile(p)
  # check that constraints added correctly
  expect_true(isTRUE(all(o$lb()[which(sim_pu_polygons$locked_in)] == 1)))
  # check that the solution obeys constraints as expected
  s <- solve(p)
  expect_true(all(s$solution[which(sim_pu_polygons$locked_in)] == 1))
  # invalid inputs
  expect_error({
    sim_pu_polygons$locked_in <- as.integer(sim_pu_polygons$locked_in)
    problem(sim_pu_polygons, sim_features) %>%
        add_minimum_set_objective() %>%
        add_relative_targets(0.1) %>%
        add_binary_decision()  %>%
        add_locked_in_constraint('locked_in')})
  expect_error({
    problem(sim_pu_polygons, sim_features) %>%
        add_minimum_set_objective() %>%
        add_relative_targets(0.1) %>%
        add_binary_decision()  %>%
        add_locked_in_constraint(NA_character_)})
  expect_error({
    problem(sim_pu_polygons, sim_features) %>%
        add_minimum_set_objective() %>%
        add_relative_targets(0.1) %>%
        add_binary_decision()  %>%
        add_locked_in_constraint('column_name_that_doesnt_exist')})
  expect_error({
    problem(sim_pu_polygons, sim_features) %>%
        add_minimum_set_objective() %>%
        add_relative_targets(0.1) %>%
        add_binary_decision()  %>%
        add_locked_in_constraint('cost')})
})

test_that('raster locked in data', {
  # create problem
  data(sim_pu_raster, sim_locked_in_raster, sim_features)
  p <- problem(sim_pu_raster, sim_features) %>%
    add_minimum_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decision() %>%
    add_locked_in_constraint(sim_locked_in_raster)
  o <- compile(p)
  # check that constraints added correctly
  locked_in_cells <- raster::Which(sim_locked_in_raster & !is.na(sim_pu_raster),
    cells=TRUE)
  locked_in_indices <- match(locked_in_cells, 
    raster::Which(!is.na(sim_pu_raster), cells=TRUE))    
  expect_true(isTRUE(all(o$lb()[locked_in_indices]==1)))
  expect_true(isTRUE(all(o$lb()[-locked_in_indices]==0)))
  # check that the solution obeys constraints
  s <- solve(p)
  expect_true(all(s[locked_in_cells] == 1))
  # check that invalid inputs throw errors
  expect_error({
    data(sim_locked_in_raster)
    extent(sim_locked_in_raster) <- c(0,20,0,20)
    problem(sim_pu_raster, sim_features) %>%
        add_minimum_set_objective() %>%
        add_relative_targets(0.1) %>%
        add_binary_decision()  %>%
        add_locked_in_constraint(sim_locked_in_raster)})
  expect_error({
    data(sim_locked_in_raster)
    sim_locked_in_raster@crs <- sp::CRS('+init=epsg:4326')
    problem(sim_pu_raster, sim_features) %>%
        add_minimum_set_objective() %>%
        add_relative_targets(0.1) %>%
        add_binary_decision()  %>%
        add_locked_in_constraint(sim_locked_in_raster)})
  expect_error({
    data(sim_locked_in_raster)
    suppressWarnings(sim_locked_in_raster <- raster::setValues(
      sim_locked_in_raster, NA))
    problem(sim_pu_raster, sim_features) %>%
        add_minimum_set_objective() %>%
        add_relative_targets(0.1) %>%
        add_binary_decision()  %>%
        add_locked_in_constraint(sim_locked_in_raster)})
  expect_error({
    data(sim_locked_in_raster)
    sim_locked_in_raster <- raster::setValues(sim_locked_in_raster, 0)
    problem(sim_pu_raster, sim_features) %>%
        add_minimum_set_objective() %>%
        add_relative_targets(0.1) %>%
        add_binary_decision()  %>%
        add_locked_in_constraint(sim_locked_in_raster)})
})
 
test_that('spatial locked in data', {
  # create problem
  data(sim_pu_polygons, sim_features)
  p <- problem(sim_pu_polygons, sim_features) %>%
    add_minimum_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decision() %>%
    add_locked_in_constraint(sim_pu_polygons[sim_pu_polygons$locked_in,])
  o <- compile(p)
  # check that constraints added correctly
  locked_in_units <- which(sim_pu_polygons$locked_in)
  expect_true(isTRUE(all(o$lb()[locked_in_units]==1)))
  expect_true(isTRUE(all(o$lb()[-locked_in_units]==0)))
  # check that the solution obeys constraints
  s <- solve(p)
  expect_true(all(s$solution[locked_in_units]==1))
  # check that invalid inputs throw errors
  expect_error({
    data(sim_pu_polygons, sim_features)
    problem(sim_pu_polygons[1:10,], sim_features) %>%
        add_minimum_set_objective() %>%
        add_relative_targets(0.1) %>%
        add_binary_decision()  %>%
        add_locked_in_constraint(sim_pu_polygons[50:55,])})
  expect_error({
    data(sim_pu_polygons, sim_features)
    problem(sim_pu_polygons[1:10,], sim_features) %>%
        add_minimum_set_objective() %>%
        add_relative_targets(0.1) %>%
        add_binary_decision()  %>%
        add_locked_in_constraint(sim_pu_polygons[0,])})
  expect_error({
    data(sim_pu_polygons, sim_features)
    sim_pu_polygons2 <- sim_pu_polygons[1:10,]
    sim_pu_polygons2@proj4string <- sp::CRS('+init=epsg:4326')
    problem(sim_pu_polygons, sim_features) %>%
        add_minimum_set_objective() %>%
        add_relative_targets(0.1) %>%
        add_binary_decision()  %>%
        add_locked_in_constraint(sim_pu_polygons2)})
})

