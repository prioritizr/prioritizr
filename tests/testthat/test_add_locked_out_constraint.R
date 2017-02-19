context('add_locked_out_constraint')

test_that('integer locked out data', {
  # create problem
  data(sim_pu_raster, sim_features)
  p <- problem(sim_pu_raster, sim_features) %>%
    add_minimum_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decision() %>%
    add_locked_out_constraint(1:20)
  o <- compile(p)
  # check that constraints added correctly
  locked_out_cells <- 1:20
  locked_out_indices <- match(locked_out_cells, 
    raster::Which(!is.na(sim_pu_raster), cells=TRUE))
  locked_out_indices <- locked_out_indices[!is.na(locked_out_indices)]
  expect_true(isTRUE(all(o$ub()[locked_out_indices] == 0)))  
  expect_true(isTRUE(all(o$ub()[-locked_out_indices] == 1)))  
  # check that the solution obeys constraints as expected
  s <- solve(p)
  locked_out_units <- locked_out_cells[locked_out_cells %in% 
    raster::Which(!is.na(s), cells=TRUE)]
  expect_true(all(s[locked_out_units]==0))
  # invalid inputs
  p <- problem(sim_pu_raster, sim_features) %>%
    add_minimum_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decision()
  expect_error(p %>% add_locked_out_constraint(-1))
  expect_error(p %>% add_locked_out_constraint(9.6))
  expect_error(p %>% add_locked_out_constraint(raster::ncell(sim_pu_raster)+1))
})

test_that('character locked out data', {
  # create problem
  data(sim_pu_polygons, sim_features)
  p <- problem(sim_pu_polygons, sim_features) %>%
    add_minimum_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decision() %>%
    add_locked_out_constraint('locked_out')
  o <- compile(p)
  # check that constraints added correctly
  expect_true(isTRUE(all(o$ub()[which(sim_pu_polygons$locked_out)] == 0)))
  # check that the solution obeys constraints as expected
  s <- solve(p)
  expect_true(all(s$solution[which(sim_pu_polygons$locked_out)] == 0))
  # invalid inputs
  expect_error({
    sim_pu_polygons$locked_out <- as.integer(sim_pu_polygons$locked_out)
    problem(sim_pu_polygons, sim_features) %>%
        add_minimum_set_objective() %>%
        add_relative_targets(0.1) %>%
        add_binary_decision()  %>%
        add_locked_out_constraint('locked_out')})
  expect_error({
    problem(sim_pu_polygons, sim_features) %>%
        add_minimum_set_objective() %>%
        add_relative_targets(0.1) %>%
        add_binary_decision()  %>%
        add_locked_out_constraint(NA_character_)})
  expect_error({
    problem(sim_pu_polygons, sim_features) %>%
        add_minimum_set_objective() %>%
        add_relative_targets(0.1) %>%
        add_binary_decision()  %>%
        add_locked_out_constraint('column_name_that_doesnt_exist')})
  expect_error({
    problem(sim_pu_polygons, sim_features) %>%
        add_minimum_set_objective() %>%
        add_relative_targets(0.1) %>%
        add_binary_decision()  %>%
        add_locked_out_constraint('cost')})
})

test_that('raster locked out data', {
  # create problem
  data(sim_pu_raster, sim_locked_out_raster, sim_features)
  p <- problem(sim_pu_raster, sim_features) %>%
    add_minimum_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decision() %>%
    add_locked_out_constraint(sim_locked_out_raster)
  o <- compile(p)
  # check that constraints added correctly
  locked_out_cells <- raster::Which(sim_locked_out_raster &
    !is.na(sim_pu_raster), cells=TRUE)
  locked_out_indices <- match(locked_out_cells, 
    raster::Which(!is.na(sim_pu_raster), cells=TRUE))    
  expect_true(isTRUE(all(o$ub()[locked_out_indices]==0)))
  expect_true(isTRUE(all(o$ub()[-locked_out_indices]==1)))
  # check that the solution obeys constraints
  s <- solve(p)
  expect_true(all(s[locked_out_cells] == 0))
  # check that invalid inputs throw errors
  expect_error({
    data(sim_locked_out_raster)
    extent(sim_locked_out_raster) <- c(0,20,0,20)
    problem(sim_pu_raster, sim_features) %>%
        add_minimum_set_objective() %>%
        add_relative_targets(0.1) %>%
        add_binary_decision()  %>%
        add_locked_out_constraint(sim_locked_out_raster)})
  expect_error({
    data(sim_locked_out_raster)
    sim_locked_out_raster@crs <- sp::CRS('+init=epsg:4326')
    problem(sim_pu_raster, sim_features) %>%
        add_minimum_set_objective() %>%
        add_relative_targets(0.1) %>%
        add_binary_decision()  %>%
        add_locked_out_constraint(sim_locked_out_raster)})
  expect_error({
    data(sim_locked_out_raster)
    suppressWarnings(sim_locked_out_raster <- raster::setValues(
      sim_locked_out_raster, NA))
    problem(sim_pu_raster, sim_features) %>%
        add_minimum_set_objective() %>%
        add_relative_targets(0.1) %>%
        add_binary_decision()  %>%
        add_locked_out_constraint(sim_locked_out_raster)})
  expect_error({
    data(sim_locked_out_raster)
    sim_locked_out_raster <- raster::setValues(sim_locked_out_raster, 0)
    problem(sim_pu_raster, sim_features) %>%
        add_minimum_set_objective() %>%
        add_relative_targets(0.1) %>%
        add_binary_decision()  %>%
        add_locked_out_constraint(sim_locked_out_raster)})
})
 
test_that('spatial locked out data', {
  # create problem
  data(sim_pu_polygons, sim_features)
  p <- problem(sim_pu_polygons, sim_features) %>%
    add_minimum_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_binary_decision() %>%
    add_locked_out_constraint(sim_pu_polygons[sim_pu_polygons$locked_out,])
  o <- compile(p)
  # check that constraints added correctly
  locked_out_units <- which(sim_pu_polygons$locked_out)
  expect_true(isTRUE(all(o$ub()[locked_out_units]==0)))
  expect_true(isTRUE(all(o$ub()[-locked_out_units]==1)))
  # check that the solution obeys constraints
  s <- solve(p)
  expect_true(all(s$solution[locked_out_units]==0))
  # check that invalid inputs throw errors
  expect_error({
    data(sim_pu_polygons, sim_features)
    problem(sim_pu_polygons[1:10,], sim_features) %>%
        add_minimum_set_objective() %>%
        add_relative_targets(0.1) %>%
        add_binary_decision()  %>%
        add_locked_out_constraint(sim_pu_polygons[50:55,])})
  expect_error({
    data(sim_pu_polygons, sim_features)
    problem(sim_pu_polygons[1:10,], sim_features) %>%
        add_minimum_set_objective() %>%
        add_relative_targets(0.1) %>%
        add_binary_decision()  %>%
        add_locked_out_constraint(sim_pu_polygons[0,])})
  expect_error({
    data(sim_pu_polygons, sim_features)
    sim_pu_polygons2 <- sim_pu_polygons[1:10,]
    sim_pu_polygons2@proj4string <- sp::CRS('+init=epsg:4326')
    problem(sim_pu_polygons, sim_features) %>%
        add_minimum_set_objective() %>%
        add_relative_targets(0.1) %>%
        add_binary_decision()  %>%
        add_locked_out_constraint(sim_pu_polygons2)})
})

