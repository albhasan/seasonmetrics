test_that("blank_raster works", {
  res <-
    blank_raster(
      grid_cells = c(rows = 90, cols = 90),
      xy_min = c(0, 0),
      xy_max = c(90, 90),
      grid_crs = "4326"
    )
  expect_equal(
    object = terra::res(res),
    expected = c(1, 1)
  )
  expect_equal(
    object = terra::size(res),
    expected = prod(dim(res))
  )
})
