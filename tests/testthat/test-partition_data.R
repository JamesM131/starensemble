context("test-partition_data")


test_that("Planes are evenly distributed", {
  expect_error(mark_planes(3))

  # Minimum number of planes
  expect_type(mark_planes(4), "list")
  expect_equal(length(mark_planes(4)), 4)

  # Basic Plane description


  # a[[1]]$line
})
