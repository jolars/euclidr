context("Test circles")

test_that("The circle functions return errors on incorrect input", {
  expect_error(intersect_circles(4, 4, -2))
  expect_error(intersect_circles(FALSE, 2, 1))
  expect_error(intersect_circles(2, 2, 2, "yes"))
  expect_error(intersect_circles(c(1, 2), 1, c(1, 2)))
  expect_error(intersect_circles(1, 1, 1, c(TRUE, FALSE)))

  two_disk_overlap

  expect_error(two_disk_overlap(4, 4, -2))
  expect_error(two_disk_overlap(FALSE, 2, 1))
  expect_error(two_disk_overlap(2, 2, 2, "yes"))
  expect_error(two_disk_overlap(c(1, 2), 1, c(1, 2)))
  expect_error(two_disk_overlap(1, 1, 1, c(TRUE, FALSE)))
})
