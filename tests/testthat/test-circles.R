context("Test circles")

test_that("The circle functions return errors on incorrect input", {
  expect_error(intersect_circles(4, 4, -2))
  expect_error(intersect_circles(FALSE, 2, 1))
  expect_error(intersect_circles(2, 2, 2, "yes"))
  expect_error(intersect_circles(c(1, 2), 1, c(1, 2)))
  expect_error(intersect_circles(c(1, 1), c(0.5, 0.5), 1, c(TRUE, FALSE)))
  expect_warning(intersect_circles(c(1, 1), c(0.5, 0.5), c(1, 2)))
  expect_error(intersect_circles(c(0, 1), c(0, 0), c(0.5, 0.5)), NA)

  expect_error(two_disk_overlap(r1, r2, d = NULL, x1 = NULL, x2 = NULL,
                                y1 = NULL, y2 = NULL))
  expect_error(two_disk_overlap(0, r2, d = NULL, x1 = NULL, x2 = NULL,
                                y1 = NULL, y2 = NULL))
  expect_error(two_disk_overlap(r1, r2, d = NULL, x1 = 2, x2 = 2,
                                y1 = NULL, y2 = NULL))
  expect_error(two_disk_overlap(r1, r2, d = NULL, x1 = NULL, x2 = NULL,
                                y1 = NULL, y2 = NULL))
  expect_error(two_disk_overlap(1, 2, d = NULL, x1 = NULL, x2 = NULL,
                                y1 = NULL, y2 = NULL))
  expect_error(two_disk_overlap(c(1, 1), 1, d = 2, x1 = NULL, x2 = NULL,
                                y1 = NULL, y2 = NULL))
  expect_error(two_disk_overlap(r1, r2, d = c(1, 2), x1 = 1:3, x2 = NULL,
                                y1 = NULL, y2 = NULL))
  expect_error(two_disk_overlap(r1, r2, d = NULL, x1 = NULL, x2 = NULL,
                                y1 = NULL, y2 = "asdf"))
  expect_error(two_disk_overlap("hi", 2, d = NULL, x1 = NULL, x2 = NULL,
                                y1 = NULL, y2 = NULL))
  expect_error(two_disk_overlap(1, -1, d = NULL, x1 = NULL, x2 = NULL,
                                y1 = NULL, y2 = NULL, warnings = TRUE))
  expect_error(two_disk_overlap(1, 1, d = -1, x1 = NULL, x2 = NULL,
                                y1 = NULL, y2 = NULL, warnings = TRUE))
  expect_error(two_disk_overlap(2, 2, d = 1, x1 = NULL, x2 = NULL,
                                y1 = NULL, y2 = NULL), NA)
  expect_error(two_disk_overlap(0, 2, d = 1, x1 = NULL, x2 = NULL,
                                y1 = NULL, y2 = NULL))
})
