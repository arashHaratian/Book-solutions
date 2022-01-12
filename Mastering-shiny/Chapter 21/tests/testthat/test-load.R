test_that("load_file() handles all input types", {
  # Create sample data
  df <- tibble::tibble(x = 1, y = 2)
  path_csv <- tempfile()
  path_tsv <- tempfile()
  write.csv(df, path_csv, row.names = FALSE)
  write.table(df, path_tsv, sep = "\t", row.names = FALSE)
  
  expect_equal(load_file("test.csv", path_csv), df)
  expect_equal(load_file("test.tsv", path_tsv), df)
  expect_error(load_file("blah", path_csv), "Invalid file")
})
