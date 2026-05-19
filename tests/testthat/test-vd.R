make_fake_vd <- function(dir, capture_path, input_path_log) {
  cmd <- file.path(dir, "vd")
  writeLines(
    c(
      "#!/bin/sh",
      "printf '%s' \"$1\" > \"$LEO_VD_INPUT_PATH\"",
      "cp \"$1\" \"$LEO_VD_CAPTURE_PATH\""
    ),
    cmd
  )
  Sys.chmod(cmd, "755")
  Sys.setenv(LEO_VD_CAPTURE_PATH = capture_path, LEO_VD_INPUT_PATH = input_path_log)
  cmd
}

test_that("vd prints a compact summary table with closing rule", {
  tmp_dir <- tempfile("leo_vd_test_")
  dir.create(tmp_dir)
  capture_path <- file.path(tmp_dir, "captured.tsv")
  input_path_log <- file.path(tmp_dir, "input_path.txt")
  fake_vd <- make_fake_vd(tmp_dir, capture_path, input_path_log)

  old_opt <- getOption("leo.basic.vd_cmd")
  old_capture <- Sys.getenv("LEO_VD_CAPTURE_PATH", unset = NA_character_)
  old_input <- Sys.getenv("LEO_VD_INPUT_PATH", unset = NA_character_)
  on.exit(options(leo.basic.vd_cmd = old_opt), add = TRUE)
  on.exit(do.call(Sys.setenv, as.list(stats::setNames(c(old_capture, old_input), c("LEO_VD_CAPTURE_PATH", "LEO_VD_INPUT_PATH")))), add = TRUE)
  options(leo.basic.vd_cmd = fake_vd)

  out <- paste(capture.output(vd(data.frame(a = 1:2))), collapse = "\n")
  expect_match(out, "VisiData summary")
  expect_match(out, "File size")
  expect_match(out, "Temp file >>> ")
  expect_match(out, "/.*vd_")
  expect_no_match(out, "RAM")
  expect_no_match(out, "Opening [0-9]+ x [0-9]+ table")
  expect_no_match(out, "VisiData closed")
  out_lines <- strsplit(out, "\n", fixed = TRUE)[[1]]
  expect_equal(sum(grepl("^[-+ ]+$", out_lines)), 2)
})

test_that("vd returns input when vd is unavailable", {
  old_opt <- getOption("leo.basic.vd_cmd")
  on.exit(options(leo.basic.vd_cmd = old_opt), add = TRUE)
  options(leo.basic.vd_cmd = FALSE)

  df <- data.frame(x = 1:3)
  expect_invisible(res <- vd(df))
  expect_identical(res, df)
})

test_that("vd writes a temporary tsv and cleans it up", {
  tmp_dir <- tempfile("leo_vd_test_")
  dir.create(tmp_dir)
  capture_path <- file.path(tmp_dir, "captured.tsv")
  input_path_log <- file.path(tmp_dir, "input_path.txt")
  fake_vd <- make_fake_vd(tmp_dir, capture_path, input_path_log)

  old_opt <- getOption("leo.basic.vd_cmd")
  old_capture <- Sys.getenv("LEO_VD_CAPTURE_PATH", unset = NA_character_)
  old_input <- Sys.getenv("LEO_VD_INPUT_PATH", unset = NA_character_)
  on.exit(options(leo.basic.vd_cmd = old_opt), add = TRUE)
  on.exit(do.call(Sys.setenv, as.list(stats::setNames(c(old_capture, old_input), c("LEO_VD_CAPTURE_PATH", "LEO_VD_INPUT_PATH")))), add = TRUE)
  options(leo.basic.vd_cmd = fake_vd)

  df <- data.frame(a = c("x", "y"), b = c(1, 2), check.names = FALSE)
  expect_invisible(vd(df))
  expect_true(file.exists(capture_path))
  expect_equal(utils::read.delim(capture_path, check.names = FALSE), df)

  tmp_path <- readLines(input_path_log, warn = FALSE)
  expect_false(file.exists(tmp_path))
})

test_that("vd writes non-default rownames as the first column", {
  tmp_dir <- tempfile("leo_vd_test_")
  dir.create(tmp_dir)
  capture_path <- file.path(tmp_dir, "captured.tsv")
  input_path_log <- file.path(tmp_dir, "input_path.txt")
  fake_vd <- make_fake_vd(tmp_dir, capture_path, input_path_log)

  old_opt <- getOption("leo.basic.vd_cmd")
  old_capture <- Sys.getenv("LEO_VD_CAPTURE_PATH", unset = NA_character_)
  old_input <- Sys.getenv("LEO_VD_INPUT_PATH", unset = NA_character_)
  on.exit(options(leo.basic.vd_cmd = old_opt), add = TRUE)
  on.exit(do.call(Sys.setenv, as.list(stats::setNames(c(old_capture, old_input), c("LEO_VD_CAPTURE_PATH", "LEO_VD_INPUT_PATH")))), add = TRUE)
  options(leo.basic.vd_cmd = fake_vd)

  df <- data.frame(a = c("x", "y"), b = c(1, 2), check.names = FALSE)
  rownames(df) <- c("cell_1", "cell_2")
  out <- paste(capture.output(expect_invisible(vd(df))), collapse = "\n")

  x <- utils::read.delim(capture_path, check.names = FALSE)
  expect_identical(names(x)[1], ".rowname")
  expect_identical(x$.rowname, rownames(df))
  expect_equal(x[, -1, drop = FALSE], data.frame(a = c("x", "y"), b = c(1, 2), check.names = FALSE))
  expect_match(out, "yes \\(.rowname\\)")
})

test_that("vd accepts matrix input", {
  old_opt <- getOption("leo.basic.vd_cmd")
  on.exit(options(leo.basic.vd_cmd = old_opt), add = TRUE)
  options(leo.basic.vd_cmd = FALSE)

  df <- as.data.frame(matrix(1:4, nrow = 2))
  expect_invisible(res <- vd(df))
  expect_identical(res, df)
})

test_that("vd rejects unsupported input", {
  old_opt <- getOption("leo.basic.vd_cmd")
  on.exit(options(leo.basic.vd_cmd = old_opt), add = TRUE)
  options(leo.basic.vd_cmd = FALSE)

  expect_error(vd(1:3), "`df` must be a data.frame or matrix.", fixed = TRUE)
})
