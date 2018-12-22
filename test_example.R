
library(testthat)
library(tidyverse)

context("my first test script in R")


source("anainit.R")

test_that("number of columns in Iris data", {
  expect_equal(ncol(iris),5)
})


test_that("number of columns in Iris data 2", {
  expect_equal(ncol(iris),5)
})

test_that("test the anainit function",{
  
  shell <- anainit()
  
  expect_equal(.GlobalEnv$varorder_,0)
  expect_equal(nrow(shell),0)
  expect_equal(ncol(shell),7)
  
})

