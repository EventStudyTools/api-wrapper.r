context("Application Input Interface")

test_that("Application Input Interface", {

  AII <- ApplicationInputInterface$new()
  parameterList <- list("arc" = 1,
                        "test2" = 1)
  AII$setNamedList(parentLevel = "tasks", parameterList = parameterList)
  AII$tasks
})
