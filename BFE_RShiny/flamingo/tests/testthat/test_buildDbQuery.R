
test_that("query builder produces correct queries", {

      stmt11 <- "exec dbo.ListModelAna 1, 'test'"
      stmt12 <- buildDbQuery("ListModelAna", 1, "test")

      expect_equal(stmt11, stmt12)

      stmt21 <- "exec dbo.createCompany 'a', 'b', 'c', 'd'"
      stmt22 <- buildDbQuery("createCompany", "a", "b", "c", "d")

      expect_equal(stmt21, stmt22)

      stmt31 <- "exec dbo.getModelData 1, 0, 0, 0"
      stmt32 <- buildDbQuery("getModelData", 1, 0, 0, 0)

      expect_equal(stmt31, stmt32)

})

