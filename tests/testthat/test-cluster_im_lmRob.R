# test_that("multiplication works", {
#   require(effects)
#   data(WVS)
#   WVS$degree.n <- as.numeric(WVS$degree)
#   WVS$gender.n <- as.numeric(WVS$gender)
#   WVS$genderXage <- WVS$gender.n * WVS$age
#   lin.model.n <- glm(degree.n ~ gender.n + age + genderXage + religion, data=WVS)
#   clust.im.result <- clusterSEs::cluster.im.glm(lin.model.n, WVS, ~ country, report = T, return.vcv = T)
#
#   rlin.model.n <- robust::lmRob(degree.n ~ gender.n + age + genderXage + religion, data=WVS)
#   rclust.im.result <- cluster_im_lmRob(rlin.model.n, WVS, ~ country, report = T, return.vcv = T)
#
#   expect_equal(2 * 2, 4)
# })
