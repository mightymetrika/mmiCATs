# test_that("cluster_im_glmrob works", {
#   require(effects)
#   data(WVS)
#
#   logit.model <- glm(degree ~ religion + gender + age, data=WVS, family=binomial(link="logit"))
#   summary(logit.model)
#
#   # compute cluster-adjusted p-values
#   clust.im.p <- clusterSEs::cluster.im.glm(logit.model, WVS, ~ country, report = T)
#
#
#
#   rlogit.model <- robust::glmRob(degree ~ religion + gender + age, data=WVS,
#                                 family=binomial(link="logit"),
#                                 method = "misclass")
#   summary(rlogit.model)
#   clust.im.p <- cluster_im_glmrob(rlogit.model, WVS, ~ country, report = T)
#
# })

# test_that("working function", {
#
#   library(effects)
#
#
#   data(WVS)
#   dat <- WVS
#   meth <- "misclass"
#   robmod <- robust::glmRob(degree ~ religion + gender + age,
#                            family=binomial(link="logit"),
#                            data = dat,
#                            method = meth)
#
#   cluster = ~ country
#   ci.level = 0.95
#   report = TRUE
#   drop = FALSE
#   truncate = FALSE
#   return.vcv = FALSE
#   expect_equal(2 * 2, 4)
# })
