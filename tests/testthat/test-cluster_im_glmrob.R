# test_that("cluster_im_glmrob works", {
#   rlogit.model <- robust::glmRob(Sepal.Length ~ Sepal.Width + Petal.Length, data=iris)
#   robustbase::glmrob(Sepal.Length ~ Sepal.Width + Petal.Length + Species, family = "gaussian", data=iris)
#
# })

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
#   # Cluster-Adjusted p-values:
#   #
#   #   variable name   cluster-adjusted p-value
#   # (Intercept)                      0.137
#   # religionyes                      0.757
#   # gendermale                      0.483
#   # age                      0.054
#   #
#   # Confidence Intervals (centered on cluster-averaged results):
#   #
#   #   variable name               CI lower              CI higher
#   # (Intercept)      -1.84567477037683      0.414084696182325
#   # religionyes     -0.952467256737735        1.1797924425042
#   # gendermale     -0.532360229693009      0.318837207458131
#   # age    -0.0325665660185535   0.000525559143591075
#
#
#
#   rlogit.model <- robust::glmRob(degree ~ religion + gender + age, data=WVS,
#                                 family=binomial(link="logit"),
#                                 method = "misclass")
#   summary(rlogit.model)
#   clust.im.p <- cluster_im_glmrob(rlogit.model, WVS, ~ country, report = T)
#
#   # Cluster-Adjusted p-values:
#   #
#   #   variable name   cluster-adjusted p-value
#   # (Intercept)                      0.134
#   # religionyes                      0.746
#   # gendermale                      0.467
#   # age                       0.05
#   #
#   # Confidence Intervals (centered on cluster-averaged results):
#   #
#   #   variable name                CI lower               CI higher
#   # (Intercept)       -1.86970500480967       0.409902433681858
#   # religionyes      -0.945183450126014        1.18243130074336
#   # gendermale      -0.530712557564686        0.31114193064948
#   # age     -0.0313464388951018   -1.56322685635063e-05
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
