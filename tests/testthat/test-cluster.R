context("cluster function")

l <- list(c("xaz", "xbz", "xcz", "m", "mn", "mno", "mnop"), 1:5)
c <- create_convo(l)

test_that(
  "cluster fx can work with either list or convo", {
    clust_l <- cluster_convo(l)
    clust_c <- cluster_convo(c)
    expect_length(clust_l, length(l))
    expect_length(clust_c, length(c))
    expect_s3_class(clust_l[[1]], "hclust")
    expect_s3_class(clust_l[[1]], "hclust")
  })

test_that(
  "cluster distances change as expected with adist param", {
    clust_insdel <- cluster_convo(l, c(ins = 1, del = 1, sub = 5))
    clust_substi <- cluster_convo(l, c(ins = 5, del = 5, sub = 1))
    expect_error(cluster_convo(l, c(1,1,1)))
    expect_equal(clust_insdel[[1]]$height, c(1,1,1,2,2,4))
    expect_equal(clust_substi[[1]]$height, c(1,1,3,5,5,5))
  })
