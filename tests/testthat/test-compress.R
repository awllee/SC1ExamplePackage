A <- grayscale(load.jpeg(system.file("img", "Rlogo.jpg", package="jpeg")))
svd.A <- svd(A)

test_that("get.singular.values", {
  expect_equal(get.singular.values(A), svd.A$d)
})

cmtx.rank.2 <- compress.matrix(A, rank=2)

test_that("compress by rank", {
  expect_equal(decompress.matrix(cmtx.rank.2),
               svd.A$u[,1:2]%*%diag(svd.A$d[1:2])%*%t(svd.A$v[,1:2]))
})

cmtx.ratio.20 <- compress.matrix(A, ratio=20)

test_that("compress by ratio", {
  expect_true(cmtx.ratio.20$rank == 2)
  expect_true(cmtx.ratio.20$ratio >= 20)
  expect_true(all(decompress.matrix(cmtx.rank.2) == decompress.matrix(cmtx.ratio.20)))
})

cmtx.ratio.20 <- compress.matrix(A, ratio=20)

test_that("compress by ratio", {
  expect_true(cmtx.ratio.20$rank == 2)
  expect_true(cmtx.ratio.20$ratio >= 20)
  expect_true(all(decompress.matrix(cmtx.rank.2) == decompress.matrix(cmtx.ratio.20)))
})

cmtx.ae.p01 <- compress.matrix(A, avg.error=0.01)

test_that("compress by avg.error", {
  expect_true(cmtx.ae.p01$avg.error <= 0.01)
})

summary(cmtx.ae.p01)

expect_error(compress.matrix(A))
expect_error(compress.matrix(A, rank=2, ratio=2, avg.error=1))
expect_error(compress.matrix(A, rank=2, ratio=2))
expect_error(compress.matrix(A, rank=2, avg.error=1))
expect_error(compress.matrix(A, ratio=2, avg.error=1))
expect_warning(cmtx.ratio.1000 <- compress.matrix(A, ratio=1000))
expect_equal(cmtx.ratio.1000$rank, 1)

view.image(fix.image(decompress.matrix(cmtx.ae.p01)))
