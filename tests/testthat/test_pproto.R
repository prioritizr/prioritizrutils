context('pproto')

test_that('check that proto objects behave as expected', {
  # initialize
  p <- proto(a=5)
  p2 <- p$proto(b=5)
  # check that fields are correct
  expect_equal(p$ls(), 'a')
  expect_equal(p$a, 5)
  expect_equal(p2$ls(), 'b')
  expect_equal(p2$b, 5)
  # update value
  p$a <- 10
  # check that a in both objects have become 10
  expect_equal(p$a, 10)
  expect_equal(p2$a, 10)
})

test_that('create new pproto', {
  p <- pproto(a=5)
  expect_equal(p$ls(), 'a')
  expect_equal(p$a, 5)
})

test_that('create inherited proto with s3 fields', {
  # initialize
  p <- pproto(a=5)
  p2 <- pproto(NULL, p, b=5)
  # check that fields are correct
  expect_equal(p$ls(), 'a')
  expect_equal(p$a, 5)
  expect_equal(p2$ls(), c('a', 'b'))
  expect_equal(p2$a, 5)
  expect_equal(p2$b, 5)
  # update parent proto
  p$a <- 7
  # check that fields are correct
  expect_equal(p$ls(), 'a')
  expect_equal(p$a, 7)
  expect_equal(p2$ls(), c('a', 'b'))
  expect_equal(p2$a, 5)
  expect_equal(p2$b, 5)
})


test_that('create inherited proto with proto fields', {
  # initialize
  p <- pproto(a=5)
  p2 <- pproto(NULL, p, b=5)
  p3 <- pproto(NULL, p2, c=5)
  # check that fields are correct
  expect_equal(p$ls(), 'a')
  expect_equal(p$a, 5)
  expect_equal(p2$ls(), c('a', 'b'))
  expect_equal(p2$a, 5)
  expect_equal(p2$b, 5)
  expect_equal(p3$ls(), c('a', 'b', 'c'))
  expect_equal(p3$a, 5)
  expect_equal(p3$b, 5)
  expect_equal(p3$c, 5)
  # change p and check that values are correct
  p$a <- 7
  p2$b <- 9
  # check that fields are correct
  expect_equal(p$ls(), 'a')
  expect_equal(p$a, 7)
  expect_equal(p2$ls(), c('a', 'b'))
  expect_equal(p2$a, 5)
  expect_equal(p2$b, 9)
  expect_equal(p3$ls(), c('a', 'b', 'c'))
  expect_equal(p3$a, 5)
  expect_equal(p3$b, 5)
  expect_equal(p3$c, 5)
})


