# Test data-generating functions

# eqnfile <- system.file("MPTmodels/2htm.eqn", package="TreeBUGS")


# test_that('genData generates expected frequencies for simple models', {
#
#   exp <- c((1-par["Dn"])*par["g"],  # FA
#            par["Do"] + (1-par["Do"])*par["g"])  # Hit
#   names(exp) <- NULL
#
#   # EQN file
#   d <- genMPT(par, c(Lure=n, Target=n), eqnfile)
#   expect_equal(d[2:3]/n,  exp, tolerance=.01)
#
#   # string model
#   d <- genMPT(par, c(Lure=n, Target=n), model)
#   expect_equal(d[2:3]/n,  exp, tolerance=.01)
#
#   # naming errors expected:
#   expect_warning(genMPT(par, c(n, n), model))
#   expect_warning(genMPT(c(.1,.4,.2), c(Lure=n, Target=n), model))
#
#   # errors due to misspecifiation:
#   expect_error(genMPT(c(.1,.4), c(Lure=n, Target=n), model, warning=FALSE))
#   expect_error(genMPT(c(.1,.4,.4), c(Lure=n), model, warning=FALSE))
# })


