#
#
# #-------------------------------------------------------------------------------
# # plot results
# #-------------------------------------------------------------------------------
#
# BF_fixed_random <- sapply(results, function(x) x$BF_fixed_random)
# BFplus0_random <- sapply(results, function(x) x$BFplus0_random)
# BFplus0_fixed <- sapply(results, function(x) x$BFplus0_fixed)
# BFplus0_random <- sapply(results, function(x) x$BFplus0_random)
# BFincl <- sapply(results, function(x) x$BFincl)
#
# # plot BF
# windows(5.5, 9)
# layout(matrix(1:2, nrow = 2))
# par(mar = c(5.1, 4.1, 1, 1.1))
#
# pchVec <- c(23, 24, 21) # fixed, random, weighted
# colVec <- c("white", "white", "lightgrey")
#
# yrange <- range(log(BF_fixed_random))*1.1
# xpos <- 1:length(BF_fixed_random)
# plot(xpos,log(BF_fixed_random), ylim = yrange, ylab="Bayes Factor",
#      type = "l", lwd = 2, bty = "l",
#      xlab = expression(paste("Scale (SD) of Half Cauchy Prior on ", tau)),
#      xaxt = "n", yaxt = "n")
# axis(1, at = 1:length(sdVec), labels = round(sdVec, 2))
# BFlab <- c(1, 3, 10, 30, 100, 300, 1000)
# axis(2, at = log(BFlab), labels = BFlab, las = 1)
# abline(h = c(0, log(3)), lty = "dashed", col = "black")
# BFlabel <- round(BF_fixed_random, 1)
#
# # add BF points
# gap <- .15
# points(xpos, log(BF_fixed_random), cex = 1.3, pch = 22, bg = "black")
#
# lines(xpos - gap ,log(BFplus0_fixed), lwd = 2)
# points(xpos - gap,log(BFplus0_fixed), pch = pchVec[1], bg = colVec[1], cex = 1.3)
#
# lines(xpos + gap,log(BFplus0_random), lwd = 2)
# points(xpos + gap, log(BFplus0_random), pch = pchVec[2], bg = colVec[2], cex = 1.1)
#
# lines(xpos, log(BFincl), lwd = 2)
# points(xpos, log(BFincl), pch = pchVec[3], bg = colVec[3], cex = 1.3)
#
#
# legend(x = xpos[1], y = yrange[2], pch = c(22, pchVec), pt.bg = c("black", colVec),
#        ncol = 1, pt.cex = 1.3, xjust = 0, yjust = 1, lwd = 2, bty = "n",
#        legend = c("Fixed over random effect model",
#                   "H1 over H0 (fixed effect model)",
#                   "H1 over H0 (random effect model)",
#                   "H1 over H0 (weighted)"))
#
# # plot posterior log-odds ratio for both models and the model average
# par(mar = c(5.1, 4.1, 1, 1.1))
# gap <- .2
# xpos <- 1:length(sdVec)
# ylim <- 1.4*range(cbind(hpd95.fixed[1,], hpd95.rand))
# plot(NULL, xlim = c(min(xpos) - gap, max(xpos) + gap), ylab = "Log Odds Ratio",
#      xlab = expression(paste("Scale (SD) of Half Cauchy Prior on ", tau)),
#      xaxt = "n", ylim = ylim, bty = "l", las = 1)
# axis(1, at = xpos, labels = round(sdVec, 2))
# arrows(xpos +gap, hpd95.rand[1,], xpos + gap,hpd95.rand[2,], code = 3, length = .03, angle = 90)
# arrows(xpos - gap, hpd95.fixed[1], xpos - gap,hpd95.fixed[2], code = 3, length = .03, angle = 90)
# arrows(xpos, hpd95.mixed[1,], xpos, hpd95.mixed[2,], code = 3, length = .03, angle = 90)
#
# points(xpos - gap, rep(m.fixed, length(xpos)), pch = pchVec[1], bg = colVec[1], cex = 1.3)
# points(xpos + gap, m.rand, pch = pchVec[2], bg = colVec[2], cex = 1.1)
# points(xpos, m.mixed, pch = pchVec[3], bg = colVec[3], cex = 1.3)
#
# legend(x = mean(xpos) + .05, y = ylim[2]*.8, pch = pchVec, pt.bg = colVec,
#        legend = c("fixed effect", "random effect", "weighted effect"),
#        ncol = 3, pt.cex = 1.3, xjust = .5, cex = .9, yjust = 0, bty = "n")
# abline(h = 0, lty = "dashed", col = "black")
#
# savePlot("Figure1.eps", type = "eps")
# savePlot("Figure1.jpeg", type = "jpg")
