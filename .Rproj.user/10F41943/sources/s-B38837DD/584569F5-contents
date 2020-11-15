pdf("4.figures/simple.linear.regression.pdf", height = 5, width = 8)

slr.plot <- scatter.smooth(x= sim.data$X1, y= sim.data$Y, main ="X1~ Y",
                           pch =16, col = 'red', xlab = "X1", ylab = "Y")

par(mfrow = c(1,2))
boxplot(sim.data$X1, main = "X1",
        sub = paste("Outlier rows:",
                    boxplot.stats(sim.data$X1)$out))
boxplot(sim.data$Y, main = "Y",
        sub = paste("Outlier rows:",
                    boxplot.stats(sim.data$Y)$out))
plot(density(sim.data$X1), main = "Density Plot:X1", 
     ylab = "Frequency", sub = paste("Skewness:",
                                     round(e1071::skewness(sim.data$X1),2)))
polygon(density(sim.data$X1), col = 'red')
plot(density(sim.data$Y), main = "Density Plot:Y", 
     ylab = "Frequency", sub = paste("Skewness:",
                                     round(e1071::skewness(sim.data$Y),2)))
polygon(density(sim.data$Y), col = 'red')
par(mfrow = c(1,1))
cvResults <- suppressWarnings( CVlm (sim.data, form.lm = Y ~ X1,
                                     m=10, dots=FALSE, seed=29,
                                     legend.pos="topleft",  printit=FALSE,
                                     main="10-Fold Cross Validation"))

dev.off()