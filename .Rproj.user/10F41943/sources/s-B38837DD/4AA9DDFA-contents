#
#
#
# SAVING PLOTS
# Saving plots into pdfs
#

# ==== SIMPLE REGRESSION PLOTS =================================================
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
# ==============================================================================

# ===== ASSUMPTION PLOTS ========================================================
pdf("4.figures/assumptions.pdf", height = 5, width = 8)

par(mfrow =c(2,2))
plot(linearMod)

par(mfrow = c(1,1))
acf(linearMod$residuals)


dev.off()
# ==============================================================================

# ===== MULTIPLE REGRESSION PLOTS ==============================================
pdf("4.figures/multiple.regression.pdf", height = 5, width = 8)

corrplot(cor(sim.data[,-3]))

persp(kde, 
      theta = 310, 
      phi = 30, 
      xlab = "beta_1", 
      ylab = "beta_2", 
      zlab = "Est. Density",
      main = "Distribution of Model Estimates")

dev.off()
# ==============================================================================

# ==== DEBT DATA ANALYSIS PLOTS ================================================
pdf("4.figures/debtdata.analysis.pdf", height = 5, width = 8)

scatter.smooth(x= debt.data$financial.integration,
               y= debt.data$debt.stock,
               main ="Debt stock~ financial integration",
               xlab = "Financial Integration",
               ylab = "Debt Stock")

par(mfrow = c(1,2))
boxplot(debt.data$financial.integration, main = "Financial Integration")
boxplot(debt.data$debt.stock, main = "Debt Stock")

par(mfrow=c(1,2))
plot(density(debt.data$debt.stock,na.rm = T), main = "Density Plot: Debt.stock", 
     ylab = "Frequency",
     sub = paste("Skewness:",round(e1071::skewness(debt.data$debt.stock, na.rm = T),2)))
polygon(density(debt.data$debt.stock, na.rm =T), col = 'red')

plot(density(debt.data$financial.integration,na.rm = T), 
     main = "Density Plot: Financial.Integration", 
     ylab = "Frequency",
     sub = paste("Skewness:",round(e1071::skewness(debt.data$financial.integration,
                                                   na.rm = T),2)))
polygon(density(debt.data$financial.integration, na.rm =T), col = 'red')

par(mfrow = c(1,1))

debt.plot<- stripchart(debt.stock ~ financial.integration,
                       data =debt.data,
                       vertical = TRUE,
                       pch = 16, 
                       cex = 1.5, las = 1, col = "firebrick",
                       xlab = "Financial Integration", ylab = "Debt Stock",
                       main = "Debt stock ~ Financial Integration") +
  abline(debt.mod) + abline(debt.mod_2, col ='blue')

dev.off()

# ==============================================================================


