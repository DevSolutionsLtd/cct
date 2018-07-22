layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
hist(data$child.no,
     breaks=7,
     main='Children Born to Beneficiaries',
     col = "lightgrey",
     ylim = c(0, 140),
     xlab = NULL,
     ylab = "No. of Beneficiaries")
hist(data$child.before.cct,
     breaks=7,
     main='Children Born before CCT',
     col="lightgrey",
     ylim = c(0, 140),
     ylab = NULL,
     xlab = NULL)
hist(data$anc.before.cct,
     breaks=7,
     col="lightgrey",
     ylab = NULL,
     xlab = NULL,
     main= 'ANC before CCT',
     ylim = c(0, 140))
hist(data$delivered.hf,
     breaks=7,
     col="lightgrey",
     main='Children Delivered in HF',
     xlab= "No. of Children",
     ylim = c(0, 140),
     ylab = NULL)

