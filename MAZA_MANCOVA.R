## MANCOVA

# We can now also extend our MANOVA to control for a covariate

# The assumptions is as in MANOVA and ANCOVA 

# Just now we have see that .......
# It is basically running ancova on 2 linear combination of the outcome instead of run 2 seperate ancova.
outcome = cbind(sbp, dbp)
m.mancova = manova(outcome ~ exercise * hba1c, data = data)
summary(m.mancova, test = "Wilks", type = "III")

# There is significant different on both main effect of exercise and hba1c, but the interaction term is not significant.

