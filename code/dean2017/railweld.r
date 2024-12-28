# railweld.r, rail weld experiment, Table 7.14, p235

# Input data for T V S y
rail.data = read.table("data/rail.weld.txt", header=T)
# Create factor variables, then display first 3 lines of rail.data
rail.data = within(rail.data, 
  { fT = factor(T); fV = factor(V); fS = factor(S) })
head(rail.data, 3)

# Try to fit a 3-way complete model
model1 = aov(y ~ fT*fV*fS, data=rail.data); anova(model1)
# See main effects non-estimable under complete model if empty cells
library(lsmeans)
lsmT = lsmeans(model1, ~ fT)
lsmT; summary(contrast(lsmT, method="pairwise"), infer=c(T,T))

# Fit a model using 5 degrees of freedom
options(contrasts=c("contr.sum","contr.poly"))
model2 = aov(y ~ fT + fV + fS + fT:fS + fV:fS, data=rail.data)
anova(model2) # Type I ANOVA
drop1(model2, ~., test="F") # Type III ANOVA

# Estimate main effects of T, V, and S
lsmT = lsmeans(model2, ~ fT); lsmT
summary(contrast(lsmT, method="pairwise"), infer=c(T,T))
lsmV = lsmeans(model2, ~ fV); lsmV
summary(contrast(lsmV, method="pairwise"), infer=c(T,T))
lsmS = lsmeans(model2, ~ fS); lsmS 
summary(contrast(lsmS, method="pairwise"), infer=c(T,T))

# Estimating interaction contrasts
lsmTS = lsmeans(model2, ~ fT:fS); lsmTS 
summary(contrast(lsmTS, list(TS=c(1,-1,-1,1)/2)), infer=c(T,T))
lsmVS = lsmeans(model2, ~ fV:fS); lsmVS 
summary(contrast(lsmVS, list(VS=c(1,-1,-1,1)/2)), infer=c(T,T))

# Multiple comparisons of treatment combinations
summary(contrast(lsmTS, method="pairwise"), infer=c(T,T))

