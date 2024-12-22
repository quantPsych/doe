# battery.r, battery experiment, Table 4.2, page 97

battery.data = read.table("data/battery.txt", header=T)
battery.data$fType = factor(battery.data$Type)
head(battery.data, 3)

model1 = aov(LPUC � fType, data=battery.data) # Fit aov model
anova(model1) # Display 1-way ANOVA

# Individual contrasts: estimates, CIs, tests
library(lsmeans)
lsmType = lsmeans(model1, � fType) # Compute and save lsmeans
levels(battery.data$fType)
summary(contrast(lsmType, list(Duty=c( 1, 1,-1,-1)/2,
                              Brand=c( 1,-1, 1,-1)/2,
                                 DB=c( 1,-1,-1, 1)/2)),
infer=c(T,T), level=0.95, side="two-sided")

# Multiple comparisons
confint(lsmType, level=0.90) # Display lsmeans and 90

# Tukey�s method
summary(contrast(lsmType, method="pairwise", adjust="tukey"),
        infer=c(T,T), level=0.99, side="two-sided")

# Dunnett�s method
summary(contrast(lsmType, method="trt.vs.ctrl", adjust="mvt", ref=1),
        infer=c(T,T), level=0.99, side="two-sided")