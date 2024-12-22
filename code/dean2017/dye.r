# dye.r, dye experiment, Table 14.17, p490

dye2.data = read.table("data/dye2.txt", header=T)
head(dye2.data, 3)
# Create factor variables
dye2.data = within(dye.data, 
           {fBlk = factor(Blk); fA = factor(A); 
              fB = factor(B);   fC = factor(C) })

# Analysis of variance
model1 = lm(y ~ fBlk + fA*fB*fC, data=dye.data)
anova(model1)

# ANOVA without block effects for comparison
model2 = lm(y ~ fA*fB*fC, data=dye.data)
anova(model2)

