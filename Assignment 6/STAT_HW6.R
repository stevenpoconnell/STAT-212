## Problem 6
## a.)
Soil=read.table(“SoilDataNhi.txt”, header = T)
attach(soil)
t.test(Soil1, Soil2, paired = T)

## b.)
wilcox.test(Soil1, Soil2, conf.int=T, conf.level=0.95, paired=TRUE)
