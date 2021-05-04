wants <- c("ggfortify", "here", "ggplot2")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])


library(here)

fish<- read.csv(here("Data", "fish.csv"))
head(fish)

names(fish)<-c("site", "protection", "observer",  "Chr.unim",  "Das.trim", "Ple.lacr", "Pom.caer", "Pom.pavo", "Pom.sulf", "Ste.fasc")

summary(fish)

table (fish$observer, fish$protection)

table (fish$observer, fish$site)

hist(fish$Chr.unim)
hist(fish$Ple.lacr)
hist(fish$Das.trim)
table(fish$Chr.unim)

library(ggplot2)
m<- ggplot(fish, aes(x=Chr.unim))
m <- m + geom_histogram(binwidth = 10)
m + facet_grid( ~ protection)

M1.C.unim<- glm(Chr.unim~protection+as.factor(site), data=fish, family= "poisson")
summary(M1.C.unim)

M2.C.unim<- glm(Chr.unim~protection+observer, data=fish, family= "poisson")
summary(M2.C.unim)

M3.C.unim<- glm(Chr.unim~as.factor(site)+observer, data=fish, family= "poisson")
summary(M3.C.unim)

M4.C.unim<- glm(Chr.unim~observer, data=fish, family= "poisson")
summary(M4.C.unim)

M5.C.unim<- glm(Chr.unim~as.factor(site), data=fish, family= "poisson")
summary(M5.C.unim)

M6.C.unim<- glm(Chr.unim~as.factor(site)*observer, data=fish, family= "poisson")
summary(M6.C.unim)

AIC(M1.C.unim,M2.C.unim,M3.C.unim,M4.C.unim,M5.C.unim,M6.C.unim)

autoplot(M6.C.unim)

