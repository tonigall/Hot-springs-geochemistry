install.packages ('ggplot2')
install.packages ('ggfortify')
install.packages("ggcorrplot")
install.packages("rcartocolor")
install.packages("devtools")
install.packages("corrr")
install.packages("FactoMineR")
install.packages("factoextra")
library(ggplot2)
library(ggfortify)
library(ggcorrplot)
library(rcartocolor)
library(devtools)
library(corrr)
library(FactoMineR)
library(factoextra)

geochem<-read.table("~/Desktop/geochem_hot_springs.csv", header=T, sep=",")
head(geochem)
str(geochem)
dim(geochem)
colSums(is.na(geochem))

hist(geochem$pH)
hist(geochem$Temp)
hist(geochem$Si)
hist(geochem$DO)

geochem$Bedrock<-factor(geochem$Bedrock, levels = c("Basaltic", "Basaltic/Sandstone", "Basaltic/Rhyolitic", "Andesitic/Dacitic", "Rhyolitic/Basaltic", "Rhyolitic/Tuff", "Rhyolitic/Greywacke", "Rhyolitic/Siltstone", "Rhyolitic", "Glacial deposits", "Lacustrine deposits"))
geochem$Bedrock<-factor(geochem$Bedrock)
geochem$Country<-factor(geochem$Country)
geochem$Area<-factor(geochem$Area)
geochem$Paper<-factor(geochem$Paper)

##perform PCA (without DO, S, temp, bedrock)

trim.data<-na.omit(geochem[,c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)])
head(trim.data)
num.data<-trim.data[,c(5,6,7,8,10,11,12,13,14,15,16,17,18,19)]
head(num.data)
data.norm<-scale(num.data)
head(data.norm)
corr_matrix<-cor(data.norm)
ggcorrplot(corr_matrix, type = "lower")

pca_res<-princomp(data.norm) ##or pca_res<-prcomp(data.norm, scale = TRUE)

summary(pca_res)
str(pca_res)
scores <- pca_res$x[,1:4]
km <- kmeans(scores, centers = 4, nstart = 5)
ggdata <- data.frame(scores, Cluster = km$cluster, Bedrock = trim.data$Bedrock)
var_explained = pca_res$sdev^2 / sum(pca_res$sdev^2)
print(var_explained)
fviz_eig(pca_res, addLabels = TRUE)
autoplot(km, data = scores)
fviz_cos2(pca_res, choice = "var", axes = 1:2)
fviz_pca_var(pca_res, col.var = "black",repel = TRUE)
fviz_pca_biplot(pca_res,  label = "var", repel = TRUE, col.var = "black", col.ind = "blue")
eig.val <- get_eigenvalue(pca_res)
eig.val
res.var <- get_pca_var(pca_res)
res.var$coord
res.var$contrib
res.var$cos2
res.ind <- get_pca_ind(pca_res)
res.ind$coord
res.ind$contrib
res.ind$cos2

ggplot(ggdata) +
  geom_point(aes(x=PC1, y=PC2, color=factor(Cluster))) +
  stat_ellipse(aes(x=PC1,y=PC2, fill=factor(Cluster)),
               geom="polygon", level=0.95, alpha=0.2) +
  guides(color=guide_legend("Cluster"),fill=guide_legend("Cluster"))

ggplot(ggdata) +
  geom_point(aes(x=PC1, y=PC2, color=factor(Bedrock)), size=5, shape=20) +
  stat_ellipse(aes(x=PC1,y=PC2,fill=factor(Bedrock)),
               geom="polygon", level=0.95, alpha=0.2) +
  guides(color=guide_legend("Bedrock"),fill=guide_legend("Bedrock"))

##plot PCA

geochem$Bedrock<-factor(geochem$Bedrock, levels = c("Basaltic", "Basaltic/Sandstone", "Basaltic/Rhyolitic", "Andesitic/Dacitic", "Rhyolitic/Basaltic", "Rhyolitic/Tuff", "Rhyolitic/Greywacke", "Rhyolitic/Siltstone", "Rhyolitic", "Glacial deposits", "Lacustrine deposits"))

trim.data$Bedrock<-factor(trim.data$Bedrock, levels = c("Basaltic", "Basaltic/Sandstone", "Basaltic/Rhyolitic", "Andesitic/Dacitic", "Rhyolitic/Basaltic", "Rhyolitic/Tuff", "Rhyolitic/Greywacke", "Rhyolitic/Siltstone", "Rhyolitic", "Glacial deposits", "Lacustrine deposits"))

pcaplot<-autoplot(pca_res, data=trim.data, loadings = TRUE, loadings.label = TRUE, , loadings.color = "black")
pcaplot + geom_point(aes(colour=Bedrock)) + scale_colour_carto_d(palette = "Safe") +
theme_classic()

autoplot(pca_res, data=trim.data, colour="Bedrock", loadings = TRUE, loadings.label = TRUE)
autoplot(pca_res, data=trim.data, colour="DO", loadings = TRUE, loadings.label = TRUE)
autoplot(pca_res, data=trim.data, colour="pH", loadings = TRUE, loadings.label = TRUE)
autoplot(pca_res, data=trim.data, colour="Temp", loadings = TRUE, loadings.label = TRUE)
autoplot(pca_res, data=trim.data, colour="S", loadings = TRUE, loadings.label = TRUE)
autoplot(pca_res, data=trim.data, colour="Area", loadings = TRUE, loadings.label = TRUE)
autoplot(pca_res, data=trim.data, colour="Country", loadings = TRUE, loadings.label = TRUE)
autoplot(pca_res, data=trim.data, colour="Paper", loadings = TRUE, loadings.label = TRUE)


##pH only models
pH1<-lm(pH ~ SO4 + S + Fe + Mo + V + Mn + Co + Ni + Cu + Zn + Mg, data=geochem)
summary(pH1)
anova(Bedrock)
drop1(Bedrock, test="F")
par(mfrow=c(2,2))
plot(Bedrock)

##we know SO4 is opposite pH in PCA

##try without SO4?

pH2<-lm(pH ~ S + Fe + Mo + V + Mn + Co + Ni + Cu + Zn + Mg, data=geochem)
summary(pH2)
anova(pH2)
drop1(pH2, test="F")
par(mfrow=c(2,2))
plot(pH2)

AIC(pH1, pH2)

##pH1 model has higher adj R squared and lower AIC but worse residuals and DF

##include interactions based on PCA

pH3<-lm(pH ~ Fe + Mo + V + Mn + Co + Ni + Cu + Zn + Mg + Cu*Zn + Mo*S, data=geochem)
summary(pH3)
anova(pH3)
drop1(pH3, test="F")
par(mfrow=c(2,2))
plot(pH3)
AIC(pH1, pH2, pH3)
lowest AIC: pH1

##try without metals that seem to be related to pH

pH4<-lm(pH ~ SO4 + S + Fe + Mo + Cu + Zn, data = geochem)
summary(pH4)
anova(pH4)
drop1(pH4, test="F")
par(mfrow=c(2,2))
plot(pH4)

##doesn't seem to be improved by S, Cu or Zn

AIC(pH1, pH2, pH3, pH4)

lowest AIC: pH4 (170)

##try without S, Cu or Zn

pH5<-lm(pH ~ SO4 + Fe + Mo, data = geochem )
summary(pH5)
anova(pH5)
drop1(pH5, test="F")
par(mfrow=c(2,2))
plot(pH5)

AIC(pH1, pH2, pH3, pH4, pH5)

##pH5 seems to be the best in terms of adj R, AIC, DF and residuals

##DO model

DO1<-lm(DO ~ SO4 + S + Fe + Mo + V + Mn + Co + Ni + Cu + Zn + Mg, data=geochem)
summary(DO1)
anova(DO1)
drop1(DO1, test="F")
par(mfrow=c(2,2))
plot(DO1)

##try without Co, Ni or Cu

DO2<-lm(DO ~ SO4 + S + Fe + Mo + V + Mn + Zn + Mg, data=geochem)
summary(DO2)
anova(DO2)
drop1(DO2, test="F")
par(mfrow=c(2,2))
plot(DO2)

##try without SO4 or S

DO3<-lm(DO ~ Fe + Mo + V + Mn + Zn + Mg, data=geochem)
summary(DO3)
anova(DO3)
drop1(DO3, test="F")
par(mfrow=c(2,2))
plot(DO3)

##bedrock

Mo<-lm(Mo~Bedrock, data = geochem)
summary(Mo)

Cu<-lm(Cu~Bedrock, data = geochem)
summary(Cu)

Ni<-lm(Ni~Bedrock, data = geochem)
summary(Ni)

V<-lm(V~Bedrock, data = geochem)
summary(V)

Zn<-lm(Zn~Bedrock, data = geochem)
summary(Zn)

Co<-lm(Co~Bedrock, data = geochem)
summary(Co)

Mn<-lm(Mn~Bedrock, data = geochem)
summary(Mn)

Mg<-lm(Mg~Bedrock, data = geochem)
summary(Mg)

Fe<-lm(Fe~Bedrock, data = geochem)
summary(Fe)



##drop variables with low F value

bedrock5<-lm(pH ~ DO + Bedrock + SO4 + S + Fe + Fe*Bedrock + Mo*Bedrock + Mn + Mn*Bedrock + Co + Co*Bedrock + Ni + Cu*DO + Mo*DO + Zn, data=geochem)
summary(bedrock5)
anova(bedrock5)
drop1(bedrock5, test="F")
par(mfrow=c(2,2))
plot(bedrock5)

AIC(bedrock1, bedrock2, bedrock3, bedrock4, bedrock5)

##DO does not improve this model, need to make separate models for this
##add temp

bedrock6<-lm(pH ~ Temp + Bedrock + SO4 + S + Fe + Fe*Bedrock + Mo + Mo*Bedrock + V*Bedrock + Mn + Mn*Bedrock + Co + Co*Bedrock + Ni + Cu + Cu*Bedrock + Zn + Zn*Bedrock, data=geochem)
summary(bedrock6)
anova(bedrock6)
drop1(bedrock6, test="F")
par(mfrow=c(2,2))
plot(bedrock5)

AIC(bedrock1, bedrock2, bedrock3, bedrock4, bedrock5, bedrock6)

##add interactions for temp, remove bedrock*Cu and bedrock*Co

bedrock7<-lm(pH ~ Temp + Bedrock + SO4 + S + Fe + Fe*Bedrock + Fe*Temp + Mo + Mo*Bedrock + Mo*Temp + V*Bedrock + Mn + Mn*Bedrock + Co + Ni + Cu + Zn + Zn*Bedrock, data=geochem)
summary(bedrock7)
anova(bedrock7)
drop1(bedrock7, test="F")
par(mfrow=c(2,2))
plot(bedrock7)
AIC(bedrock1, bedrock2, bedrock3, bedrock4, bedrock5, bedrock6, bedrock7)

##DO models

DOonly1<-lm(DO ~ pH + Bedrock + SO4 + S + Fe + Mo + V + Mn + Co + Ni + Cu + Zn + Mg, data=geochem)
summary(DOonly1)
anova(DOonly1)
drop1(DOonly1, test="F")
par(mfrow=c(2,2))
plot(DOonly1)

##drop those with low F values

DOonly2<-lm(DO ~ Bedrock + SO4 + Fe + Mo + V + Co + Ni + Cu + Zn + Mg, data=geochem)
summary(DOonly2)
anova(DOonly2)
drop1(DOonly2, test="F")
par(mfrow=c(2,2))
plot(DOonly2)

##drop Zn, add interactions

DOonly3<-lm(DO ~ Bedrock + SO4 + Fe + Fe*pH + Fe+Bedrock + Mo + Mo*pH + V + Co + Ni + Cu + Mg, data=geochem)
summary(DOonly3)
anova(DOonly3)
drop1(DOonly3, test="F")
par(mfrow=c(2,2))
plot(DOonly3)

AIC(DOonly1, DOonly2, DOonly3)

lowest AIC: DOonly2

##remove interactions

DOonly4<-lm(DO ~ Bedrock + SO4 + Fe + Mo + V + Co + Ni + Cu + Mg, data=geochem)
summary(DOonly4)
anova(DOonly4)
drop1(DOonly4, test="F")
par(mfrow=c(2,2))
plot(DOonly4)

##remove Ni, Co, V, Bedrock

DOonly5<-lm(DO ~ SO4 + Fe + Mo + Cu + Mg, data=geochem)
summary(DOonly5)
anova(DOonly5)
drop1(DOonly5, test="F")
par(mfrow=c(2,2))
plot(DOonly5)

AIC(DOonly1, DOonly2, DOonly3, DOonly4, DOonly5)

lowest AIC: Doonly5

##remove Fe and Mg

DOonly6<-lm(DO ~ SO4 + Mo + Cu, data=geochem)
summary(DOonly6)
anova(DOonly6)
drop1(DOonly6, test="F")
par(mfrow=c(2,2))
plot(DOonly6)

AIC(DOonly1, DOonly2, DOonly3, DOonly4, DOonly5, DOonly6)

lowest AIC: Doonly6

##Temp

temp1<-lm(Temp ~ SO4 + S + Fe + Mo + V + Mn + Co + Ni + Cu + Zn + Mg, data=geochem)
summary(temp1)
anova(temp1)
drop1(temp1, test="F")
par(mfrow=c(2,2))
plot(temp1)

##remove S, Fe, Co, Ni, Cu

temp2<-lm(Temp ~ SO4 + Mo + V + Mn + Zn + Mg, data=geochem)
summary(temp2)
anova(temp2)
drop1(temp2, test="F")
par(mfrow=c(2,2))
plot(temp2)

##remove Zn

temp3<-lm(Temp ~ SO4 + Mo + V + Mn + Mg, data=geochem)
summary(temp3)
anova(temp3)
drop1(temp3, test="F")
par(mfrow=c(2,2))
plot(temp3)

AIC(temp1, temp2, temp3)

##

all1<-lm(pH~Temp + Bedrock + DO + Fe + Mg + Mn + Co + Zn + V + Ni + Cu + Mo, data = geochem)
summary(all1)

all2<-lm(pH~Temp + Bedrock + DO + SO4 + S + Fe + Mg + Mn + Co + Zn + V + Ni + Cu + Mo, data = geochem)
summary(all2)

all3<-lm(pH~Temp + Bedrock + DO + SO4 + S + Fe + DO*Fe + Mg + Mg*DO + Mn + Co + Zn + Zn*DO + V + V*DO + Ni + Cu + Mo + DO*Mo + Temp*Mo + Temp*Mn + Temp*Mg, data = geochem)
summary(all3)

all4<-lm(pH~Temp + Bedrock + DO + SO4 + S + Fe + DO*Fe + Bedrock*Fe + Mg + Mg*DO + Bedrock*Mg + Mn + Bedrock*Mn + Co + Bedrock*Co + Zn + Zn*DO + Bedrock*Zn + V + V*DO + Bedrock*V + Ni + Bedrock*Ni + Cu + Bedrock*Cu + Mo + DO*Mo + Temp*Mo + Bedrock*Mo + Temp*Mn + Bedrock*Mn + Temp*Mg + Bedrock*Mg, data = geochem)
summary(all4)
anova(all4)

all5<-lm(pH~Temp + Bedrock + SO4 + S + Fe + Bedrock*Fe + Bedrock*Mg + Mn + Bedrock*Mn + Bedrock*Co + Zn + Bedrock*Zn + Bedrock*V + Bedrock*Ni + Bedrock*Cu + Mo + DO*Mo + Temp*Mo + Bedrock*Mo + Temp*Mn + Bedrock*Mn + Bedrock*Mg, data = geochem)
summary(all5)
anova(all5)

all6<-lm(pH~Temp + Bedrock + SO4 + S + Fe + Bedrock*Fe + Bedrock*Mg + Mn + Bedrock*Mn + Zn + Bedrock*Zn + Bedrock*V + Bedrock*Ni + Mo + DO*Mo + Temp*Mo + Bedrock*Mo + Bedrock*Mn + Bedrock*Mg, data = geochem)
summary(all6)
anova(all6)

all7<-lm(pH~Temp + Bedrock + SO4 + S + Fe + Bedrock*Fe + Bedrock*Mg + Mn + Bedrock*Mn + Zn + Bedrock*Zn + Bedrock*Ni + Mo + DO*Mo + Bedrock*Mo + Bedrock*Mn + Bedrock*Mg, data = geochem)
summary(all7)
anova(all7)

all8<-lm(pH~Temp + Bedrock + SO4 + S + Fe + Bedrock*Fe + Bedrock*Mg + Mn + Bedrock*Mn + Zn + Bedrock*Zn + Mo + DO*Mo + Bedrock*Mo + Bedrock*Mn + Bedrock*Mg, data = geochem)
summary(all8)
anova(all8)

AIC(all1,all2,all3,all4,all5,all6,all7,all8)
AIC(all9)

all6 is best

all9<-lm(pH~Temp + Bedrock + SO4 + S + Fe + Bedrock*Fe + Bedrock*Mg + Mn + Bedrock*Mn + Zn + Bedrock*Zn + Bedrock*V + Bedrock*Ni + Mo + DO*Mo + Temp*Mo + Bedrock*Mo + SiO2*Mo + Bedrock*Mn + Bedrock*Mg, data = geochem)
summary(all9)
anova(all9)
