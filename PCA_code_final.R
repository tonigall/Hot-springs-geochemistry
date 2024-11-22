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
fviz_eig(pca_res, addLabels = TRUE)
fviz_cos2(pca_res, choice = "var", axes = 1:2)
fviz_pca_var(pca_res, col.var = "black",repel = TRUE)
fviz_pca_biplot(pca_res,  label = "var", repel = TRUE, col.var = "black", col.ind = "blue")

##plot PCA

geochem$Bedrock<-factor(geochem$Bedrock, levels = c("Basaltic", "Basaltic/Sandstone", "Basaltic/Rhyolitic", "Andesitic/Dacitic", "Rhyolitic/Basaltic", "Rhyolitic/Tuff", "Rhyolitic/Greywacke", "Rhyolitic/Siltstone", "Rhyolitic", "Glacial deposits", "Lacustrine deposits"))

trim.data$Bedrock<-factor(trim.data$Bedrock, levels = c("Basaltic", "Basaltic/Sandstone", "Basaltic/Rhyolitic", "Andesitic/Dacitic", "Rhyolitic/Basaltic", "Rhyolitic/Tuff", "Rhyolitic/Greywacke", "Rhyolitic/Siltstone", "Rhyolitic", "Glacial deposits", "Lacustrine deposits"))

pcaplot<-autoplot(pca_res, data=trim.data, loadings = TRUE, loadings.label = TRUE, , loadings.color = "black")
pcaplot + geom_point(aes(colour=Bedrock)) + scale_colour_carto_d(palette = "Safe") +
theme_classic()

##models

model1<-lm(pH~Temp + Bedrock + DO + Fe + Mg + Mn + Co + Zn + V + Ni + Cu + Mo, data = geochem)
summary(model1)
anova(model1)
drop1(model1, test="F")
par(mfrow=c(2,2))
plot(model1)

model2<-lm(pH~Temp + Bedrock + DO + SO4 + S + Fe + Mg + Mn + Co + Zn + V + Ni + Cu + Mo, data = geochem)
summary(model2)
anova(model2)
drop1(model2, test="F")
par(mfrow=c(2,2))
plot(model2)

model3<-lm(pH~Temp + Bedrock + DO + SO4 + S + Fe + DO*Fe + Mg + Mg*DO + Mn + Co + Zn + Zn*DO + V + V*DO + Ni + Cu + Mo + DO*Mo + Temp*Mo + Temp*Mn + Temp*Mg, data = geochem)
summary(model3)
anova(model3)
drop1(model3, test="F")
par(mfrow=c(2,2))
plot(model3)

model4<-lm(pH~Temp + Bedrock + DO + SO4 + S + Fe + DO*Fe + Bedrock*Fe + Mg + Mg*DO + Bedrock*Mg + Mn + Bedrock*Mn + Co + Bedrock*Co + Zn + Zn*DO + Bedrock*Zn + V + V*DO + Bedrock*V + Ni + Bedrock*Ni + Cu + Bedrock*Cu + Mo + DO*Mo + Temp*Mo + Bedrock*Mo + Temp*Mn + Bedrock*Mn + Temp*Mg + Bedrock*Mg, data = geochem)
summary(model4)
anova(model4)
drop1(model4, test="F")
par(mfrow=c(2,2))
plot(model4)

model5<-lm(pH~Temp + Bedrock + SO4 + S + Fe + Bedrock*Fe + Bedrock*Mg + Mn + Bedrock*Mn + Bedrock*Co + Zn + Bedrock*Zn + Bedrock*V + Bedrock*Ni + Bedrock*Cu + Mo + DO*Mo + Temp*Mo + Bedrock*Mo + Temp*Mn + Bedrock*Mn + Bedrock*Mg, data = geochem)
summary(model5)
anova(model5)
drop1(model5, test="F")
par(mfrow=c(2,2))
plot(model5)

model6<-lm(pH~Temp + Bedrock + SO4 + S + Fe + Bedrock*Fe + Bedrock*Mg + Mn + Bedrock*Mn + Zn + Bedrock*Zn + Bedrock*V + Bedrock*Ni + Mo + DO*Mo + Temp*Mo + Bedrock*Mo + Bedrock*Mn + Bedrock*Mg, data = geochem)
summary(model6)
anova(model6)
drop1(model6, test="F")
par(mfrow=c(2,2))
plot(model6)

model7<-lm(pH~Temp + Bedrock + SO4 + S + Fe + Bedrock*Fe + Bedrock*Mg + Mn + Bedrock*Mn + Zn + Bedrock*Zn + Bedrock*Ni + Mo + DO*Mo + Bedrock*Mo + Bedrock*Mn + Bedrock*Mg, data = geochem)
summary(model7)
anova(model7)
drop1(model7, test="F")
par(mfrow=c(2,2))
plot(model7)

model8<-lm(pH~Temp + Bedrock + SO4 + S + Fe + Bedrock*Fe + Bedrock*Mg + Mn + Bedrock*Mn + Zn + Bedrock*Zn + Mo + DO*Mo + Bedrock*Mo + Bedrock*Mn + Bedrock*Mg, data = geochem)
summary(model8)
anova(model8)
drop1(model8, test="F")
par(mfrow=c(2,2))
plot(model8)

model9<-lm(pH~Temp + Bedrock + SO4 + S + Fe + Bedrock*Fe + Bedrock*Mg + Mn + Bedrock*Mn + Zn + Bedrock*Zn + Bedrock*V + Bedrock*Ni + Mo + DO*Mo + Temp*Mo + Bedrock*Mo + SiO2*Mo + Bedrock*Mn + Bedrock*Mg, data = geochem)
summary(model9)
anova(model9)
drop1(model9, test="F")
par(mfrow=c(2,2))
plot(model9)

AIC(model1,model2,model3,model4,model5,model6,model7,model8,model9)

