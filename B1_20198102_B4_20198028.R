if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install()
BiocManager::install("antiProfilesData")
datasetName <-antiProfilesData::apColonData

bm=datasetName
pdata=pData(bm)
edata=exprs(bm)
fdata=fData(bm)

#Q1
class(datasetName)
#a
lapply(edata, class)
lapply(pdata, class)
lapply(fdata, class)

#b
colnames(edata)
rownames(edata)

colnames(pdata)
rownames(pdata)

colnames(fdata)
rownames(fdata)

#c
summary(pdata)
summary(edata)
summary(fdata)

#d
table(edata,useNA="ifany")
table(pdata,useNA="ifany")
table(fdata,useNA="ifany")

#e
cor(edata[,c(1:10)])
cov(edata[,c(1:10)])
col<- colorRampPalette(c("deeppink", "pink", "darkblue"))(20)
heatmap(cor(edata[,c(1:10)]), col = col, symm = TRUE)

#f
GSM95478 <- edata[,"GSM95478"]
GSM95473 <- edata[,"GSM95473"]
plot(GSM95473,GSM95478,col=c("darkmagenta","blue4"))
relation <- lm(GSM95478 ~ GSM95473)
abline(relation,lwd=4)

#Q2
#data normalization
edata = log2(edata + 1-min(edata))
edata_centered = edata - rowMeans(edata)
svd1<-svd(edata_centered)
edata_centered2 = t(t(edata) - colMeans(edata))
svd2 = svd(edata_centered2)
pc1<-prcomp(edata_centered2)
plot(pc1$rotation[,1],svd2$v[,1],col=2)

#Q3.1
zodiac<-as.factor(c(rep("Aries",29),
                    rep("Taurus",24),
                    rep("Gemini",22),
                    rep("Cancer",19),
                    rep("Leo",21),
                    rep("Virgo",18),
                    rep("Libra",19),
                    rep("Scorpio",20),
                    rep("Sagittarius",23),
                    rep("Capricorn",18),
                    rep("Aquarius",20),
                    rep("Pisces",23)))
chisq.test(table(zodiac))

'''question 3.2
H0:zodiac signs are evenly distributed across visual artists
H1:zodiac signs arenot evenly distributed across visual artists 
since p-value >0.05, H0 is accepted and H1 is rejected'''

#Q4
edata <- log2(edata+1-min(edata))

dist1 = dist(t(edata[,c(1:10)]))
hclust1 = hclust(dist1)
plot(hclust1,hang = -1) 

dim(edata)
k1 <- kmeans(edata,centers = 68)
table(k1$cluster)

k2 <- kmeans(edata,centers = 3)
table(k2$cluster)
