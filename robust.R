### Analyse robuste ----
library(robust)
library(tidyverse)
library(dplyr)

data<-df
dfquanti <- data %>% select(where(is.numeric)) #partition df quantitatif
dfquali <- data %>% select(where(is.character) | where(is.factor)) #partition df qualitatif

for(i in 1:ncol(dfquali)){
  tt <- table(dfquali[,i]) %>% as.data.frame() %>% arrange(desc(Freq))
  print(slice_head(tt, n = 8))
  print(nrow(tt))
} 
# analyse univarier avec methode des 3 MAD
for(i in 1:ncol(dfquanti)){
   DD <- as.data.frame(dfquanti[,i]) %>% filter(dfquanti[,i] > 0)
   med <- median(DD[,1],na.rm = TRUE)
   mad <- mad(DD[,1], na.rm = TRUE)
   h1 <- med - 3*mad
   h2 <- med + 3*mad
   title <- colnames(dfquanti)[i]
   plot(DD[,1], main = title, xlab = "", ylab ="")
   abline(h=h1, col = 'red')
   abline(h=h2, col = 'red')
} 
#detection des outliers 
obs.extr <- c()
obs.ext <- c()
for(i in 1:ncol(dfquanti)){
  DD <- as.data.frame(dfquanti[,i]) %>% filter(dfquanti[,i]>0)
  med <- median(DD[,1], na.rm = TRUE)
  mad <- mad(DD[,1], na.rm = TRUE)
  cutoff <- med + 3*mad
  n <- filter(dfquanti,dfquanti[,i] >= cutoff) %>% nrow()
  obs.extr <- c(obs.extr,n)
  
  ext <- filter(dfquanti,is.na(dfquanti[,i])) %>% nrow()
  cutoff_b<-med-3*mad
  m <- filter(dfquanti,dfquanti[,i] <= cutoff_b) %>% nrow()
  obs.ext <- c(obs.ext,m)
  dfquanti <- filter(dfquanti, dfquanti[,i] < cutoff)
  dfquanti <- dfquanti %>% select(where(is.numeric))
}

tab <- data.frame(obs.extr, obs.ext, row.names = colnames(dfquanti))

tab$Total <- tab$obs.extr + tab$obs.ext
tab[nrow(tab)+1,] <- c(sum(as.integer(tab$obs.extr)),sum(as.integer(tab$obs.ext)),as.integer(sum(tab$Total)))
rownames(tab) <- c(colnames(dfquanti),"Total")
print(xtable(tab))

### Robustesse multidimensionnelle ----
data<-df
dfquanti <- data %>% select(where(is.numeric))
dfquali <- data %>% select(where(is.character) | where(is.factor))

MCD <- dfquanti %>% covRob(estim = "mcd", na.action = na.omit)# utlisation du MCD pour la robustesse
distrob=sqrt(mahalanobis(dfquanti, MCD$center, MCD$cov)) #calcul distance de mahnobis
par(mfrow = c(1,1))
plot(distrob, ylab="Robust Mahalanobis distances")
cutoff=sqrt(qchisq(0.975,df=ncol(dfquanti)))
abline(h=cutoff, col="red")
#identify(distrob, labels=row.names(data), cex=1.5)
New_db <- cbind(dfquanti, dfquali)
rejected <- New_db %>% filter(distrob >cutoff) %>% nrow()
New_db <- New_db %>% filter(distrob <=cutoff) %>% select(-distrob)
dfquanti <- New_db %>% select(where(is.numeric))
dfquali <- New_db %>% select(where(is.character) | where(is.factor))





