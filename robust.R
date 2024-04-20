### Analyse robuste ----
library(robust)
library(tidyverse)
library(dplyr)

db_final$Seniority <- as.numeric(db_final$Seniority)
data<-db_final
dfquanti <- data %>% select(where(is.numeric))
dfquali <- data %>% select(where(is.character) | where(is.factor))

for(i in 1:ncol(dfquali)){
  tt <- table(dfquali[,i]) %>% as.data.frame() %>% arrange(desc(Freq))
  print(slice_head(tt, n = 8))
  print(nrow(tt))
}

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
obs.extr <- c()
obs.nas <- c()
for(i in 1:ncol(dfquanti)){
  DD <- as.data.frame(dfquanti[,i]) %>% filter(dfquanti[,i]>0)
  med <- median(DD[,1], na.rm = TRUE)
  mad <- mad(DD[,1], na.rm = TRUE)
  cutoff <- med + 3*mad
  n <- filter(dfquanti,dfquanti[,i] >= cutoff) %>% nrow()
  obs.extr <- c(obs.extr,n)
  nas <- filter(dfquanti,is.na(dfquanti[,i])) %>% nrow()
  obs.nas <- c(obs.nas,nas)
  dfquanti <- filter(dfquanti, dfquanti[,i] < cutoff)
  dfquanti <- dfquanti %>% select(where(is.numeric))
}
tab <- data.frame(obs.extr, obs.nas, row.names = colnames(dfquanti))

tab$Total <- tab$obs.extr + tab$obs.nas
tab[nrow(tab)+1,] <- c(sum(as.integer(tab$obs.extr)),sum(as.integer(tab$obs.nas)),as.integer(sum(tab$Total)))
rownames(tab) <- c(colnames(dfquanti),"Total")
print(xtable(tab))

### Robustesse multidimensionnelle ----
data<-db_final
dfquanti <- data %>% select(where(is.numeric))
dfquali <- data %>% select(where(is.character) | where(is.factor))

n_distinct(dfquanti)

MCD <- dfquanti %>% covRob(estim = "mcd", na.action = na.omit)
distrob=sqrt(mahalanobis(dfquanti, MCD$center, MCD$cov))
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





