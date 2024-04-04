### Analyse robuste ----
data<-db_final
dfquanti <- data %>% select(where(is.numeric))
dfquali <- data %>% select(where(is.character))
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