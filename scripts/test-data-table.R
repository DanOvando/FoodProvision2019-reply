
input <- if (file.exists("flights14.csv")) {
  "flights14.csv"
} else {
  "https://raw.githubusercontent.com/Rdatatable/data.table/master/vignettes/flights14.csv"
}
flights <- fread(input)
flights

wtf <- c(1,2,3)

a <- flights[,..wtf]


k_per_cell <- as.data.table(KprotectedPerCell_Library)

mpa_index <- which(MPAselect0 == 1)

mpa_pre <-
  data.table(matrix(rep(
    rowSums(k_per_cell[, ..mpa_index]), length(celltoiterate)
  ), nrow = nrow(k_per_cell), ncol = length(celltoiterate)))
  
k_per_cell <- t(k_per_cell)

mpa_pre = t(mpa_pre)

a <- Sys.time()
result <- foreach(iter = 1:100, .combine = rbind) %dopar% {
  
  a <- Sys.time()
  
  R <- mpa_pre + k_per_cell[celltoiterate,]
  
  ER_redistribute1 <- 1 - (1 - ER) ^ (1 / (1 - R))
  
  hmpa <-
    (ER_redistribute * ((m * K * (1 - R)) / ((ER_redistribute * R) +
                                                      m)) * (1 - ((
                                                        ER_redistribute * (1 - R) * m
                                                      ) / (((ER_redistribute * R) + m
                                                      ) * r))))
  hmpa <- hmpa * (hmpa > 0)
  HMPA <- rowSums(hmpa)
  d = HMPA - HBAU
  Sys.time() - a
  
}
Sys.time() - a


b <- Sys.time()

result <- foreach(iter = 1:length(celltoiterate), .combine = rbind) %dopar% {

  R<-MPAselectPrev+KprotectedPerCell_Library[,celltoiterate[iter]]
  ER_redistribute<-1-(1-ER)^(1/(1-R))
  
  
  hmpa<-na.omit(ER_redistribute*((m*K*(1-R))/((ER_redistribute*R)+m))*(1-((ER_redistribute*(1-R)*m)/(((ER_redistribute*R)+m)*r))))
  hmpa<-hmpa*(hmpa>0)
  HMPA<-sum(hmpa)
  HMPA-HBAU
}

Sys.time() - b
