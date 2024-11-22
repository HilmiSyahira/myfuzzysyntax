#library package ( jika belum memiliki library package anda bisa mendownload dengan 'install.package("readxl")')
library(readxl)

#input data
data_input = read_excel("D:/UNDIP/SKRIPSI 2/data/chen manual minyak mentah.xlsx") #Ganti "D:/UNDIP/SKRIPSI 2/data/chen manual minyak mentah.xlsx" dengan lokasi file Anda.
data <- ts(data_input)
data

# Membentuk interval
interval_data <- function(data,  D1 = 0, D2 = 0) {
  Umin <- min(data) - D1 
  Umax <- max(data) + D2 
  n <- round(1 + 3.322 * log10(length(data)))
  l <- (Umax - Umin) / n 
  intervals <- data.frame(mins = 0, maxs = 0)
  intervals[1, 1] <- Umin
  intervals[1, 2] <- Umin + l
  
  for (i in 2:n) {
    intervals[i, 1] <- intervals[i - 1, 2]
    intervals[i, 2] <- intervals[i, 1] + l
  }
  
  return((intervals = intervals))
}
interval = interval_data(data,D1=0, D2=0) #nilai D1 dan D2 bisa diubah berdasarkan keinginan peneliti
interval

# Mencari nilai rata-rata sub interval
m <- as.vector(rowMeans(interval))
Nilai_Tengah  = cbind(interval,m)
Nilai_Tengah 

#Fuzzifikasi
fuzzifikasi <- c()
for (i in 1:length(data)) {
  for (j in 1:nrow(interval)) {
    if (i != which.max(data)) {
      if (data[i] >= interval[j, 1] & data[i] < interval[j, 2]) {
        fuzzifikasi[i] <- j
        break
      }
    } else {
      if (data[i] >= interval[j, 1] & data[i] <= interval[j, 2]) {
        fuzzifikasi[i] <- j
      }
    }
  }
}
Hasil_fuzzifikasi = cbind(data,fuzzifikasi)
Hasil_fuzzifikasi

#membuat fuzzy relationship
flr <- data.frame(current_state=0, next_state=0)
for(i in 1:length(fuzzifikasi)){
  if(i < length(fuzzifikasi)){
    flr[i,]=c(fuzzifikasi[i],fuzzifikasi[i+1])
  }
  else {
    flr[i,]=c(fuzzifikasi[i],0)
  }
}

#membuat fuzzy relationship Grup
flrg <- list()
for(i in 1:nrow(interval)) {
  flrgi <- c()
  has_transition <- FALSE  
  
  for (j in 1:(length(data)-1)) {
    if(flr[j,1] == i) {
      flrgi <- c(flrgi, flr[j,2])
      has_transition <- TRUE
    }
  }
  
  # Jika tidak ada transisi untuk state i, isi dengan 0
  if(!has_transition) {
    flrgi <- c(0)
  }
  
  flrg[[i]] <- flrgi
}
uni <- list()
for (i in 1:nrow(interval)){
  y <- flrg[[i]]
  r <- unique(y)
  uni[[i]] <- r
}
cat("Fuzzy Logic Relationship Grup : \n")
print(uni)

#Defuzzifikasi  
jum <- list()
for (i in 1:nrow(interval)){
  jums <- c()
  for (j in 1:length(uni[[i]])) {
    jums <- c(jums, m[uni[[i]][[j]]])
  }
  if (length(jums) == 0) {
    jum[[i]] <- m[i]  
  } else {
    jum[[i]] <- jums
  }
}
meanpred <- lapply(jum, mean)
meanpred

#Hasil Peramlan 
prediksi <- c()
for (i in 1:length(data)){
  if (i == 1){
    pred <- NA 
  }
  else {
    pred <- meanpred[[fuzzifikasi[(i-1)]]]
  }
  prediksi <- c(prediksi, pred)
}
Hasil_Peramalan = cbind(data,prediksi)
Hasil_Peramalan

#Peramalan 1 Periode kedepan
ramal <- meanpred[[fuzzifikasi[length(data)]]]
ramal

#Ketepatan hasil
Error = c()
for (i in 1:length(prediksi)){
  if (i == 0){
    galat = NA
  }
  else {
    galat = data-prediksi
  }
}
error = round(galat,3)
MSE = mean(galat^2,na.rm = TRUE)
RMSE= sqrt(mean(galat^2,na.rm = TRUE))
MAD = mean((abs(galat)),na.rm = TRUE)
MAPE = mean(abs(galat/data*100),na.rm = TRUE)
ketepatan_peramalan= cbind(MSE,RMSE,MAD,MAPE)
ketepatan_peramalan

#Grafik perbandingan data aktual dan hasil peramalan
plot( data, type = "l", col = "blue", lwd = 2, ylab = "Nilai", xlab = "Periode", main = "Perbandingan Data Aktual dan Prediksi")
lines( prediksi, col = "red", lwd = 2)
legend("topleft", legend = c("Data Aktual", "Data Prediksi"), col = c("blue", "red"), lty = 1, lwd = 2, bty="n")
  