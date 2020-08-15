#PROJEKTNA NALOGA PRI PREDMETU STATISTIKA

library(lattice)
library(plyr)
library(Rmisc)
library(gplots)
library(plotrix)
library(ggplot2)

#1. naloga 

#V datoteki Kibergrd se nahajajo informacije
# o 48.886 družinah, ki stanujejo v mestu Kobarid


setwd("~/Documents/FAKS/Statistika/Statistika---seminarska-naloga/Projekt")
data1 <- read.table("Kibergrad.csv", header = TRUE, sep = ",")
N <- nrow(data1)  #stevilo vseh podatkov

#a) izberite anostavni slučani vzorec 200 družin in na
#njegovi podlagi ocenite povprečno stevilo 
#otrok na družino v Kobaridu

n <- 200    #velikost vzorca
index <- sample(1:N, n)
vzorec <- data1[index,]

# Enostavni slucajni vzorec 200 ljudi
vzorec <- data1[sample(1:N, n),]
st_otrok_v <- vzorec$OTROK
povp_st_otrok <- mean(st_otrok_v)
povp_st_otrok

#b) Ocenite standardno napako in postavite 95% interval zaupanja.
napaka <- sqrt(sum(mapply(st_otrok_v,FUN =function(x) (povp_st_otrok-x)^2))*1/n^2*(1-n/N))
napaka
alfa = 0.05
koef = qnorm(1 - alfa / 2)
sp_meja <- povp_st_otrok - koef * napaka
zg_meja <- povp_st_otrok + koef * napaka

sp_meja
zg_meja

#c) Vzorčno povprečje in ocenjeno standardno napako primerjajte s populacijskim 
#povprečjem in pravo standardno napako. Ali interval zaupanja iz prejšnje točke 
#pokrije populacijsko povprečje?

st_otrok_p <- data1$OTROK
povp_st_otrok_p <- mean(st_otrok_p)
napaka_p <- sqrt(sum(mapply(st_otrok_p,FUN =function(x) (povp_st_otrok_p-x)^2))*1/N^2*(1-N/N))
povp_st_otrok_p
napaka_p
abs(povp_st_otrok_p - povp_st_otrok)
abs(napaka_p - napaka)

#d) Vzemite še 99 enostavnih slučajnih vzorcev in prav tako za vsakega določite 
#95% interval zaupanja. Narišite intervale zaupanja, ki pripadajo tem 100 vzorcem. 
#Koliko jih pokrije populacijsko povprečje?

# Standardne napake in povprecja za vseh 100 vzorcev
povprecja <- povp_st_otrok
napake <- napaka

for (i in 2:100) {
  index_i <- sample(1:nrow(data1), n)
  vzorec_i <- data1[index_i,]
  otroci_i <- vzorec_i$OTROK
  povprecje_i <- sum(otroci_i) / n 
  povprecja <- c(povprecja, povprecje_i)
  napaka_i <- sqrt(sum(mapply(otroci_i,FUN =function(x) (povprecje_i-x)^2))*1/n^2*(1-n/N))
  napake <- c(napake, napaka_i)
}
napake

# Spodnja meja za interval zaupanja
sp_meje <- povprecja[1] - koef * napake[1] 
for (i in 2:100) {
  sp_i <- povprecja[i] - koef * napake[i]
  sp_meje <- c(sp_meje, sp_i)
}

# Zgornja meja za interval zaupanja
zg_meje <- povprecja[1] + koef * napake[1] 
for (i in 2:100) {
  zg_i <- povprecja[i] + koef * napake[i]
  zg_meje <- c(zg_meje, zg_i)
}

###

# Zdruzimo podatke v tabelo
podatki <- cbind(sp_meje, zg_meje)
podatki

# Koliko intervalov zaupanja pokrije populacijski delez: 
# popul delez mora biti torej vecji od spodnje meje in hkrati manjsi od zgornje.

t <- 0  #stevilo int zaupanja, ki pokrijejo pop. delez
for (i in 1:100) {
  if (podatki[,1][i] < povp_st_otrok_p && podatki[,2][i] > povp_st_otrok_p) {
    t <- t + 1
  }
}

t

#intervale zaupanja shranimo v csv datoteko
write.table(podatki, file="intervali_zaupanja.csv", quote=FALSE, col.names=c('Zgornja meja', 'Spodnja meja'), row.names=FALSE,sep=",")

#e) Izračunajte standardni odklon vzorčnih povprečij za 100 prej dobljenih vzorcev.
#Primerjajte s pravo standardno napako za vzorec velikosti 200.

standardni_odklon <- 0
for (i in 1:100) {
  standardni_odklon <- standardni_odklon + (povprecja[i]  - mean(povprecja))^2
}
standardni_odklon <- sqrt(standardni_odklon / 100)

standardni_odklon
napaka

abs(standardni_odklon - napaka)

#f) Izvedite prejšnji dve točki še na 100 vzorcih po 800 družin. Primerjajte in 
#razložite razlike s teorijo vzorčenja.

n2 <- 800 
vzorec2 <- data1[sample(1:N, n2),] # Enostavni slucajni vzorec 800 ljudi
st_otrok_v2 <- vzorec2$OTROK
povp_st_otrok2 <- mean(st_otrok_v2)
napaka2 <- sqrt(sum(mapply(st_otrok_v2,FUN =function(x) (povp_st_otrok2-x)^2))*1/n2^2*(1-n2/N))

povprecja2 <- povp_st_otrok2
napake2 <- napaka2


for (i in 2:100) {
  index_i <- sample(1:nrow(data1), n2)
  vzorec_i <- data1[index_i,]
  otroci_i <- vzorec_i$OTROK
  povprecje_i <- sum(otroci_i) / n2 
  povprecja2 <- c(povprecja2, povprecje_i)
  napaka_i <- sqrt(sum(mapply(otroci_i,FUN =function(x) (povprecje_i-x)^2))*1/n2^2*(1-n2/N))
  napake2 <- c(napake2, napaka_i)
}

# Spodnja meja za interval zaupanja
sp_meje2 <- povprecja2[1] - koef * napake2[1] 
for (i in 2:100) {
  sp_i <- povprecja2[i] - koef * napake2[i]
  sp_meje2 <- c(sp_meje2, sp_i)
}

# Zgornja meja za interval zaupanja
zg_meje2 <- povprecja2[1] + koef * napake2[1] 
for (i in 2:100) {
  zg_i <- povprecja2[i] + koef * napake2[i]
  zg_meje2 <- c(zg_meje2, zg_i)
}

# Zdruzimo podatke v tabelo
podatki2 <- cbind(sp_meje2, zg_meje2)
podatki2
povp_st_otrok_p

t2 <- 0  #stevilo int zaupanja, ki pokrijejo pop. delez
for (i in 1:100) {
  if (podatki2[,1][i] < povp_st_otrok_p && podatki2[,2][i] > povp_st_otrok_p) {
    t2 <- t2 + 1
  }
}

t2

write.table(podatki2, file="intervali_zaupanja2.csv", quote=FALSE, col.names=c('Zgornja meja', 'Spodnja meja'), row.names=FALSE,sep=",")
standardni_odklon2 <- 0
for (i in 1:100) {
  standardni_odklon2 <- standardni_odklon2 + (povprecja2[i]  - mean(povprecja2))^2
}
standardni_odklon2 <- sqrt(standardni_odklon2 / 100)

standardni_odklon2
napaka2

abs(standardni_odklon2 - napaka2)
