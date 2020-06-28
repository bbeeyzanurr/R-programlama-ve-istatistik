
# R-programlama-ve-istatistik
uretilen_veri_sayisi <-  20
U           <-  runif(uretilen_veri_sayisi) ###uniform dagilimdan uretilmis 20 rastgele sayi 
x           <- (((1/(1-U))^0.5)-1)^0.33 ### bu sayılardan ters dönüşüm sonucu elde edilen fonksiyon aracılığıyla elde edilen veriler 
x <- matrix(x)
xort<-mean(x)
xsd<-sd(x)
xmed<-median(x)
x_deg_kat <- (100*xsd/xort)
n=dim(x)[1] #x in boyutu: örnek hacmi
B=1000 #boostrap örnek sayısı
xornek=matrix(rep(0,B*n),ncol=n) # ornekleri yazağımız boş matris
xortb=matrix(rep(0,B),ncol=1) # örnek ortalamaları için boş matris
xsdb=matrix(rep(0,B),ncol=1) # örnek standart sapmaları için boş matris
xmedb=matrix(rep(0,B),ncol=1) # örnek medyanları için boş matris
x_deg_katt = matrix(rep(0,B),ncol = 1) # örnek degisim katsayilari için boş matris

## bootstrap örneklerinin seçimi, seçilen örneklerin ortalama, standart sapma ve medyan hesabı
for(i in 1:B)
{
  xornek[i,]=sample(x,n,replace = TRUE) #bootstrap örnekleri
  xortb[i]=mean(xornek[i,]) #bootstrap örnek ortalamaları
  xsdb[i]= sd(xornek[i,]) #bootstrap örnek standart sapmaları
  xmedb[i]=median((xornek[i,])) #bootstrap örnek medyanları
  x_deg_katt[i]= (100*xsdb[i]/xortb[i]) # bootstrap ornek degisim katsayilari 
}

#seçilen bootstrap örneklerinin ortalamasına, standart sapmasına ve medyana ilişkin #histogramlar
par(mfrow=c(1,3)) #grafikler için 1 satır 3 sütunluk yer aç
hist(xortb,main="Ortalama",prob=TRUE)
hist(xsdb,main="Standart Sapma",prob=TRUE)
hist(xmedb,main="Medyan",prob=TRUE)
#Güven aralıklarının oluşturulması
sxort=sort(xortb) # bootstrap örnek ortalamalarının sıralanması
gaa=sxort[B*(0.05)] # bootstrap güven aralığının alt sınırı
gau=sxort[B*(0.95)] # bootstrap güven aralığının alt sınırı
xortGA=c(gaa, gau) # örnek ortalamasının bootstrap güven aralığı
ssd=sort(xsdb) # bootstrap örnek standart sapmalarının sıralanması
sdGA=c(ssd[B*(0.05)],ssd[B*(0.95)]) #örnek standart sapmasının bootstrap güven aralığı
smed=sort(xmedb) # bootstrap örnek medyanlarının sıralanması
medGA=c(smed[B*(0.05)],smed[B*(0.95)])#örnek medyanının bootstrap güven aralığı
sdeg=sort(x_deg_katt) # bootstrap örnek dedgisim katsayilarinin sıralanması
deg_GA=c(sdeg[B*(0.05)],sdeg[B*(0.95)])#örnek örnek dedgisim katsayilarinin bootstrap güven aralığı
