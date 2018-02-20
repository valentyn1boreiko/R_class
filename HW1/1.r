#Part 1
#a)
objekte = load("neuwal.RDATA")
objekte
rm(list = ls())


#b)
objekte = load("neuwal.RDATA")

#c)
lens =c(length(monat), length(oevp), length(fpoe), length(spoe), length(neos), length(gruene), length(pilz), length(n))
min(length(monat) == lens) == 1

#d)
tag = c(9,tag)
monat = c(10, monat)
oevp = c(33,oevp)
fpoe = c(27,fpoe)
spoe = c(23,spoe)
neos = c(6,neos)
gruene =c(5,gruene)
pilz = c(5,pilz)
n = c(1000,n)
#e)
vec = c(summary(n),sd(n))
vec

#f)
pilz[is.na(pilz)] = 0
others = 100-(oevp+fpoe+spoe+neos+gruene+pilz)

#Part 2

#a)
pears = cor(spoe,n,method="pearson")
spear = cor(spoe,n,method="spearman")
pears - spear

#b)
quantile_1 = 0.25
quantile_2 = 0.75
mean(spoe[ n < quantile(n, probs=quantile_1)])
mean(spoe[ n > quantile(n, probs=quantile_2)])

#c)
mit = mean(oevp)
cummit = cumsum(oevp) / c(1:length(oevp))
sum(cummit > mit)

#d)
sum(fpoe > spoe)/length(spoe) 
sep_fpoe = fpoe[c(monat >= 9)]
sep_spoe = spoe[c(monat >= 9)]
sum(sep_fpoe > sep_spoe)/length(sep_spoe) 

#e)
change = diff(spoe)
first_change = change[abs(change) == max(abs(change))]

#f)
second_change = change[abs(change) == max(abs(change[change != first_change]))]

#g)
tage = tag[(neos > gruene)]
monate = monat[(neos > gruene)]

#h)
paste(tag[(neos > gruene)],monat[(neos > gruene)],sep = ".")

#i)
tag_ = tag
tag_[which(neos > gruene)][tag[which(neos > gruene)]<10] = paste(0,tag[which(neos > gruene)][tag[which(neos > gruene)]<10],sep="")
monat_ = monat
monat_[which(neos > gruene)][monat[which(neos > gruene)]<10] = paste(0,monat[which(neos > gruene)][monat[which(neos > gruene)]<10],sep="")

paste(tag_[which(neos > gruene)],monat_[which(neos > gruene)],sep = ".")

#Part 3

#a)
oevp
sample = oevp
oevp
alpha = 0.01 
q_n = qnorm(1 - (alpha/2))
sch = sample/n
sd = sqrt(sch*(1-sch))
untere_g = sch - q_n * sd/sqrt(n)
obere_g = sch + q_n * sd/sqrt(n)
paste(untere_g,obere_g,sep = ",")

#b)
sdf = sd/sqrt(n*0.8)

#c)
mean_sdf = mean(sdf[(monat >= 8)])


#d)
#Es ist schon gemacht, wobei man kann einfach andere Partie bei sample hinschreiben.

#e)
ind = 1:50
equal = monat[ind]==monat[ind+1]
equal
equal[length(equal)] = FALSE
monat
dif = diff(monat)
dif
which(dif==-1)[1]
days_cum = c(which(dif==-1),length(monat)) 
days_cum
days_mon = c(which(dif==-1)[1], diff(days_cum)) 
days_mon
differences_umfrage = rev(n)*rep(seq(0.25, by=-0.02, length.out=50)[1:length(n[!equal])], times = rev(days_mon)) 
differences_umfrage
eff = rev(n)-differences_umfrage #Effiziente Stichprobe

#Part 4

#a)
prop.test(x = c(fpoe[1], fpoe[2]), n = c(n[1], n[2])) #Difference is not significant

#b)
randd = sample(1:length(fpoe), 2, replace=FALSE)
prop.test(x = c(fpoe[randd[1]], fpoe[randd[2]]), n = c(n[randd[1]], n[randd[2]]), conf.level=0.9) #Difference is not significant

#c)
umfrage_sept = fpoe[(monat==9)]  

umfrage_okt = fpoe[(monat==10)] 
t.test( fpoe[monat==10], fpoe[monat==9], conf.level = 0.99)  #Difference is not significant

