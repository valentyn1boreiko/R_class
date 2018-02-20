#1)
install.packages("lsr")
library("lsr")
kreuz = function(x,y, rand_hf = FALSE, relat = FALSE, zeilen_pro = FALSE, spalten_pro = FALSE, mit_pro_zeichen = FALSE, genauigkeit = 2, missing = FALSE, max_levs = 2000, max_zh = 20) 
  {
  
  rel = relat+zeilen_pro+spalten_pro
  if(rel > 1)
    stop("Entweder relat oder zeilen oder spalten\n")
  if (rel + mit_pro_zeichen > 2)
    warning("No prozent Zeichen\n")
  if(length(x) != length(y)){
    stop("Die Lange von zwei Vektoren muss übereinstimmen\n")
  }
  x = as.factor(x)
  y = as.factor(y)
  merkmale = list(x,y)
  if (nlevels(x) < 2 || nlevels(y) < 2)
    stop("Nlevels < 2 \n")
  for (i in 1:length(merkmale)) {
    z = merkmale[[i]]
    if(sum(nchar(levels(z)) > max_zh) > 0)
       levels(merkmale[[i]]) = abbreviate(levels(z), minlength = max_zh)
    
    if (!is(tryCatch(as.numeric(levels(z)), error = function(e) e, warning = function(w) w),"warning"))
      {
      if (length(levels(z)) > max_levs){
        levels(merkmale[[i]]) = cut(as.numeric(levels(z)), breaks = max_levs)
      }
    }
    
    }
  #print(paste("Boom2 ",nlevels(x)," ",nlevels(y)))
  x = as.factor(merkmale[[1]])
  y = as.factor(merkmale[[2]])
  if (nlevels(x) < 2 || nlevels(y) < 2)
    stop("Nlevels < 2 \n")
  if(length(x) != length(y)){
    stop("Die Lange von zwei Vektoren muss übereinstimmen\n")
  }
  res = table(x,y)
  
  if (sum(res < 5)/length(res) > 0.2)
    warning("Zellenhaeufigkeit sind zu gering\n")
  if (missing)
    res = table(x,y, useNA = "ifany")
  
  if (rel){
    bool = ((relat)&&(res = round(prop.table(res)*100,genauigkeit)) || (zeilen_pro)&&(res = round(prop.table(res,1)*100,genauigkeit)) || (spalten_pro)&&(res = round(prop.table(res,2)*100,genauigkeit)))
    if (rand_hf)
      res = addmargins(res)
    if (mit_pro_zeichen)
      res[,] = paste0(res,"%")
  }
  else
    if (rand_hf)
      res = addmargins(res)
  return(list(table = res, chi_squared_wert = chisq.test(x,y)$statistic, chi_squared_p = chisq.test(x,y)$p.value, cramersV = cramersV(x,y)))
}

#2)
kreuz(iris$Species, iris$Sepal.Width, rand_hf = TRUE,genauigkeit = 3,mit_pro_zeichen =TRUE ,relat  = TRUE, max_zh=4)
kreuz(iris$Species, iris$Sepal.Length, rand_hf = TRUE,genauigkeit = 3,spalten_pro  = TRUE, max_zh=3)
kreuz(iris$Species, iris$Sepal.Length, rand_hf = TRUE,genauigkeit = 5)
kreuz(iris$Species[1:50], iris$Sepal.Length[1:20], rand_hf = TRUE,genauigkeit = 5)
kreuz(iris$Species, iris$Sepal.Length, rand_hf = TRUE, max_levs = 2)
#3)
load("school.RDATA")
mat = matrix(nrow=length(school),ncol = length(school))
ind1 = c()
ind2 = c()
for (i in 1:(length(school)-1)){
  for (j in (i+1):length(school)){
    
    a = school[[i]]
    b = school[[j]]
    if ( (!any(is.na(a)) && sum(diff(as.numeric(a))) == 0) || (!any(is.na(b)) && sum(diff(as.numeric(b))) == 0) || nlevels(as.factor(a)) < 2 || nlevels(as.factor(b)) < 2 || i == 13) {
      ind1 = c(ind1,i)
      ind2 = c(ind2,j)
    }
    else{
      print(paste("done ",i," ",j))
      mat[i,j] = kreuz(school[[i]],school[[j]])$chi_squared_wert
    }
  }
}
indices = cbind(ind1,ind2)
indices
mat[1:3,1:5]
#4)
rm(list=ls())
file = "verleihungen.dat"
#a)
tab = read.table(file,header=TRUE, sep = ";")
head(tab)
start = as.POSIXct(tab[["Verleihung"]])
#b)
end = start + tab[["Verleihzeit.in.Minuten"]]*60
#c)
geb = tab[["Geburtsdatum..TT.MM.JJJJ."]]
bool = list(
  bool_1 = grepl("\\d{2}\\.\\d{2}\\.\\d{4}",geb),
  bool_2 = grepl("\\d{2}\\.,\\d{2}\\.\\d{4}",geb),
  bool_3 = geb == "-",
  bool_4 = grepl("\\d{2}/\\d{2}/\\d{4}",geb),
  bool_5 = grepl("\\d{1}-\\d{2}-\\d{4}",geb),
  bool_6 = grepl("\\d{1}\\.\\d{2}\\.\\d{4}nein",geb),
  bool_7 = grepl("^\\d{2}\\.\\d{2}\\.\\d{2}$",geb),
  bool_8 = grepl("^\\d{1,}$",geb),
  bool_9 = geb == "",
  bool_10 = grepl("^\\d{2}-\\d{2}-\\d{2}$",geb))

geb_ = seq(Sys.Date(), Sys.Date()+length(geb)-1,1)
geb_[bool[[1]]] = as.Date(geb[bool[[1]]], format = "%d.%m.%Y")
geb_[bool[[2]]] = as.Date(geb[bool[[2]]], format = "%d.,%m.%Y")
geb_[bool[[3]]] = NA
geb_[bool[[4]]] = as.Date(geb[bool[[4]]], format = "%d/%m/%Y")
geb_[bool[[5]]] = as.Date(geb[bool[[5]]], format = "%d-%m-%Y")
geb_[bool[[6]]] = as.Date(geb[bool[[6]]], format = "%d.%m.%Ynein")
geb_[bool[[7]]] = as.Date(geb[bool[[7]]], format = "%d.%m.%Y")
geb_[bool[[8]]] = as.Date((as.numeric(as.character(geb[bool[[8]]]))-1), origin = as.Date("31.12.1903", format = "%d.%m.%Y"))
geb_[bool[[9]]] = NA
geb_[bool[[10]]] = as.Date(geb[bool[[10]]], format = "%y-%m-%d")
geb_
#e)
start_date = as.POSIXlt(tab[["Verleihung"]])
result = start_date$year - as.POSIXlt(geb_)$year


