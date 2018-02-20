Part 1
#a)
load("nrw17.RDATA")
length(nrw17)
attributes(nrw17)
#b)
nrw17$inhalt[1]
#c)
ww = strsplit(nrw17$inhalt[1:10], " ")
ww = sapply(ww, function(w) w[!grepl("[^a-zA-Z]", w)])
sapply(ww, "[", 2)

#d)
ww2 = nrw17$inhalt[1:100]
ww2 = ww2[!nrw17$isretweet[1:100]]
ww2 = strsplit(ww2, " ")
ww2 = sapply(ww2, function(w) w[!grepl("[^a-zA-Z]", w)])
sapply(ww2, "[", 1)
#e)
retw = nrw17$inhalt[nrw17$isretweet]
retw = strsplit(retw, " ")
retw = sapply(retw, function(w) w[!grepl("[^a-zA-Z]", w)])
retw = sapply(retw, "[", 1)
retw = strsplit(retw, " ")
sum(retw == "RT")/length(retw)
#f)
sum(regexpr("@" ,nrw17$inhalt)>0)/length(nrw17$inhalt) 

#g)
bool_ = regexpr("@", nrw17$inhalt) > 0 
no_at = which(bool_ == FALSE) 
num_at = sapply(gregexpr("@", nrw17$inhalt),length) 
gregexpr("@", nrw17$inhalt)
num_at[no_at] = 0 
nrw17 = c(nrw17,list(Num_at=num_at)) 
table(num_at)

#Part 2
#a)
words_vec = unlist(strsplit(nrw17$inhalt," "))
words_vec
#b)
at_verw = unlist(strsplit(nrw17$inhalt[bool_], split=" ")) 
bool_at = grepl("@", at_verw) 
words_at = at_verw[bool_at] 
#c) 
sorted_usr = sort(table(nrw17$name))
len = length(sorted_usr)
top_10 = rev(sorted_usr[(len-10):len])
top_10
#d)
tw_table = table(nrw17$name) 
unique(sort(tw_table))
len = length(tw_table)
break_points = c(which(diff(rev(sort(tw_table))) < 0), X = len)
break_diff = c(break_points[1], diff(break_points))
names(break_diff) = rev(unique(sort(tw_table)))
rev(break_diff)

#e) 
top_100 = rev(sort(table(nrw17$name)))[1:100]
rand_usr = sample(top_100 ,1, replace=FALSE)
num_ref = sum(grepl(paste0("@",names(rand_usr),separate= ""), nrw17$inhalt[bool_]))
num_ref

#Part 3
#a)
all_sgn =  unlist(strsplit(nrw17$inhalt, split="")) 
table_ = table(all_sgn) 
table_
#b)
hauf_tab = table(all_sgn[grepl("[a-zA-Z]", all_sgn)]) 
hauf_tab
#c)
all_na = ifelse(grepl("[a-zA-Z]", all_sgn), all_sgn[grepl("[a-zA-Z]", all_sgn)] , "NA") 
table_all_na = table(all_na) 
table_all_na
#d)

times = nrw17$zeit 
times_dot = sub(":",".", times)
times_vec =  unlist(times_dot) 
times_num = as.numeric(times_vec) 
hrs = round(times_num)
hrs_sep = paste(hrs, "00" , sep=".")
hrs_table = sort(table(hrs_sep)) 
hrs_table
#e)
chisq.test(hrs_table) # Nein, p-value ist viel kleiner als Signifikanzniveau, wir können die Nullhypothese nicht annehmen.

#Part 4
#a)
matr =  t(sapply(rep(2, 1000), sample, prob=c(0.3,0.7), replace=TRUE, size = 700)) 
matr[matr==2] = 0
#b)
sam = sample(1000) 
ran_row = paste0("Zeile",sam)
rownames(matr) = ran_row
matr
#c) 
ja_anteil = rowMeans(matr) 
ja_anteil
#d)
sort(ja_anteil)
sort(ja_anteil)[25]
sort(ja_anteil)[25] == quantile(ja_anteil, probs=0.025)
sort(ja_anteil)[975] == quantile(ja_anteil, probs=0.975)
#e)
alpha = 0.05  
n = 1000
sch = ja_anteil
q_n = qnorm(1 - (alpha/2))
sd = sqrt(sch*(1-sch))
untere_g = sch - q_n * sd/sqrt(n)
obere_g = sch + q_n * sd/sqrt(n)
conf_int = paste(untere_g,obere_g,sep = ",")
conf_int[975]
quantile(ja_anteil, probs=0.975) 
conf_int[25]
quantile(ja_anteil, probs=0.025)
#f)
row_names = names(ja_anteil)
row_names_num = sub("Zeile", replace= "",  row_names) 
row_nums = (as.numeric(row_names_num))
row_nums_sort = paste0("Zeile", sort(row_nums), sep="") 
ja_anteil_sort = ja_anteil[row_nums_sort] 
ja_anteil_sort

#g)

sam = sample(700000, 2000) 
matr[sam] = NA
ja_anteil_no_na <- rowMeans(matr, na.rm=TRUE)
ja_anteil_no_na



