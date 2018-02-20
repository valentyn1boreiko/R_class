#1)
load("school.RDATA")
load("student.RDATA")
load("teach.RDATA")
length(school)
min(c(class(school),class(student),class(teach)) == "data.frame") == TRUE
#2)
table(sapply(school,class))
table(sapply(student,class))
table(sapply(teach,class))

apply(sapply(school,unclass),2,class)
apply(sapply(student,unclass),2,class)
apply(sapply(teach,unclass),2,class)
#3)
IDSCHOOL = school[grepl("^PCBG", colnames(school))]
colnames(IDSCHOOL) = gsub("^PCBG", "F", colnames(IDSCHOOL))
#4)
sapply(sapply(fragebogen,unique),length)
#5)
mat = apply(as.matrix(school),2,as.numeric)
lapply(lapply(as.data.frame(school[as.vector(apply(!apply(mat,2,is.na),2,max) == 1)]),function(x) as.numeric(as.character(x))),is.numeric)
#6)
x = levels(school$PCBG04)[school$PCBG04]
change = gsub("or less", "oder weniger",gsub("to", "bis", gsub("More than", "Mehr als", x)))
levels(school$PCBG04) = change
levels(school$PCBG04)
#7)
table(school$PCBG07A, useNA = "always")
#8)
breaks = c(0, 150, 200, 250, 300)
labels = c("selten", "durchschnittlich", "oft", "sehr oft")
kategorien = cut(as.numeric(as.character(school$PCBG07A)), breaks = breaks, labels = labels, ordered = TRUE)
school$PCBG07AA = kategorien
#9)
school_ = na.omit(school)
days_per_week_open = school_$PCBG07C
size_of_the_school = school_$PCBG05A
kreuztabelle = table(days_per_week_open, size_of_the_school)
prop.table(kreuztabelle, 2) #Spaltenprozent
prop.table(kreuztabelle, 1) 
#10)
teach_ = na.omit(teach)
physics_min_per_week_prep = teach_$PTBP17
age = teach_$PTBG03   
table(physics_min_per_week_prep, age)
mean_ = tapply(physics_min_per_week_prep, age, FUN = mean)
sd_ = tapply(physics_min_per_week_prep, age, FUN = sd)
n = tapply(physics_min_per_week_prep, age, FUN = length)
se = sd_/sqrt(n)
stat_ = as.data.frame(cbind(mean_, se))
#11)
cat1 <- na.omit(physics_min_per_week_prep[age %in% levels(age)[2]])
cat2 <- na.omit(physics_min_per_week_prep[age %in% levels(age)[3]])
t.test(cat1, cat2)
#12)
aggregate(teach[sapply(teach, is.numeric)], list(teach$PTBG02), sum) 
data_ = aggregate(teach[sapply(teach, is.numeric)], list(teach$IDSCHOOL), sum)
names(data_)[1] = "IDSCHOOL"
data_
#13)
nrow(school) == nrow(data_)
merge(data_, school, by = "IDSCHOOL")
#14)
geschlecht = student$PSBG01 == "Male"
physic_lernen = as.numeric(as.character(student$PSBP15)) 
aggregate(cbind(geschlecht,physic_lernen),list(student$IDSCHOOL), sum, na.rm = TRUE)
aggregate(cbind(geschlecht,physic_lernen),list(student$IDCLASS), sum, na.rm = TRUE)
#15)
merge(school, student, by = "IDSCHOOL")
intersect(ls(school),ls(student))
