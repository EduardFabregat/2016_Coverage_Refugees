######################################################
########## Statistical Analysis

ref <- readxl::read_excel("1 new a analysis after Rej/Variables Refugiats 2 intent.xlsx")

ref$Country
all_frames$Group.1

ref[29,1] <- "United States"
ref[27,1] <- "United Kingdom"
all_frames[15,1] <- "Uganda"
colnames(all_frames)[1] <- "Country"

ref <- ref[which(ref$Country %in% all_frames$Country),]

ref <- ref[c(1, 5:ncol(ref))]

ref <- cbind(all_frames[all_frames$Country != "Singapore", ], ref)

ref <- ref[, -5]

colnames(ref)

ref$conflict <- as.numeric(ref$conflict)

ref$Distance_Country_1.km.<-as.numeric(ref$Distance_Country_1.km.)
ref$Distance_Country_2.km.<-as.numeric(ref$Distance_Country_2.km.)
ref<-ref[ref$Country!="Singapore",]
ref$Government<-as.factor(ref$Government)
ref2<-select(ref,-c(Origin_Refugees,Distance_to_Myanmar,Distance_to_SouthSudan,Distance_to_Syria,Population_Below_Poverty_Line))

attach(ref)


hist(moral)
hist(conflict)
hist(politics)

#Conflit
myvars <- c(2,5:16)

colnames(ref)[myvars]

apa.cor.table(ref[,myvars])
apa.cor.table(ref[,myvars], filename="Conflict.Corr.doc")

#Moral
myvars <- c(3,5:16)

colnames(ref)[myvars]

apa.cor.table(ref[,myvars])
apa.cor.table(ref[,myvars], filename="Moral.Corr.doc")

#Politics
myvars <- c(4,5:16)

colnames(ref)[myvars]

apa.cor.table(ref[,myvars])
apa.cor.table(ref[,myvars], filename="Politics.Corr.doc")

moral_press <- lm(moral~Press_Freedom_Index, ref)
moral_dist <- lm(moral~`Distance_Country_1.km`, ref)

apa.reg.table(moral_press, filename = "moral_press.doc")
apa.reg.table(moral_dist, filename = "moral_dist.doc")

conflict_gdp <- lm(conflict~`GDP(percapita)`, ref)
conflict_peace <- lm(conflict~Peace_Index, ref)

apa.reg.table(conflict_gdp, filename = "conflict_gdp.doc")
apa.reg.table(conflict_peace, filename = "conflict_peace.doc")


pol_dist <- lm(politics~Distance_Country_1.km, ref)
pol_gdp <- lm(politics~`GDP(percapita)`, ref)
pol_press <- lm(politics~Press_Freedom_Index, ref)

apa.reg.table(pol_dist, filename = "pol_dist.doc")
apa.reg.table(pol_gdp, filename = "pol_gdp.doc")
apa.reg.table(pol_press, filename = "pol_press.doc")

