######################################################
########## Statistical Analysis

ref <- Variables_Refugiats_2_intent

colnames(ref)

ref<-ref[ref$Country!="Singapore",]


attach(ref)

#Moral Universal
myvars <- c(2,5,6,7,8,9,10,11,12,13,14,15,16)

colnames(ref)[myvars]

apa.cor.table(ref[,myvars], filename="ex.CorTable1.m_u.doc")

m_u <- lm(solutions~Distance_Country_1.km +Political_Instability, ref)
summary(m_u)
vif(m_u)
bptest(m_u)

plot(m_u)

apa.reg.table(m_u, filename = "regtable.m_u")

p1<-ggplot(ref,aes(x=ref$solutions))+geom_point(aes(y=ref$Distance_Country_1.km))+
  xlab("Moral-Universal Frame Salience")+ylab("Distance to country of Refugees")+theme_bw()
p2<-ggplot(ref,aes(x=ref$solutions))+geom_point(aes(y=ref$Political_Instability))+
  xlab("Moral-Universal Frame Salience")+ylab("Political Instability")+theme_bw()


require(gridExtra)
grid.arrange(p1, p2, ncol=2)

#Conflict

myvars <- c(3,5,6,7,8,9,10,11,12,13,14,15,16)

colnames(ref)[myvars]

apa.cor.table(ref[,myvars], filename="ex.CorTable1.Conflict.doc")

attach(ref)

conflict <- lm(conflict~Distance_Country_2.km +Political_Instability,ref)
summary(conflict)
vif(conflict)
bptest(conflict)

plot(conflict)

apa.reg.table(conflict, filename = "regtable.conflict")


######################

p3<-ggplot(ref,aes(x=ref$conflict))+geom_point(aes(y=ref$Distance_Country_2.km))+
  xlab("Conflict Frame Salience")+ylab("Distance to country of Refugees")+theme_bw()
p4<-ggplot(ref,aes(x=ref$conflict))+geom_point(aes(y=ref$Political_Instability))+
  xlab("Conflict Frame Salience")+ylab("Political Instability")+theme_bw()


require(gridExtra)
grid.arrange(p1, p2, p3, p4, ncol=2)

#Politics

myvars <- c(4,5,6,7,8,9,10,11,12,13,14,15,16)

colnames(ref)[myvars]

apa.cor.table(ref[,myvars], filename="ex.CorTable1.Politics.doc")
