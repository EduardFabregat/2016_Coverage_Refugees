###Separate newspapers by country
Corpus <- meta_theta_df_ALL

asia <- Corpus[Corpus$region == "Asia", ]
africa <- Corpus[Corpus$region == "Africa", ]
eu <- Corpus[Corpus$region == "Europe", ]
na <- Corpus[Corpus$region == "North America", ]
me <- Corpus[Corpus$region == "Middle East", ]
oc <- Corpus[Corpus$region == "Oceania", ]

pp <- count(eu, publication)

uk <- eu[eu$publication == "Daily Express; London (UK)", ]

uk$newspaper <- "Daily Express"
uk$publication <- NULL
uk$country <- "UK"

uk2 <- eu[eu$publication == "Express (Onlin e); London (UK)" | eu$publication == "Express (Online) London (UK)"|
           eu$publication == "Express (Online); London (U K)" | eu$publication == "Express (Online); London (UK )" |
            eu$publication == "Express (Online); London (UK)", ]

uk2$newspaper <- "Express(Online)"
uk2$publication <- NULL
uk2$country <- "UK"

uk3 <- eu[eu$publication == "Daily M ail; London (UK)" | eu$publication == "Daily Mail; London (UK)", ]

uk3$newspaper <- "Daily Mail"
uk3$publication <- NULL
uk3$country <- "UK"

uk4 <- eu[eu$publication == "Financial Times; London (UK)", ]

uk4$newspaper <- "Financial Times"
uk4$publication <- NULL
uk4$country <- "UK"

uk5 <- eu[eu$publication == "Sunday Times; London (UK)", ]

uk5$newspaper <- "Sunday Times"
uk5$publication <- NULL
uk5$country <- "UK"

uk6 <- eu[eu$publication == "The Daily Mirror; London (UK)", ]

uk6$newspaper <- "The Daily Mirror"
uk6$publication <- NULL
uk6$country <- "UK"

uk7 <- eu[eu$publication == "The Daily Telegraph; London (UK)", ]

uk7$newspaper <- "The Daily Telegraph"
uk7$publication <- NULL
uk7$country <- "UK"

uk8 <- eu[eu$publication == "The Guardian; Lon don (UK)" | eu$publication == "The Guardian; London (UK)", ]

uk8$newspaper <- "The Guardian"
uk8$publication <- NULL
uk8$country <- "UK"

uk9 <- eu[eu$publication == "The Sun; London (UK)", ]

uk9$newspaper <- "The Sun"
uk9$publication <- NULL
uk9$country <- "UK"

uk10 <- eu[eu$publication == "The Times; Londo n (UK)" | eu$publication == "The Times; London (UK)", ]

uk10$newspaper <- "The Times"
uk10$publication <- NULL
uk10$country <- "UK"

UK <- rbind(uk, uk2, uk3, uk4, uk5, uk6, uk7, uk8, uk9, uk10)

rm(uk, uk2, uk3, uk4, uk5, uk6, uk7, uk8, uk9, uk10)

openxlsx::write.xlsx(UK, file = "UK.xlsx")

IRE <- eu[eu$publication == "Irish T imes; Dublin" | eu$publication == "Irish Times; Dubli n" |
            eu$publication == "Irish Times; Dublin", ]

IRE$newspaper <- "Irish Times"
IRE$publication <- NULL
IRE$country <- "Ireland"

openxlsx::write.xlsx(IRE, file = "Ireland.xlsx")

pp <- count(africa, publication)

SAF <- africa[africa$publication == "The Pr etoria News; Pretoria" | africa$publication == "The Pretoria News; Pretoria", ]

SAF$newspaper <- "The Pretoria Times"
SAF$publication <- NULL
SAF$country <- "South Africa"

SAF1 <- oc[oc$publication == "The Mercury; Durban", ]

SAF1$newspaper <- "The Mercury"
SAF1$publication <- NULL
SAF1$country <- "South Africa"

SAF2 <- rbind(SAF, SAF1)

openxlsx::write.xlsx(SAF2, file = "South Africa.xlsx")

KEN <- africa[africa$publication == "Daily Nation; Nairobi", ]

KEN$newspaper <- "Daily Nation"
KEN$publication <- NULL
KEN$country <- "Kenya"

#openxlsx::write.xlsx(KEN, file = "Kenya.xlsx")

EGY <- africa[africa$publication == "Daily News Egypt; Cairo", ]

EGY$newspaper <- "Daily News Egypt"
EGY$publication <- NULL
EGY$country <- "Egypt"

openxlsx::write.xlsx(EGY, file = "Egypt.xlsx")

MOR <- africa[africa$publication == "Morocco World News; Rabat", ]

MOR$newspaper <- "Morocco World News"
MOR$publication <- NULL
MOR$country <- "Morocco"

openxlsx::write.xlsx(MOR, file = "Morocco.xlsx")

SUD <- africa[africa$publication == "Sudan Tribune; Paris", ]

SUD$newspaper <- "Sudan Tribune"
SUD$publication <- NULL
SUD$country <- "Sudan"

openxlsx::write.xlsx(SUD, file = "Sudan.xlsx")

KEN2 <- africa[africa$publication == "The East African; Nairobi", ]

KEN2$newspaper <- "The East African"
KEN2$publication <- NULL
KEN2$country <- "Kenya"

KEN1 <- rbind(KEN, KEN2)

openxlsx::write.xlsx(KEN1, file = "Kenya.xlsx")

RW <- africa[africa$publication == "The New Times; Kigali", ]

RW$newspaper <- "The News Times"
RW$publication <- NULL
RW$country <- "Rwanda"

openxlsx::write.xlsx(RW, file = "Rwanda.xlsx")

UG <- africa[africa$publication == "The New Vision; Kampala", ]

UG$newspaper <- "The New Vision"
UG$publication <- NULL
UG$country <- "Uganda"

openxlsx::write.xlsx(UG, file = "Uganda.xlsx")

NIG <- eu[eu$publication == "The Sun; Lagos", ]

NIG$newspaper <- "The Sun"
NIG$publication <- NULL
NIG$country <- "Nigeria"

openxlsx::write.xlsx(NIG, file = "Nigeria.xlsx")

pp <- count(asia, publication)

SIN <- asia[asia$publication == "The Straits Times; Singapore", ]

SIN$newspaper <- "The Straits Times"
SIN$publication <- NULL
SIN$country <- "Singapore"

openxlsx::write.xlsx(SIN, file = "Singapore.xlsx")

pp <- count(asia, publication)

THAI <- asia[asia$publication == "Asia News Monitor; Bangko k" | asia$publication == "Asia News Monitor; Bangkok", ]

THAI$newspaper <- "Asia News Monitor"
THAI$publication <- NULL
THAI$country <- "Thailand"

THAI2 <- asia[asia$publication == "The Nation; Bangkok", ]

THAI2$newspaper <- "The Nation"
THAI2$publication <- NULL
THAI2$country <- "Thailand"

THAI1 <- rbind(THAI, THAI2)

openxlsx::write.xlsx(THAI1, file = "Thailand.xlsx")

TURK <- asia[asia$publication == "Journal of Turkish Weekly; Ankara", ]

TURK$newspaper <- "Journal of Turkish Weekly"
TURK$publication <- NULL
TURK$country <- "Turkey"

openxlsx::write.xlsx(TURK, file = "Turkey.xlsx")

PAK1 <- asia[asia$publication == "Right Vision News; Lahore", ]

PAK1$newspaper <- "Right Vision News"
PAK1$publication <- NULL
PAK1$country <- "Pakistan"

PAK2 <- asia[asia$publication == "The Balochistan Times; Quetta", ]

PAK2$newspaper <- "The Balochistan Times"
PAK2$publication <- NULL
PAK2$country <- "Pakistan"

PAK3 <- asia[asia$publication == "The Nation; Lahore, Pakistan", ]

PAK3$newspaper <- "The Nation"
PAK3$publication <- NULL
PAK3$country <- "Pakistan"

PAK <- rbind(PAK1, PAK2, PAK3)

openxlsx::write.xlsx(PAK, file = "Pakistan.xlsx")

CHI <- asia[asia$publication == "Shanghai Daily; Shanghai, China", ]

CHI$newspaper <- "Shanghai Daily"
CHI$publication <- NULL
CHI$country <- "China"

openxlsx::write.xlsx(CHI, file = "China.xlsx")

HK <- asia[asia$publication == "South China Morning Post; Hong Kong", ]

HK$newspaper <- "Hong Kong"
HK$publication <- NULL
HK$country <- "Hong Kong"

openxlsx::write.xlsx(HK, file = "Hong Kong.xlsx")

IND1 <- asia[asia$publication == "The Balochistan Times; Quetta", ]

IND1$newspaper <- "The Balochistan Times"
IND1$publication <- NULL
IND1$country <- "India"

IND2 <- asia[asia$publication == "The Hindu; Chennai", ]

IND2$newspaper <- "The Hindu"
IND2$publication <- NULL
IND2$country <- "India"

IND3 <- asia[asia$publication == "The Hindustan Times; New Delhi", ]

IND3$newspaper <- "The Hindustan Times"
IND3$publication <- NULL
IND3$country <- "India"

IND4 <- asia[asia$publication == "The Times of India; New Delhi", ]

IND4$newspaper <- "The Times of India"
IND4$publication <- NULL
IND4$country <- "India"

IND <- rbind(IND1, IND2, IND3, IND4)

openxlsx::write.xlsx(IND, file = "India.xlsx")

SK <- asia[asia$publication == "The Korea Times; Seoul", ]

SK$newspaper <- "The Korea Times"
SK$publication <- NULL
SK$country <- "South Korea"

openxlsx::write.xlsx(SK, file = "South Korea.xlsx")

pp <- count(me, publication)

SAR <- me[me$publication == "Asharq Alawsat, English ed.; London", ]

SAR$newspaper <- "Asharq Alawsat"
SAR$publication <- NULL
SAR$country <- "Saudi Arabia"

openxlsx::write.xlsx(SAR, file = "Saudi Arabia.xlsx")

CYP <- me[me$publication == "Cyprus Mail; Nicosia", ]

CYP$newspaper <- "Cyrpus Mail"
CYP$publication <- NULL
CYP$country <- "Cyprus"

openxlsx::write.xlsx(CYP, file = "Cyrpus.xlsx")

UAE <- me[me$publication == "Gulf News; Dubai", ]

UAE$newspaper <- "Gulf News"
UAE$publication <- NULL
UAE$country <- "United Arab Emirates"

openxlsx::write.xlsx(UAE, file = "United Arab Emirates.xlsx")

IRA <- me[me$publication == "Iran Daily; Tehran", ]

IRA$newspaper <- "Iran Daily"
IRA$publication <- NULL
IRA$country <- "Iran"

openxlsx::write.xlsx(IRA, file = "Iran.xlsx")

ISR1 <- me[me$publication == "Jerusalem Post; Jerusalem", ]

ISR1$newspaper <- "Jerusalem Post"
ISR1$publication <- NULL
ISR1$country <- "Israel"

ISR2 <- me[me$publication == "The Times of Israel; Jerusalem, Israel", ]

ISR2$newspaper <- "The Times of Israel"
ISR2$publication <- NULL
ISR2$country <- "Israel"

ISR <- rbind(ISR1, ISR2)

openxlsx::write.xlsx(ISR, file = "Israel.xlsx")

JOR <- me[me$publication == "Jordan Times; Amman", ]

JOR$newspaper <- "Jordan Times"
JOR$publication <- NULL
JOR$country <- "Jordan"

openxlsx::write.xlsx(JOR, file = "Jordan.xlsx")

LEB1 <- me[me$publication == "The Dail y Star; Beirut" | me$publication == "The Daily Star; Bei rut" |
            me$publication == "The Daily Star; Beir ut" | me$publication == "The Daily Star; Beiru t" |
            me$publication == "The Daily Star; Beirut", ]

LEB1$newspaper <- "The Daily Star"
LEB1$publication <- NULL
LEB1$country <- "Lebanon"

LEB2 <- me[me$publication == "Yerepouni Daily News; Beirut", ]

LEB2$newspaper <- "The Daily StaYerepouni Daily News"
LEB2$publication <- NULL
LEB2$country <- "Lebanon"

LEB <- rbind(LEB1, LEB2)

openxlsx::write.xlsx(LEB, file = "Lebanon.xlsx")

QAT <- me[me$publication == "The Peninsula; Doha", ]

QAT$newspaper <- "The Peninsula"
QAT$publication <- NULL
QAT$country <- "Qatar"

openxlsx::write.xlsx(QAT, file = "Qatar.xlsx")

pp <- count(na, publication)

US1 <- na[na$publication == "Chicago Tribune; Chicag o, Ill." | na$publication == "Chicago Tribune; Chicago, I ll." |
            na$publication == "Chicago Tribune; Chicago, Ill.", ] 

US1$newspaper <- "Chicago Tribune"
US1$publication <- NULL
US1$country <- "US"

US2 <- na[na$publication == "Los Angel es Times; Los Angeles, Calif." | na$publication == "Los Angeles Time s; Los Angeles, Calif." |
            na$publication == "Los Angeles Times Los Angeles, Calif." | na$publication == "Los Angeles Times; Los Angeles, Calif.", ] 

US2$newspaper <- "LA Times"
US2$publication <- NULL
US2$country <- "US"

US3 <- na[na$publication == "New Yor k Times, Late Edition (East Coast); New York, N.Y." | 
            na$publication == "New York Ti mes, Late Edition (East Coast); New York, N.Y." |
            na$publication == "New York Times, La te Edition (East Coast); New York, N.Y." | 
            na$publication == "New York Times, Lat e Edition (East Coast); New York, N.Y." |
            na$publication == "New York Times, Late E dition (East Coast); New York, N.Y." |
            na$publication == "New York Times, Late Editio n (East Coast); New York, N.Y." |
            na$publication == "New York Times, Late Edition (Ea st Coast); New York, N.Y." |
            na$publication == "New York Times, Late Edition (Eas t Coast); New York, N.Y.." |
            na$publication == "New York Times, Late Edition (East Coa st); New York, N.Y." |
            na$publication == "New York Times, Late Edition (East Coas t); New York, N.Y." |
            na$publication == "New York Times, Late Edition (East Coast); Ne w York, N.Y." |
            na$publication == "New York Times, Late Edition (East Coast); New Y ork, N.Y." |
            na$publication == "New York Times, Late Edition (East Coast); New Yo rk, N.Y." |
            na$publication == "New York Times, Late Edition (East Coast); New Yor k, N.Y." |
            na$publication == "New York Times, Late Edition (East Coast); New York, N.Y ." |
            na$publication == "New York Times, Late Edition (East Coast); New York, N.Y.", ]

US3$newspaper <- "New York Times"
US3$publication <- NULL
US3$country <- "US"

US4 <- na[na$publication == "Th e Washington Post; Washington, D.C." | 
            na$publication == "The Wa shington Post; Washington, D.C." |
            na$publication == "The Was hington Post; Washington, D.C." | 
            na$publication == "The Washington Post; Wa shington, D.C." |
            na$publication == "The Washington Post; Washi ngton, D.C." |
            na$publication == "The Washington Post; Washington, D .C." |
            na$publication == "The Washington Post; Washington, D.C.", ] 

US4$newspaper <- "The Washington Post"
US4$publication <- NULL
US4$country <- "US"

US5 <- na[na$publication == "Wall S treet Journal (Online); New York, N.Y." | 
            na$publication == "Wall Stre et Journal (Online); New York, N.Y." |
            na$publication == "Wall Stree t Journal (Online); New York, N.Y." | 
            na$publication == "Wall Street J ournal (Online); New York, N.Y." |
            na$publication == "Wall Street Journ al (Online); New York, N.Y." |
            na$publication == "Wall Street Journal (O nline); New York, N.Y." |
            na$publication == "Wall Street Journal (Online) New York, N.Y." |
            na$publication == "Wall Street Journal (Online); Ne w York, N.Y." |
            na$publication == "Wall Street Journal (Online); New Yo rk, N.Y." |
            na$publication == "Wall Street Journal (Online); New Yor k, N.Y." |
            na$publication == "Wall Street Journal (Online); New York, N .Y." |
            na$publication == "Wall Street Journal (Online); New York, N.Y ." |
            na$publication == "Wall Street Journal (Online); New York, N.Y.", ] 

US5$newspaper <- "Wall Street Journal"
US5$publication <- NULL
US5$country <- "US"

US6 <- eu[eu$publication == "The Sun; Lowell, Mass." | eu$publication == "The Sun; San Bernardino, Calif.", ]

US6$newspaper <- "The Sun"
US6$publication <- NULL
US6$country <- "US"

US7 <- eu[eu$publication == "The Times; Shreveport, La." | eu$publication == "The Times; Victor Harbor, S. Aust.", ]

US7$newspaper <- "The Times"
US7$publication <- NULL
US7$country <- "US"


US8 <- na[na$publication == "USA TODAY; McLean, Va.", ]

US8$newspaper <- "USA Today"
US8$publication <- NULL
US8$country <- "US"

US <- rbind(US1, US2, US3, US4, US5, US6, US7, US8)

openxlsx::write.xlsx(US, file = "US.xlsx")

CAN1 <- na[na$publication == "Chronicle - Herald; Halifa x, N.S." | 
             na$publication == "Chronicle - Herald; Halifax, N. S." |
             na$publication == "Chronicle - Herald; Halifax, N.S.", ]

CAN1$newspaper <- "Chronicle-Herald"
CAN1$publication <- NULL
CAN1$country <- "Canada" 

CAN2 <- na[na$publication == "National Pos t; Don Mills, Ont." | 
             na$publication == "National Post; Don Mills, On t." |
             na$publication == "National Post; Don Mills, Ont.", ]

CAN2$newspaper <- "National Post"
CAN2$publication <- NULL
CAN2$country <- "Canada" 

CAN3 <- na[na$publication == "Telegraph-Journal; Saint Jo hn, N.B." | 
             na$publication == "Telegraph-Journal; Saint John, N.B.", ]

CAN3$newspaper <- "Telegraph-Journal"
CAN3$publication <- NULL
CAN3$country <- "Canada" 

CAN4 <- na[na$publication == "The Globe and Mail; Toronto, Ont.", ]

CAN4$newspaper <- "The Globe and Mail"
CAN4$publication <- NULL
CAN4$country <- "Canada"

CAN5 <- na[na$publication == "The Ottawa Citizen; Ottawa, Ont.", ]

CAN5$newspaper <- "The Ottawa Citizen"
CAN5$publication <- NULL
CAN5$country <- "Canada"

CAN6 <- na[na$publication == "Toronto Star; Toronto, Ont.", ]

CAN6$newspaper <- "Toronto Star"
CAN6$publication <- NULL
CAN6$country <- "Canada"

CAN <- rbind(CAN1, CAN2, CAN3, CAN4, CAN5, CAN6)

openxlsx::write.xlsx(CAN, file = "Canada.xlsx")

pp <- count(oc, publication)

AUS1 <- oc[oc$publication == "Sydney Morning Herald; Sydney, N.S.W.", ]

AUS1$newspaper <- "Syndey Morning Herald"
AUS1$publication <- NULL
AUS1$country <- "Australia"

AUS2 <- oc[oc$publication == "The Age; Melbour ne, Vic." | oc$publication == "The Age; Melbourne, Vic.", ]

AUS2$newspaper <- "The Age"
AUS2$publication <- NULL
AUS2$country <- "Australia"

AUS3 <- oc[oc$publication == "The Australian; Canberra, A.C.T.", ]

AUS3$newspaper <- "The Australian"
AUS3$publication <- NULL
AUS3$country <- "Australia"

AUS4 <- oc[oc$publication == "The Canberra Times; Canberra, A.C.T ." |
             oc$publication == "The Canberra Times; Canberra, A.C.T.", ]

AUS4$newspaper <- "The Canberra Times"
AUS4$publication <- NULL
AUS4$country <- "Australia"

AUS5 <- oc[oc$publication == "The Courier - Mail; Brisbane, Qld.", ]

AUS5$newspaper <- "The Courier-Mail"
AUS5$publication <- NULL
AUS5$country <- "Australia"

AUS6 <- oc[oc$publication == "The Mercury; Hobart Town, Tas.", ]

AUS6$newspaper <- "The Mercury"
AUS6$publication <- NULL
AUS6$country <- "Australia"

AUS7 <- eu[eu$publication == "The Daily Telegraph; Surry Hills, N.S.W.", ]

AUS7$newspaper <- "The Daily Telegraph"
AUS7$publication <- NULL
AUS7$country <- "Australia"

AUS <- rbind(AUS1, AUS2, AUS3, AUS4, AUS5, AUS6, AUS7)

openxlsx::write.xlsx(AUS, file = "Australia.xlsx")

NZ <- oc[oc$publication == "The New Zealand Herald; Auckland, New Zealand", ]

NZ$newspaper <- "The New Zealand Herald"
NZ$publication <- NULL
NZ$country <- "New Zealand"

openxlsx::write.xlsx(NZ, file = "New Zealand.xlsx")

all <- rbind(AUS, CAN, CHI, CYP, EGY, HK, IND, IRA, IRE, ISR, JOR, KEN1, LEB, MOR, NIG, NZ, PAK, QAT, RW, SAF2,
             SAR, SIN, SK, SUD, THAI1, TURK, UAE, UG, UK, US)

openxlsx::write.xlsx(all, "all.articles.country.newspaper.xlsx")


####Find frame usage by country
for (mycountry in unique(all_articles_country_newspaper$country)) {
  print(mycountry)
  temp_country <- all_articles_country_newspaper[all_articles_country_newspaper$country == mycountry, ]
  temp_date <- aggregate(x=temp_country[,7:56],
                         by=list(temp_country$date),FUN="mean")
  frame1 <- temp_date[, c("Group.1","V2", "V6", "V10", "V25", "V32", "V34", "V42", "V44", "V45", 
                          "V46", "V47", "V50")]
  frame1$solutions <- rowSums(frame1[2:ncol(frame1)])
  frame1_small <- frame1[, c(1, 14)]
  frame2 <- temp_date[, c("Group.1", "V3", "V4", "V5", "V9", "V11", "V15", "V18", 
                          "V19", "V20", "V26", "V27", "V29", "V30", "V33", "V35",
                          "V38", "V41")]
  frame2$conflict <- rowSums(frame2[2:ncol(frame2)])
  frame2_small <- frame2[, c(1, 19)]
  frame3 <- temp_date[, c("Group.1", "V1","V8","V12","V13","V17","V22","V23","V24","V28","V31",
                          "V36","V37","V39","V43","V48","V49")]
  frame3$politics <- rowSums(frame3[2:ncol(frame3)])
  frame3_small <- frame3[, c(1, 18)]
  frames <- cbind(frame1_small, frame2_small, frame3_small)
  frames <- frames[, c(1,2,4,6)]
  frames$country <- mycountry
  country <- frames
  openxlsx::write.xlsx(country, file = paste0(mycountry, "_frames.xlsx"))
}


####Plot frame usage by country
library(tidyverse)

aus <- ggplot(Australia_frames, aes(x = Group.1))+
  geom_smooth(aes(y = solutions),se=F,color="darkgreen")+
  geom_smooth(aes(y = conflict), se=F, color = "blue")+
  geom_smooth(aes(y = politics), se = F, color = "darkorange")+
  ylim(0, 0.6) +
  theme_bw() +
  xlab("") +
  ylab("Frame Frequency") +
  labs(title = "Australia")

can <- ggplot(Canada_frames, aes(x = Group.1))+
  geom_smooth(aes(y = solutions),se=F,color="darkgreen")+
  geom_smooth(aes(y = conflict), se=F, color = "blue")+
  geom_smooth(aes(y = politics), se = F, color = "darkorange")+
  ylim(0, 0.6) +
  theme_bw() +
  xlab("") +
  ylab("") +
  labs(title = "Canada")

chi <- ggplot(China_frames, aes(x = Group.1))+
  geom_smooth(aes(y = solutions),se=F,color="darkgreen")+
  geom_smooth(aes(y = conflict), se=F, color = "blue")+
  geom_smooth(aes(y = politics), se = F, color = "darkorange")+
  ylim(0, 0.6) +
  theme_bw() +
  xlab("") +
  ylab("") +
  labs(title = "China")

cyp <- ggplot(Cyprus_frames, aes(x = Group.1))+
  geom_smooth(aes(y = solutions),se=F,color="darkgreen")+
  geom_smooth(aes(y = conflict), se=F, color = "blue")+
  geom_smooth(aes(y = politics), se = F, color = "darkorange")+
  ylim(0, 0.6) +
  theme_bw() +
  xlab("") +
  ylab("") +
  labs(title = "Cyprus")

egy <- ggplot(Egypt_frames, aes(x = Group.1))+
  geom_smooth(aes(y = solutions),se=F,color="darkgreen")+
  geom_smooth(aes(y = conflict), se=F, color = "blue")+
  geom_smooth(aes(y = politics), se = F, color = "darkorange")+
  ylim(0, 0.6) +
  theme_bw() +
  xlab("") +
  ylab("") +
  labs(title = "Egypt")

hk <- ggplot(Hong_Kong_frames, aes(x = Group.1))+
  geom_smooth(aes(y = solutions),se=F,color="darkgreen")+
  geom_smooth(aes(y = conflict), se=F, color = "blue")+
  geom_smooth(aes(y = politics), se = F, color = "darkorange")+
  ylim(0, 0.6) +
  theme_bw() +
  xlab("") +
  ylab("Frame Frequency") +
  labs(title = "Hong Kong")

ind <- ggplot(India_frames, aes(x = Group.1))+
  geom_smooth(aes(y = solutions),se=F,color="darkgreen")+
  geom_smooth(aes(y = conflict), se=F, color = "blue")+
  geom_smooth(aes(y = politics), se = F, color = "darkorange")+
  ylim(0, 0.6) +
  theme_bw() +
  xlab("") +
  ylab("") +
  labs(title = "India")

egy <- ggplot(Egypt_frames, aes(x = Group.1))+
  geom_smooth(aes(y = solutions),se=F,color="darkgreen")+
  geom_smooth(aes(y = conflict), se=F, color = "blue")+
  geom_smooth(aes(y = politics), se = F, color = "darkorange")+
  ylim(0, 0.6) +
  theme_bw() +
  xlab("") +
  ylab("") +
  labs(title = "Egypt")

ira <- ggplot(Iran_frames, aes(x = Group.1))+
  geom_smooth(aes(y = solutions),se=F,color="darkgreen")+
  geom_smooth(aes(y = conflict), se=F, color = "blue")+
  geom_smooth(aes(y = politics), se = F, color = "darkorange")+
  ylim(0, 0.6) +
  theme_bw() +
  xlab("") +
  ylab("") +
  labs(title = "Iran")

ire <- ggplot(Ireland_frames, aes(x = Group.1))+
  geom_smooth(aes(y = solutions),se=F,color="darkgreen")+
  geom_smooth(aes(y = conflict), se=F, color = "blue")+
  geom_smooth(aes(y = politics), se = F, color = "darkorange")+
  ylim(0, 0.6) +
  theme_bw() +
  xlab("") +
  ylab("") +
  labs(title = "Ireland")

isr <- ggplot(Israel_frames, aes(x = Group.1))+
  geom_smooth(aes(y = solutions),se=F,color="darkgreen")+
  geom_smooth(aes(y = conflict), se=F, color = "blue")+
  geom_smooth(aes(y = politics), se = F, color = "darkorange")+
  ylim(0, 0.6) +
  theme_bw() +
  xlab("") +
  ylab("") +
  labs(title = "Israel")

jor <- ggplot(Jordan_frames, aes(x = Group.1))+
  geom_smooth(aes(y = solutions),se=F,color="darkgreen")+
  geom_smooth(aes(y = conflict), se=F, color = "blue")+
  geom_smooth(aes(y = politics), se = F, color = "darkorange")+
  ylim(0, 0.6) +
  theme_bw() +
  xlab("") +
  ylab("Frame Frequency") +
  labs(title = "Jordan")

ken <- ggplot(Kenya_frames, aes(x = Group.1))+
  geom_smooth(aes(y = solutions),se=F,color="darkgreen")+
  geom_smooth(aes(y = conflict), se=F, color = "blue")+
  geom_smooth(aes(y = politics), se = F, color = "darkorange")+
  ylim(0, 0.6) +
  theme_bw() +
  xlab("") +
  ylab("") +
  labs(title = "Kenya")

leb <- ggplot(Lebanon_frames, aes(x = Group.1))+
  geom_smooth(aes(y = solutions),se=F,color="darkgreen")+
  geom_smooth(aes(y = conflict), se=F, color = "blue")+
  geom_smooth(aes(y = politics), se = F, color = "darkorange")+
  ylim(0, 0.6) +
  theme_bw() +
  xlab("") +
  ylab("") +
  labs(title = "Lebanon")

mor <- ggplot(Morocco_frames, aes(x = Group.1))+
  geom_smooth(aes(y = solutions),se=F,color="darkgreen")+
  geom_smooth(aes(y = conflict), se=F, color = "blue")+
  geom_smooth(aes(y = politics), se = F, color = "darkorange")+
  ylim(0, 0.6) +
  theme_bw() +
  xlab("") +
  ylab("") +
  labs(title = "Morocco")

nz <- ggplot(New_Zealand_frames, aes(x = Group.1))+
  geom_smooth(aes(y = solutions),se=F,color="darkgreen")+
  geom_smooth(aes(y = conflict), se=F, color = "blue")+
  geom_smooth(aes(y = politics), se = F, color = "darkorange")+
  ylim(0, 0.6) +
  theme_bw() +
  xlab("") +
  ylab("") +
  labs(title = "New Zealand")

nig <- ggplot(Nigeria_frames, aes(x = Group.1))+
  geom_smooth(aes(y = solutions),se=F,color="darkgreen")+
  geom_smooth(aes(y = conflict), se=F, color = "blue")+
  geom_smooth(aes(y = politics), se = F, color = "darkorange")+
  ylim(0, 0.6) +
  theme_bw() +
  xlab("") +
  ylab("Frame Frequency") +
  labs(title = "Nigeria")

pak <- ggplot(Pakistan_frames, aes(x = Group.1))+
  geom_smooth(aes(y = solutions),se=F,color="darkgreen")+
  geom_smooth(aes(y = conflict), se=F, color = "blue")+
  geom_smooth(aes(y = politics), se = F, color = "darkorange")+
  ylim(0, 0.6) +
  theme_bw() +
  xlab("") +
  ylab("") +
  labs(title = "Pakistan")

qat <- ggplot(Qatar_frames, aes(x = Group.1))+
  geom_smooth(aes(y = solutions),se=F,color="darkgreen")+
  geom_smooth(aes(y = conflict), se=F, color = "blue")+
  geom_smooth(aes(y = politics), se = F, color = "darkorange")+
  ylim(0, 0.6) +
  theme_bw() +
  xlab("") +
  ylab("") +
  labs(title = "Qatar")

rwa <- ggplot(Rwanda_frames, aes(x = Group.1))+
  geom_smooth(aes(y = solutions),se=F,color="darkgreen")+
  geom_smooth(aes(y = conflict), se=F, color = "blue")+
  geom_smooth(aes(y = politics), se = F, color = "darkorange")+
  ylim(0, 0.6) +
  theme_bw() +
  xlab("") +
  ylab("") +
  labs(title = "Rwanda")

sau <- ggplot(Saudi_Arabia_frames, aes(x = Group.1))+
  geom_smooth(aes(y = solutions),se=F,color="darkgreen")+
  geom_smooth(aes(y = conflict), se=F, color = "blue")+
  geom_smooth(aes(y = politics), se = F, color = "darkorange")+
  ylim(0, 0.6) +
  theme_bw() +
  xlab("") +
  ylab("Frame Frequency") +
  labs(title = "Saudi Arabia")

sin <- ggplot(Singapore_frames, aes(x = Group.1))+
  geom_smooth(aes(y = solutions),se=F,color="darkgreen")+
  geom_smooth(aes(y = conflict), se=F, color = "blue")+
  geom_smooth(aes(y = politics), se = F, color = "darkorange")+
  ylim(0, 0.6) +
  theme_bw() +
  xlab("") +
  ylab("") +
  labs(title = "Singapore")

saf <- ggplot(South_Africa_frames, aes(x = Group.1))+
  geom_smooth(aes(y = solutions),se=F,color="darkgreen")+
  geom_smooth(aes(y = conflict), se=F, color = "blue")+
  geom_smooth(aes(y = politics), se = F, color = "darkorange")+
  ylim(0, 0.6) +
  theme_bw() +
  xlab("") +
  ylab("") +
  labs(title = "South Africa")

sok <- ggplot(South_Korea_frames, aes(x = Group.1))+
  geom_smooth(aes(y = solutions),se=F,color="darkgreen")+
  geom_smooth(aes(y = conflict), se=F, color = "blue")+
  geom_smooth(aes(y = politics), se = F, color = "darkorange")+
  ylim(0, 0.6) +
  theme_bw() +
  xlab("") +
  ylab("") +
  labs(title = "South Korea")

sud <- ggplot(Sudan_frames, aes(x = Group.1))+
  geom_smooth(aes(y = solutions),se=F,color="darkgreen")+
  geom_smooth(aes(y = conflict), se=F, color = "blue")+
  geom_smooth(aes(y = politics), se = F, color = "darkorange")+
  ylim(0, 0.6) +
  theme_bw() +
  xlab("") +
  ylab("") +
  labs(title = "Sudan")

tha <- ggplot(Thailand_frames, aes(x = Group.1))+
  geom_smooth(aes(y = solutions),se=F,color="darkgreen")+
  geom_smooth(aes(y = conflict), se=F, color = "blue")+
  geom_smooth(aes(y = politics), se = F, color = "darkorange")+
  ylim(0, 0.6) +
  theme_bw() +
  xlab("") +
  ylab("") +
  labs(title = "Thailand")

tur <- ggplot(Turkey_frames, aes(x = Group.1))+
  geom_smooth(aes(y = solutions),se=F,color="darkgreen")+
  geom_smooth(aes(y = conflict), se=F, color = "blue")+
  geom_smooth(aes(y = politics), se = F, color = "darkorange")+
  ylim(0, 0.6) +
  theme_bw() +
  xlab("Date") +
  ylab("Frame Frequency") +
  labs(title = "Turkey")

uga <- ggplot(Uganda_frames, aes(x = Group.1))+
  geom_smooth(aes(y = solutions),se=F,color="darkgreen")+
  geom_smooth(aes(y = conflict), se=F, color = "blue")+
  geom_smooth(aes(y = politics), se = F, color = "darkorange")+
  ylim(0, 0.6) +
  theme_bw() +
  xlab("Date") +
  ylab("Frame Frequency") +
  labs(title = "Uganda")

uk <- ggplot(UK_frames, aes(x = Group.1))+
  geom_smooth(aes(y = solutions),se=F,color="darkgreen")+
  geom_smooth(aes(y = conflict), se=F, color = "blue")+
  geom_smooth(aes(y = politics), se = F, color = "darkorange")+
  ylim(0, 0.6) +
  theme_bw() +
  xlab("Date") +
  ylab("") +
  labs(title = "United Kingdom")

uae <- ggplot(United_Arab_Emirates_frames, aes(x = Group.1))+
  geom_smooth(aes(y = solutions),se=F,color="darkgreen")+
  geom_smooth(aes(y = conflict), se=F, color = "blue")+
  geom_smooth(aes(y = politics), se = F, color = "darkorange")+
  ylim(0, 0.6) +
  theme_bw() +
  xlab("Date") +
  ylab("") +
  labs(title = "United Arab Emirates")

us <- ggplot(US_frames, aes(x = Group.1))+
  geom_smooth(aes(y = solutions),se=F,color="darkgreen")+
  geom_smooth(aes(y = conflict), se=F, color = "blue")+
  geom_smooth(aes(y = politics), se = F, color = "darkorange")+
  ylim(0, 0.6) +
  theme_bw() +
  xlab("Date") +
  ylab("") +
  labs(title = "United States")

library(grid)
library(gridExtra)

grid.arrange(aus, can, chi, cyp, egy, hk,ind, ira, ire, isr, jor, ken, leb,
             mor, nz, nig, pak, qat, rwa, saf, sau, sin, sok, sud, tha, tur, 
             uae, uga, uk, us, ncol=5)