options(stringsAsFactors = FALSE)

library(quanteda)
library(dplyr)
library(topicmodels)
library(doParallel)
library(RNewsflow)
library(lubridate)
library(xlsx)

Corpus$index <- 1:nrow(Corpus)
Corpus<-as.data.frame(Corpus)

table(Corpus$region)

sub1<-Corpus[Corpus$region=="Africa",]
sub2<-Corpus[Corpus$region=="Asia",]
sub3<-Corpus[Corpus$region=="Europe",]
sub4<-Corpus[Corpus$region=="Middle East",]
sub5<-Corpus[Corpus$region=="North America",]
sub6<-Corpus[Corpus$region=="Oceania",]

sub1$date <- parse_date_time(sub1$date, "%b %d, %Y")
sub2$date <- parse_date_time(sub2$date, "%d-%b-%y")
sub3$date <- parse_date_time(sub3$date, "%d-%b-%y")
sub4$date <- parse_date_time(sub4$date, "%b %d, %Y")
sub5$date <- parse_date_time(sub5$date, "%d-%b-%y")
sub6$date <- parse_date_time(sub6$date, "%b %d, %Y")

newcorpus<-bind_rows(sub1,sub2,sub3,sub4,sub5,sub6)

missing <- which(is.na(newcorpus$date))

Corpus2<- newcorpus[-which(newcorpus$index %in% missing),]

mycorpus <- corpus(Corpus2)
stopwords_and_single<-c(stopwords("english"),"amp","just",LETTERS,letters)

dfm_press <- dfm(mycorpus,tolower = TRUE, remove_punct = TRUE,remove_numbers=TRUE, 
                 remove = stopwords_and_single,stem = FALSE,
                 remove_separators=TRUE) 

docnames(dfm_press) <- dfm_press@docvars$index

dfm_press2 <- dfm_trim(dfm_press, max_docfreq = 0.95, min_docfreq = 0.001, 
                       docfreq_type = "prop")

dfm_P <- delete.duplicates(dfm_press2, similarity = .95, 
                           keep = "first", tf.idf = FALSE)

dtm_lda <- convert(dfm_P, to = "topicmodels")
mycores<-detectCores()-1

full_data<-dtm_lda

n <- nrow(full_data)

rm(sub1,sub2,sub3,sub4,sub5,sub6)

###Find the appropiate K
memory.limit(120000)

print(Sys.time())
MainresultDF<-data.frame(k=c(1),perplexity=c(1),myalpha=c("x"))
MainresultDF<-MainresultDF[-1,]
candidate_alpha<- c(50) 
candidate_k <- c(seq(1,10)*10)

for (eachalpha in candidate_alpha) { 
  print ("now running ALPHA:")
  print (eachalpha)
  print(Sys.time())
  #----------------5-fold cross-validation, different numbers of topics----------------
  cluster <- makeCluster(detectCores(logical = TRUE) - 1) 
  registerDoParallel(cluster)
  
  clusterEvalQ(cluster, {
    library(topicmodels)
  })
  
  folds <- 5
  splitfolds <- sample(1:folds, n, replace = TRUE)
  #candidate_k <- c(2, 3, 4, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100) # candidates for how many topics
  
  clusterExport(cluster, c("full_data", "splitfolds", "folds", "candidate_k"))
  
  system.time({
    results <- foreach(j = 1:length(candidate_k), .combine = rbind) %dopar%{
      k <- candidate_k[j]
      print(k)
      results_1k <- matrix(0, nrow = folds, ncol = 2)
      colnames(results_1k) <- c("k", "perplexity")
      for(i in 1:folds){
        train_set <- full_data[splitfolds != i , ]
        valid_set <- full_data[splitfolds == i, ]
        
        #fitted <- LDA(train_set, k = k, method = "Gibbs",
                      #control = list(alpha=eachalpha/k,burnin = burnin, iter = iter, keep = keep) )
                      #control = list(alpha=eachalpha) )
                      
        fitted <- LDA(train_set, k = k, method = "Gibbs")
                                    
        results_1k[i,] <- c(k, perplexity(fitted, newdata = valid_set))
      }
      return(results_1k)
    }
  })
  stopCluster(cluster)
  
  results_df <- as.data.frame(results)
  results_df$myalpha<-as.character(eachalpha)
  MainresultDF<-rbind(MainresultDF,results_df)
}


MainresultDF$kalpha=paste0(as.character(MainresultDF$k),MainresultDF$myalpha) 
ggplot(MainresultDF) +geom_boxplot(aes(x=k, y=perplexity, group=kalpha,color=myalpha))

MainresultDF<-MainresultDF[order(MainresultDF$k),]

cars.spl <- with(MainresultDF, smooth.spline(k, perplexity, df = 3))
with(cars, predict(cars.spl, x = MainresultDF$k, deriv = 2))

plot(with(cars, predict(cars.spl, x = MainresultDF$k, deriv = 2)), type = "l")
abline(v=50)


###Run LDA
runsdf<-data.frame(myk=c(40,50))

mymodels<-list()

cluster <- makeCluster(detectCores(logical = TRUE) - 6) # leave one CPU spare...
registerDoParallel(cluster)

clusterEvalQ(cluster, {
  library(topicmodels)
})

#clusterExport(cluster, c("full_data", "burnin", "iter", "keep", "splitfolds", "folds", "candidate_k"))
clusterExport(cluster, c("full_data","runsdf"))

system.time({
  mymodels <- foreach(j = 1:nrow(runsdf)) %dopar%{
    k_run <- runsdf[j,1]
    #alpha_run<-runsdf[j,2]
    fitted <- LDA(full_data, k = k_run, method = "Gibbs",
                  #control = list(alpha=alpha_run,seed=267348) )
                  control = list(seed=3341) )
  }
})
stopCluster(cluster)

LDA <- mymodels[[2]]
save_filename<-"Refugees"



                 ####################################################################
                 ###############                                     ################
                 ###############          ANTMN Method               ################
                 ###############    by D. Walter & Y. Ophir (2019)   ################
                 ###############                                     ################
                 ####################################################################

##We choose the second model 
LDAfit<-mymodels[[2]]

###Add duplicates
docnamestoadd<-(setdiff(docnames(dfm_press2),docnames(dfm_P)))
deleted_dups<-dfm_press2[docnames(dfm_press2) %in% docnamestoadd,]
deleted_doc_sim<-documents.compare(deleted_dups,dfm_P)
deleted_doc_sim<-deleted_doc_sim[deleted_doc_sim$similarity>0.9,]

theta_df_no_dups<-cbind(LDAfit@documents,LDAfit@gamma)

matches_theta_results=list()

for (eachnum in 1:nrow(deleted_dups)){
  temp1<-deleted_doc_sim[deleted_doc_sim$x==as.character(docnames(deleted_dups)[eachnum]),]
  temp1<-temp1[order(temp1$similarity,decreasing = TRUE),]
  matchdoc<-temp1[1,2]
  

  temp2<-theta_df_no_dups[theta_df_no_dups[,1]==matchdoc,]
  temp2[1]<-as.character(docnames(deleted_dups)[eachnum])
  
  matches_theta_results[[eachnum]]<-temp2
  
}

matches_theta_results_DF<-as.data.frame(do.call(rbind, matches_theta_results))

theta_df_ALL<-rbind(theta_df_no_dups,matches_theta_results_DF)

theta_df_ALL_sorted<-theta_df_ALL
colnames(theta_df_ALL_sorted)<-c("index2",paste0("V",seq(1,50)))
theta_df_ALL_sorted<-theta_df_ALL_sorted[order(as.integer(theta_df_ALL_sorted$index)),]

sorted_meta<-data3[order(as.integer(data3$index)),]


meta_theta_df_ALL<-cbind(sorted_meta,theta_df_ALL_sorted)

meta_theta_df_ALL<-meta_theta_df_ALL[order(as.integer(meta_theta_df_ALL$index)),]

meta_theta_df_ALL[,8:((8+LDAfit@k)-1)]<- sapply(meta_theta_df_ALL[,8:((8+LDAfit@k)-1)],as.numeric)

#table(as.integer(meta_theta_df_ALL$index)-as.integer(meta_theta_df_ALL$index2))

save.image("searchk_10-100+final_models_40_50_DUPS_BACK.RData")


###Add size to documents
dfm_forsize<-data.frame(dfm_P)
indextemp<-dfm_forsize[,1]
dfm_forsize<-dfm_forsize[,-1]
sizevect<-rowSums(dfm_forsize)

index_and_size<-data.frame(index=indextemp,size=sizevect)

matches_size_results=list()

for (eachnum in 1:nrow(deleted_dups)){
  temp1<-deleted_doc_sim[deleted_doc_sim$x==as.character(docnames(deleted_dups)[eachnum]),]
  temp1<-temp1[order(temp1$similarity,decreasing = TRUE),]
  matchdoc<-temp1[1,2]
  
  temp2<-index_and_size[index_and_size[,1]==matchdoc,]
  
  matches_size_results[[eachnum]]<-c(as.character(docnames(deleted_dups)[eachnum]),temp2[1,2])
}

new_size_df<-as.data.frame(do.call(rbind, matches_size_results))
colnames(new_size_df)<-c("index","size")

size_df_ALL<-rbind(index_and_size,new_size_df)

size_df_ALL<-size_df_ALL[order(as.integer(size_df_ALL$index)),]
size_df_ALL[,2]<-as.integer(size_df_ALL[,2])

topic.frequency <- colSums(meta_theta_df_ALL[,8:ncol(meta_theta_df_ALL)]*as.vector(size_df_ALL[,2]))
topic.proportion <- topic.frequency/sum(topic.frequency)

deleted_topics <- c(7, 14, 16, 21, 40)

topic_names <- c("Cologne Sexual Attacks",
                 "Food",
                 "Syrian War",
                 "Rohignya Refugees",
                 "Isreal-Palestine Relations",
                 "Business",
                 "Boilerplate",
                 "EU-Turkey Relations",
                 "Muslim Issues",
                 "Syrian Refugees in Canada",
                 "US Elections",
                 "Terrorism in Europe",
                 "Demographic Changes",
                 "Boilerplate",
                 "Lebanon Foreign Relations",
                 "Boilerplate",
                 "Refugee Control in Europe",
                 "Debate on Europe's Position in the World",
                 "International Aid to Syrian Refugees",
                 "Kashmir Elections/Refugees",
                 "Boilerplate",
                 "Child Refugees in UK/Calais",
                 "Brexit",
                 "Refugees in Calais",
                 "Christian Community Events",
                 "Battle of Aleppo",
                 "Afghan Refugees in Pakistan",
                 "Terrorism in Europe",
                 "US Government Press Briefings",
                 "UN Condemnation of Terrorist Attacks",
                 "Crime by Refugees",
                 "Women and Child Refugees",
                 "South Sudan Refugees",
                 "Art",
                 "Iraq War Against IS",
                 "Economy",
                 "Elections in Austria",
                 "North Korea",
                 "Germany",
                 "Boilerplate",
                 "Economic/Sustainable Development",
                 "Art",
                 "Refugees in the Mediterranean Sea",
                 "Sports",
                 "Natural Disasters/Climate Change",
                 "Education",
                 "Media",
                 "EU Refugee Policies",
                 "Refugee Crisis in the EU",
                 "Australian Detention Centers")

##Run ANTMN
mynewnet<-network_from_LDA(LDAobject=LDA,
                           topic_size=topic.proportion,
                           deleted_topics=deleted_topics,
                           topic_names = topic_names,
                           save_filename= save_filename)
