#################
#################
# What we are doing:
## 1. Preprocessing functions in base R
## 2. LDA wrapper
## 3. Dictionary functions using base R
## 4. Machine Learning wrapper (with Juan)
#
#################
# Assumptions:
## English stopwords
##
##
#################
# Authors: Berkeley D-Lab Computational Text Analysis Working Group (CTAWG)
# Date Created: 3/11/2015
# Last Updated: 3/17/2015 by Brooks
#
#################
# Collaboration Guidelines:
#
## CollabEdit:
###### Source: http://collabedit.com/w7m72
###### Run:    http://collabedit.com/ga4nw
##
### The CollabEdit page for this source file is a space for live coding with a group. What you see here will update on others' screens. View the version history here: http://collabedit.com/w7m72/history. Please make sure work done in CollabEdit contributes to the Git repo by copying it to your fork and submitting it as a pull request. The Run page provides an example script to run the function with arguments using a file structure like the one in the Git repository.
#
## GitHub: https://github.com/brooksambrose/CTAWG
##
### Feel free to contribute callone (collaborate alone) to the git repo for this project by issuing pull requests! Also available in the repo is a script (dadorunrun.R) that shows how to load a function as a source file.
##
#################


fulltext2lda<-function(
in.dir=stop('in.dir: You need to specify a path to a file folder containing the text files (i.e. documents) that comprise your corpus. For example:\nin.dir=\'MyDocuments/ProjectFiles/TextDocuments\'.\n')
,out.dir=stop('out.dir: You need to specify a path to a file folder where you want your output files to be stored once text processing is completed. For example out.dir=\'MyDocuments/ProjectFiles/Output\'.\n')
,k=stop('k: Specify the number of topics you want LDA to model.\n')
,alpha=cat('alpha: The alpha parameter must be greater than 0.\nAlpha < 1 assumes that each document is constructed from relatively few topics.\nAlpha > 1 assumes that each is constructed from many topics.\nIf you aren\'t sure choose the convention: 50/k, which will also be the default if you specify nothing.\n')
,sample.docs=NULL # put a number here if you want to take a random subset of your docs
,visualize.results=F
)
{
# Check package requirements and arguments
require(tm)
require(SnowballC)
require(stm)

if(is.null(alpha)) alpha<-50/k

### 1. Preprocessing functions in base R ###

docs<-list() # container for docs
files<-list.files(in.dir,full.names=T)
if(!is.null(sample.docs)) files<-sample(files,sample.docs)
files<-files[order(sub(paste('.*','(.+)',sep=.Platform$file.sep),'',files))] # helps later to have files in alpha order by document name
for(i in files) docs[[i]]<-readLines(i,warn=F)
docs<-lapply(docs,FUN=paste,collapse=' ') # each doc is one long character string
docs<-lapply(docs,FUN=tolower) # transform to lower case
docs<-lapply(docs,FUN=removePunctuation) # ...
docs<-lapply(docs,FUN=removeNumbers)
docs<-sapply(docs,FUN=strsplit,split='\\s+') # split docs into words "\\s+" is a regex. "[[:space:]]+" also works but is R specific
docs<-lapply(docs,FUN=removeWords,stopwords('english'))
docs<-lapply(docs,FUN=function(x) x[!!nchar(x)]) #remove blanks
docs<-lapply(docs,FUN=stemDocument,language='english')

## here we end with a list of untabulated character vectors in original document order. It is possible to go in the 'bag of words' direction, to create ngrams, or to do grammatical parsing...maybe (is stemming a problem here?)

### 2. LDA wrapper (stm package)
vocab<-sort(unique(unlist(docs))) # stm expects as input a list of matrices where each element of the list is a document and where the first row of each matrix is the index of a word and the second row is the count of the word. This is a more memory efficient form since zeros are not stored.
stm.docs<-list()
for(i in names(docs)){
	t<-table(docs[[i]])
	stm.docs[[i]]<-rbind(vocab.index=which(vocab%in%names(t)),frequency=t)
}

pre2stm<-list()
pre2stm$model<-stm(documents=stm.docs,vocab=vocab,K=k,control=list(alpha=alpha))
pre2stm$top.word.phi.beta<-sapply(data.frame(pre2stm$model$beta$logbeta),function(x) sapply(x,function(y) ifelse(is.infinite(y),.Machine$double.eps,exp(y)))) # called beta by stm, epsilon closest thing to zero the machine can represent, necessary to prevent error
colnames(pre2stm$top.word.phi.beta)<-pre2stm$model$vocab
pre2stm$doc.top.theta<-pre2stm$model$theta
rownames(pre2stm$doc.top.theta)<-names(docs)
pre2stm$doc.length<-sapply(docs,length)
pre2stm$vocab<-pre2stm$model$vocab
tn<-table(unlist(docs))
pre2stm$term.frequency<-as.integer(tn)
names(pre2stm$term.frequency)<-names(tn)

save(pre2stm,file=sub(paste(rep(.Platform$file.sep,2),collapse=""),.Platform$file.sep,paste(out.dir,paste("stm-model-k",k,"-alpha",round(alpha,3),".RData",sep=""),sep=.Platform$file.sep)))

pre2stm
}

stmviz<-function(pre2stm,thresh="choose"){
	# from http://cpsievert.github.io/LDAvis/newsgroup/newsgroup.html
	# http://nlp.stanford.edu/events/illvi2014/papers/sievert-illvi2014.pdf
	# http://glimmer.rstudio.com/cpsievert/xkcd/
	require(LDAvis)
	require(servr)
	require(network)

	bam<-pre2stm$doc.top.theta
	b<-quantile(bam,seq(0,1,.05))
	h<-hist(bam,breaks=b,col="black")
	abline(v=b,col=gray(0,.5))
	text(
		x=rev(h$breaks)
		,y=seq(0,max(h$density),length.out=21)
		,labels=rev(paste('|',names(b),' <= ',round(b,3),sep=''))
		,pos=4
		,offset=0
		,cex=1
		)
	if(thresh=="choose"){
		cat("\nPlease choose an edge weight threshold by clicking on the histogram near the x-axis where you would like to cut the distribution of probabilities that a document draws words from a particular topic (i.e. theta or the document-topic probability matrix). Relationships between documents and topics that fall below this threshold will be ignored.\n")
	thresh<-locator(n=1,type="p",col="red")
	abline(v=thresh$x,col="red")
	text(
		x=thresh$x
		,y=thresh$y
		,labels=rev(paste('|',round(mean(bam<thresh$x)*100,2),'% <= ',round(thresh$x,3),sep=''))
		,pos=4
		,offset=0
		,cex=1
		,col="red"
		)
	}
	bam[bam<thresh$x]<-0
	m1am<-bam%*%t(bam)
	m1net<-network(m1am,directed=F,loops=F)
	network.vertex.names(m1net)<-sub(paste('.*','(.+)',sep=.Platform$file.sep),'\\1',rownames(m1am))
	pdf('doc-by-top-net.pdf')
	plot(m1net
			 ,displaylabels=T
			 ,label=paste('T',1:nrow(m1am),sep='')
			 ,label.pos=5
			 ,label.col="white"
			 ,label.cex=.25
			 ,vertex.col="black"
			 ,vertex.cex=2
	)
	dev.off()

	m2am<-t(bam)%*%bam
	m2net<-network(m2am,directed=F,loops=F)

	pdf('top-by-doc-net.pdf')
	plot(m2net
			 ,displaylabels=T
			 ,label=paste('D',1:nrow(m2am),sep='')
			 ,label.pos=5
			 ,label.col="black"
			 ,label.cex=.75
			 ,vertex.col="white"
			 ,vertex.cex=2
	)
	dev.off()

# 	b<-quantile(m1am,seq(0,1,.1))
# 	h<-hist(m1am,breaks=b)
# 	abline(v=b,col'pink')
# 	h<-hist(m2am,breaks=quantile(m2am,seq(0,1,.1)))

	bel<-which(bam>0,arr.ind=T)
	w<-bam[bel]
	bel<-cbind(sub(paste(".+","(.+)",sep=.Platform$file.sep),"\\1",rownames(bel)),paste("t",bel[,2],sep=""))
	o<-order(bel[,1],bel[,2])
	bel<-data.frame(bel[o,])
	w<-w[o]
	colnames(bel)<-c("document","topic")
	nm1<-length(unique(bel$document))
	nm2<-length(unique(bel$topic))
	bnet<-network(bel,bipartite=nm1,matrix.type="edgelist")
	bnet%e%"w"<-w
pdf('bimodal-net.pdf')
	plot(
		bnet
		,displaylabels=T
		,label=c(
			paste(1:nm1)
			,network.vertex.names(bnet)[-(1:nm1)]
		)
		,label.pos=5
		,label.col=c(rep("white",nm1),rep("black",nm2))
		,label.cex=.75
		,vertex.col=c(rep("black",nm1),rep("white",nm2))
		,vertex.cex=2
		#,vertex.sides=c(rep(3,nm1),rep(20,nm2))
	)
dev.off()

	json <- createJSON(
		phi = pre2stm$top.word.phi.beta
		,theta = pre2stm$doc.top.theta
		,vocab = pre2stm$vocab
		,doc.length = pre2stm$doc.length
		,term.frequency = pre2stm$term.frequency
	)
	save(json,file='viz.RData')
	serVis(json, out.dir = "vis", open.browser = T)
}

###3. Dictionary functions using base R
# arguments are in.dir, keys(as in keywords), n(for functions seeking nearby words), and out.dir.   ##??Do we want to add meta-data to make it easy for people to make crosstabs of the counts?? If so, we need to add arguments specifying the meta data columns of an output file.
#Functions described below
#a. counts: Output sought: Frequency counts of keywords for each document
#b. show.near: Output sought: Display words within 'n' words of keywords within each document
#c. count.near: Output sought: Display frequency counts for co-occurrence (within 'n' characters of one another (within the same document)) of two (or more) different keywords within a document. Will display all co-occurrences if keyword list is not shortened.
#d. show.assoc: (using 'findAssocs' function from tm)Output sought: Display words associated with search terms (i.e. display the top ten words appearing in the same document as a search term and a measure of their association with the keywords.

###b
show.near<-function(
in.dir=stop('in.dir: You need to specify a path to a file folder containing the text files (i.e. documents) that comprise your corpus. For example in.dir=\'MyDocuments/ProjectFiles/TextDocuments\'.')
,out.dir=stop('out.dir: You need to specify a path to a file folder where you want your output files to be stored once text processing is completed. For example out.dir=\'MyDocuments/ProjectFiles/Output\'.')
,keys=stop('keys: You need to specify a path to a tab-delimited, or comma-separated text file containing a list of all keywords you wish to search for. For example keys=\'MyDocuments/ProjectFiles/keywords.csv\'.')
,n=stop('n:You need to specify the number of characters within which you want to search for nearby words. The word blue contains 4 characters, so maybe you want to set this to 20, or 50.')
)
{

keys <- c('mayor', 'Mayor', 'Chief', 'chief', 'governor', 'Governor', 'spokesperson', 'Spokesperson', 'spokesman', 'Spokesman', 'spokeswoman', 'Spokeswoman', 'Senator', 'Representative', 'Justice', 'Judge', 'attorney', 'Attorney', 'General', 'DA', 'D.A.', 'County', 'Chair', 'City', 'Vice', 'Aide', 'Deputy', 'Assistant', 'Manager', 'Commissioner', 'Mayoral', 'Committee', 'Planner', 'Director', 'Fire', 'Administrator', 'Public', 'Health', 'Parks', 'Recreation', 'Official', 'Works', 'Sanitation', 'Codes', 'Inspector', 'Relations', 'Forensics', 'Council', 'Councilman', 'Councilwoman', 'Councilmen', 'Councilwomen', 'Councilmember', 'Councilmembers', 'Assembly', 'Assemblyman', 'Assemblymen', 'Assemblywoman', 'Assemblywomen', 'Port', 'Authority', 'Police', 'Lieutenant', 'Sergeant', 'Captain', 'Cpt.', 'Commander', 'Sherrif', 'Superintendent', 'Colonel', 'Major', 'Ranger', 'Detective', 'Officer', 'leader', 'Leader', 'Representative', 'Labor'  )

keys

# Initialize empty lists / vectors that we'll assemble into a data.frame below
resultids <- integer()
cities <- c()
states <- c()
artcl_id <- integer()
result <- list()

# Loop over each row in the table, appending to containers above if we get hits
for(idx in 1:nrow(data97)) {
  toks <- unlist(strsplit(data97[idx,'article_text'], ' '))
  curhits <- which(toks %in% keys)
  if(length(curhits) > 0) {
    # We got something from keys! append to each of our (eventual) data.frame columns
    for(hit in curhits) {
      #result <- append(result, list(c(toks[hit-1], toks[hit], toks[hit+1], toks[hit+2], toks[hit+3])))
      beg <- hit - 2
      end <- hit + 4
      if(beg < 1) beg <- 1
      if(end > length(toks)) end <- length(toks)
      result <- append(result, paste(toks[(beg):(end)], collapse=' '))
      resultids <- append(resultids, idx)
      cities <- append(cities, data97[idx,'city'])
      states <- append(states, data97[idx,'state'])
      artcl_id <- append(artcl_id, data97[idx,'article_id'])
    }
  }
}
result
resultids


#constructing dataframe from our results
res.df <- data.frame(idx=resultids, article_id=artcl_id, city=cities, state=states, neighborhood=unlist(result))
#res.df$neighborhood
#res.df[26,]

write.csv(res.df, 'happiness2.csv') #this csv has all we need
}

###4. Machine Learning wrapper (with Juan)

###5. STM wrapper

