#################
#################
#What we are doing:
## 1. Preprocessing functions in base R
## 2. LDA wrapper
## 3. Dictionary functions using base R
## 4. Machine Learning wrapper (with Juan)
#
#################
#Assumptions:
## English stopwords
##
##
#################
#Authors: Berkeley D-Lab Computational Text Analysis Working Group (CTAWG)
#Date: 3/11/2015
#Updated: 3/11/2015 by Nick and Brooks
#
#################


fulltext2lda<-function(
in.dir=stop('in.dir: You need to specify a path to a file folder containing the text files (i.e. documents) that comprise your corpus. For example in.dir=\'MyDocuments/ProjectFiles/TextDocuments\'.')
,out.dir=stop('out.dir: You need to specify a path to a file folder where you want your output files to be stored once text processing is completed. For example out.dir=\'MyDocuments/ProjectFiles/Output\'.')
,k=stop('k: Specify the number of topics you want LDA to model.')
,alpha=50/k
)
{
require(tm)
require(SnowballC)

    if(alpha!=50/k) cat(paste('\nalpha: You have chosen an alpha of ',round(alpha,3),' The alpha parameter must be greater than 0. Alpha < 1 assumes that each document is constructed from relatively few topics. Alpha > 1 assumes that each is constructed from many topics.',sep=''))

### 1. Preprocessing functions in base R ###

docs<-list() # container for docs
for(i in list.files(in.dir)) docs[[i]]<-readLines(i,warn=F)

docs<-lapply(docs,FUN=paste,collapse=' ') # each doc is one long character string
docs<-sapply(docs,FUN=strsplit,split='[:space:]+') # split docs into words
docs<-lapply(docs,FUN=tolower) # transform to lower case
docs<-lapply(docs,FUN=removePunctuation) # ...
docs<-lapply(docs,FUN=removeNumbers)
docs<-lapply(docs,FUN=removeWords,stopwords('english'))
docs<-lapply(docs,FUN=stemDocument,language='english')
## here we end with a list of untabulated character vectors in original document order. It is possible to go in the 'bag of words' direction, to create ngrams, or to do grammatical parsing...maybe (is stemming a problem here?)


### 2. LDA wrapper (stm package)
vocab<-sort(unique(unlist(docs)) # stm expects as input a list of matrices where each element of the list is a document and where the first row of each matrix is the index of a word and the second row is the count of the word
docs<-lapply(docs,FUN=function(x) {
x<-factor(x,levels=vocab)
x<-rbind(1:length(vocab),table(x))
x # this is functionally a document term matrix if you were to run do.call(rbind,x). It includes the zero values, but behavior for this is not described in stm.
}
)

pre2stm<-stm(



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
,n=stop('n:You need to specify the number of characters within which you want to search for nearby words. The word blue contains 4 characters, so maybe you want to set this to 20, or 50.'
){

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




###4. Machine Learning wrapper (with Juan)


###5. STM wrapper
