# Da Do Run Run, actually Da Doo Ron Ron: https://www.youtube.com/watch?v=uTqnam1zgiw
# A script like this is the best way to use the CTAWG.R function
# When loading a function from source, we can take advantage of development tools and syntax checks.

do.you.want.to.clear.memory="nope"
if(grepl("[Yy]",do.you.want.to.clear.memory)) rm(list=ls())

Would.you.like.to.lineprof="nope"
if(grepl("[Yy]",Would.you.like.to.lineprof)){
	require(lineprof) # optional: a Hadley Wickam tool for line by line visualization of a function's memory and running time
	source("CTAWG.R")
	sh<-lineprof(findings<-fulltext2lda(in.dir="/in",out.dir="/out",k=3,alpha=.5,sample.docs=5))
	shine(sh)
}

source("CTAWG.R")
proquest<-fulltext2lda(in.dir="./ProQuestDescriptions",out.dir="./ProQuestOut",k=20,alpha=50/k)
