# Da Do Run Run, actually Da Doo Ron Ron: https://www.youtube.com/watch?v=uTqnam1zgiw
# A script like this is the best way to use the CTAWG.R function
# When loading a function from source, we can take advantage of development tools and syntax checks.

Would.you.like.to.lineprof="yup"
if(grepl("[Yy]",Would.you.like.to.lineprof)){
	require(lineprof) # optional: a Hadley Wickam tool for line by line visualization of a function's memory and running time
	source("CTAWG.R")
	sh<-lineprof(findings<-fulltext2lda())
	shine(sh)
}

