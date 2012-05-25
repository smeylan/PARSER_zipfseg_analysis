getTxtFileFscores = function(input, fscore_type){
#get the fscores for a model output text file and the one with the correct spacings	

sentences$correct = unlist(lapply(read.csv(input$correct, header=F, colClasses="character")$V1, FUN=function(x) substr(x,2,nchar(x)-1)))  

segmented = read.csv(input$segmented, header=F, colClasses="character") 
sentences$as.segmented = unlist(lapply(segmented$V1, FUN=function(x) substr(x,2,nchar(x))))


if(fscore_type == 'boundary'){
	for (i in c(1:length(sentences$sentence))){
		
		correct.withseg = gsub('','%',sentences$correct[i])
		correct.withseg = gsub('% %',' ',correct.withseg)
		correct.withseg = gsub('^%','',correct.withseg)
		correct.withseg = gsub('%$','',correct.withseg)
	
		answer.withseg = gsub('','%',sentences$as.segmented[i])
		answer.withseg = gsub('% %',' ',answer.withseg)
		answer.withseg = gsub('^%','',answer.withseg)
		answer.withseg = gsub('%$','',answer.withseg)	
		if(nchar(correct.withseg) != nchar(answer.withseg)){stop('length mismatch')}
		
		# get the indices 
		correct.indices = which(strsplit(correct.withseg,split='')[[1]] == ' ')
		answer.indices = which(strsplit(answer.withseg,split='')[[1]] == ' ')
		#this inflates the f-scores unfairly by counting the utterance boundaries

		if (length(answer.indices)>0 ){
			sentences$precision[i] = length(which(answer.indices %in% correct.indices))/length(answer.indices)} else {
			sentences$precision[i] = 0
		}
		sentences$recall[i] = length(which(correct.indices %in% answer.indices))/length(correct.indices)
		sentences$fscore[i] = harmonic.mean(c(sentences$precision[i], sentences$recall[i] ))
		sentences$index[i] = i
	}
} else if (fscore_type == 'token'){
sentences.short = sentences[,c('correct','as.segmented')]
sentences.short$index = c(1:length(sentences.short$as.segmented))
itemList = split(sentences.short, with(sentences.short,index))
sentenceScoresAll = lapply(itemList, getSentenceScore)
sentenceScores = do.call("rbind", sentenceScoresAll)	
}

return(sentences)
}

getSentenceScore = function(sentence){
	correctTokens = strsplit(sentence$correct,split=' ')[[1]] 
	foundTokens = strsplit(sentence$as.segmented, split=' ')[[1]]
	if (length(foundTokens)>0 ){
			sentence$precision = length(which(correctTokens %in% foundTokens))/length(foundTokens)
	} else {
			sentence$precision = 0
	}
	
	sentence$recall = length(which(foundTokens %in% correctTokens))/length(correctTokens)
	
	sentence$fscore = harmonic.mean(c(sentence$precision, sentence$recall))
	return(sentence)	
}