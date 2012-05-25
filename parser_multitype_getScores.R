rm(list=ls(all=TRUE))
library('psych')
library('reshape')
options(scipen=5)

types =  c(6,9,12,24)
conds = c('uniform','zipfian')
numLanguages = 1
repeats = 10
forgetRates = seq(2,100, by=2)/1000

scores.store = data.frame()
modelfit.store = data.frame()

type_ind = 1
fr_ind = 1
c_ind = 1
numL_ind = 1
r_ind = 1

pseudosubjects.store = data.frame()
for (type_ind in c(1:length(types))){
	numTypes = types[type_ind]
	
	for (fr_ind in c(1:length(forgetRates))){
		forgetRate = forgetRates[fr_ind]			
		
		for (c_ind in c(1:length(conds))){
			cond = conds[c_ind]
					
			for (numL_ind in c(1:numLanguages)){	
				lang = numL_ind
				
				pseudosubject.runs = data.frame() # a psuedosubject averages over runs
				for (r_ind in c(1:repeats)){
					
					repeat_ind = r_ind
					
					run_score = getOnlineParserScores(lang,cond, repeat_ind, numTypes, forgetRate, 'token')
		
					pseudosubject.runs = rbind.fill(pseudosubject.runs, run_score)
				}
				pseudosubject =  aggregate.data.frame(pseudosubject.runs$fscore, list(pseudosubject.runs$index), FUN=mean)		
				names(pseudosubject)=c('trial','fscore')
				
				pseudosubject$cond = conds[c_ind]
				pseudosubject$forgetRate = forgetRates[fr_ind]
				pseudosubject$types = types[type_ind]
				
				pseudosubjects.store = rbind.fill(pseudosubjects.store, pseudosubject)
			}
		}
	}
}


dim(pseudosubjects.store)

	
write.table(pseudosubjects.store, '~/Desktop/PARSER_multitype_results.csv', sep=",", row.names=FALSE)	
	
	
wireframe(fscore ~ trial* forgetRate | types, groups=cond, data = pseudosubjects.store, main='PARSER  Performance on Zipfian and \nUniform Word Frequency Distributions', as.table=T, pch=20, cex=.2, scales =list(arrows=FALSE), auto.key=TRUE)

#separate by the number of types and get lattice xyplots for the 

quartz()
par(mfrow = c(2,2))

pdf('~/Desktop/PARSER_zipfseg_multitype_performance.pdf')
for (i in c(1:length(types))){
names(pseudosubjects.store)
i=24
typesubset = subset(pseudosubjects.store, types == i)

xyplot(fscore~trial | forgetRate, groups=cond, data=typesubset, cex=.2, auto.key=T, as.table=T, panel = panel.superpose, panel.groups = function(x, y, ...) {panel.loess(x,y,..., family='gaussian' )}, ylab='trial', main=paste('PARSER  Performance on Zipfian and \nUniform Word Frequency Distributions\n', i, 'types'))
}
dev.off()




plot(pseudosubjects.store$fscore ~ c(1:length(pseudosubjects.store$fscore)))
points(lowessed$fscore  ~ c(1:length(lowessed$fscore)), col='red')


#model fitting stuff

	scores.store = rbind.fill(scores.store, pseudopop)
		
		#plot(pseudopop$fscore ~ pseudopop$index, ylim=c(0,1))
		#points(reference_dataset[[comparison_conds[c_ind]]] ~ c(1:60))
		#plot(pseudopop$fscore ~ reference_dataset[[comparison_conds[c_ind]]])
	

	#populate modelfit
	#modelfit = data.frame(forgetRates[fr_ind])
	#names(modelfit) = 'forgetRate'				
	#lm = lm(pseudopop$fscore ~ reference_dataset_long$score)
	#modelfit$l_r.squared = summary(lm)$r.squared
	#modelfit$l_cor = cor.test(pseudopop$fscore,reference_dataset_long$score)$estimate[1]
	
	#lm2 = lm(pseudopop$fscore ~ reference_dataset_noL_long$score)
	#modelfit$noL_r.squared = summary(lm)$r.squared
	#modelfit$noL_cor = cor.test(pseudopop$fscore,reference_dataset_noL_long$score)$estimate[1]	
	
	#modelfit.store = rbind.fill(modelfit.store, modelfit )
}
}

write.table(scores.store, '/Users/behemoth/Dropbox/Stanford/ZIPFSEG/cognition_revisions/modeling/PARSER/parser_scores/zipf_uniform_parser_scores_50-100.csv', row.names=FALSE, sep=",")





getOnlineParserScores = function(lang,cond,repeat_ind, numTypes, forgetRate, fscore_type){ 

sentences = read.csv(paste('~/Dropbox/Stanford/ZIPFSEG/cognition_revisions/modeling/PARSER/parser_output/', numTypes, '_c_', cond, '_s', lang,'_fr', forgetRate ,'_r', repeat_ind, '_sent.csv', sep=""), header=F) 
names(sentences) ='sentence'

sentences$correct = gsub('^#','',as.character(sentences$sentence), perl=TRUE)
sentences$correct = gsub('#$','',as.character(sentences$correct), perl=TRUE)


segmented = read.csv(paste('~/Dropbox/Stanford/ZIPFSEG/cognition_revisions/modeling/PARSER/parser_output/', numTypes, '_c_', cond, '_s', lang,'_fr', forgetRate ,'_r',  repeat_ind, '.csv', sep=""), header=F) 
names(segmented) = 'as.segmented'
sentences$as.segmented = gsub('^#','',as.character(segmented$as.segmented), perl=TRUE)
sentences$as.segmented = gsub('#$','',as.character(sentences$as.segmented), perl=TRUE)
sentences$as.segmented = gsub('^ ','',as.character(sentences$as.segmented), perl=TRUE)
sentences$as.segmented = gsub(' $','',as.character(sentences$as.segmented), perl=TRUE)


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
	for (i in c(1:length(sentences$sentence))){
		#this is actually a simpler calculation
		sentences$correct[i]
		sentences$as.segmented[i]
		
		correctTokens = strsplit(as.character(sentences$correct[i]),split=' ')[[1]] 
		
		foundTokens = strsplit(as.character(sentences$as.segmented[i]),split=' ')[[1]] 
		if (length(foundTokens)>0 ){
			sentences$precision[i] = length(which(correctTokens %in% foundTokens))/length(foundTokens)
		} else {
			sentences$precision[i] = 0
		}
		
		sentences$recall[i] = length(which(foundTokens %in% correctTokens))/length(correctTokens)
	
		#is this robust to short foundTokens-- now it is
		#is this robust to multiple instances of the same in either case?-- yes
		
		sentences$fscore[i] = harmonic.mean(c(sentences$precision[i], sentences$recall[i] ))
		sentences$index[i] = i
	}	
}



sentences$lang = lang
sentences$cond = cond
sentences$repeat_ind = repeat_ind
sentences$forgetRate = forgetRate
sentences$fscore_type = fscore_type 

return(sentences)
}