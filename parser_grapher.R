library('rgl')

parserHighForget = read.csv('/Users/behemoth/Dropbox/Stanford/ZIPFSEG/cognition_revisions/modeling/PARSER/parser_scores/zipf_uniform_parser_scores_50-100.csv')

head(parserHighForget)

xyplot(fscore ~ index | forgetRate, groups=cond, data= parserHighForget, type='l', auto.key=TRUE, as.table=T)

parserRegularForget = read.csv('/Users/behemoth/Dropbox/Stanford/ZIPFSEG/cognition_revisions/modeling/PARSER/parser_scores/zipf_uniform_parser_scores.csv')

xyplot(fscore ~ index | forgetRate, groups=cond, data= parserRegularForget, type='l', auto.key=TRUE, as.table=T)

parser  = rbind.fill(parserRegularForget, parserHighForget)

xyplot(fscore ~ index | forgetRate, groups=cond, data= parser, type='l', auto.key=TRUE, as.table=T)


cloud(fscore ~ index * forgetRate, groups=cond, data = parser, main='PARSER  Performance on Zipfian (red) and \nUniform (green) Word Frequency Distributions', as.table=T, pch=20, cex=.2, scales =list(arrows=FALSE))


uniform = subset(parser, cond == 'uni-1-6')
zipf = subset(parser, cond = 'zipf-1-6')



lines3d(zipf$fscore, zipf$index, zipf$forgetRate, col='blue')

plot3d(uniform$index, uniform$fscore, uniform$forgetRate, col='red', type='l')