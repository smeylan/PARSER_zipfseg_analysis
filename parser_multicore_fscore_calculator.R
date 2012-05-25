#generate all of the filenames
library('psych')
library('multicore')
library('matlab')
source('/Volumes/HD/Local_Stanford/Zipfseg_PARSER/getTxtFileFscores.R')

subjects =  c(1:12)
conds = c('uni', 'zipf')
numTypes = c(6, 9, 12, 24)

runsPerLanguage = c(1:5);
maxUnits_collection = c(2, 3, 4)
shapingThresholds = 1 
forgetRates = seq(5,100,5)/1000
interfereRates = c(.004, .005, .006)
reactivationGains = c(.4, .5, .6)
beginWeights = c(1,1.05,1.1)


length(runsPerLanguage) * length(maxUnits_collection) * length(shapingThresholds) * length(forgetRates) * length(interfereRates) * length(reactivationGains) * length(beginWeights) * length(subjects) * length(conds) * length(numTypes)



allCombinations = expand.grid(subjects, conds, numTypes, runsPerLanguage, maxUnits_collection, shapingThresholds, forgetRates, interfereRates,reactivationGains,beginWeights)

dim(allCombinations)
names(allCombinations) = c('sub', 'cond', 'types', 'run', 'mu', 'st', 'fr','ir','rg', 'bw')

head(allCombinations)
#777k files

basepath = '/Volumes/HD/Local_Stanford/Zipfseg_PARSER/PARSER_multicore/' 

#[conds{cond} '_' num2str(expt) '/subject_' num2str(sub) '/fr_' num2str(forgetRate) '/']

allCombinations$folder = paste(allCombinations$cond, '_', allCombinations$types,'/subject_', allCombinations$sub, '/fr_', allCombinations$fr, '/', sep="")

#fileName = ['run' num2str(runIndex) '_mu' num2str(maxUnits) '_ir' num2str(interfereRate) '_rg' num2str(reactivationGain) '_bw' num2str(beginWeight)];

allCombinations$filename = paste('run',  allCombinations$run, '_mu', allCombinations$mu, '_ir', allCombinations$ir, '_rg', allCombinations$rg, '_bw', allCombinations$bw, sep="")


allCombinations$correct = paste(basepath, 'correct/', allCombinations$folder,allCombinations$filename,'_sent.csv', sep="")
allCombinations$segmented = paste(basepath, 'segmented/', allCombinations$folder,allCombinations$filename,'.csv', sep="")

#now we have the filenames of all of the model outputs and correct files

allCombinations.short =  allCombinations[11500:11600,]


combinationItems = split(allCombinations.short, with(allCombinations.short,segmented))

fscores.store = data.frame
tic()
fscores.store = lapply(combinationItems, function(x) getTxtFileFscores(x, 'token'))
toc()
fscores.store = do.call("rbind", fscores.store)





singleRun = .033
(singleRun * 777000) #seconds
(singleRun * 777000) / 60 #minutes
(singleRun * 777000) / 60 / 60 # 27 hours







tic()
test = getTxtFileFscores(allCombinations$segmented[1], allCombinations$correct[1], 'token')
toc()

# problem with the file reading-- the model didn't work on everything














#write 