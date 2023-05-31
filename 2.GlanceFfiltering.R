######################################################
# GLanCE Training Data Analysis and Filtering Script #
######################################################

###################  PART 2  ######################### 
#  Look at global patterns in sites and filter at    #
#  continental scale                                 #
######################################################

# run set up in read and pre-process GLanCE training data
# source('Rscripts/1.GlanCETrainingSetup.R')

# subset LC class and features
featureDat <- glancedat[,c(lc.class[3],blue,green,red,nir,swir1,swir2,lst,topo,climate)]

# convert LC class to a factor for classification
featureDat[,1] <- factor(featureDat[,'LC_Class'])

# remove sin/cos coefs from features
cos.sins <- c(5:7,13:15,
              21:23,29:31,
              37:39,45:47,
              53:55,61:63,
              69:71,77:79,
              85:87,93:95,
              101:103,109:111)
good.cols <- !((1:dim(featureDat)[2]) %in% cos.sins)
featureDat <- featureDat[,good.cols]
nfeat <- dim(featureDat)[2]-1

# save a version of original - need to be able to restore later
featureDat.Base <- featureDat

# set number of trees for Random Forest
ntrees=500

# set number of classes
nclass <- length(unique(featureDat[,1]))

# estimate and plot variable importance
ranger.perm <- ranger(LC_Class~.,data=featureDat,num.trees=ntrees,importance='permutation')

# select 50 most important features - based on trial and error - not super sensitive
best.features <- names(sort(ranger.perm$variable.importance))[(nfeat-49):nfeat]
featureDat <- featureDat[,c('LC_Class',best.features)]

# Estimate base model 
ranger.base <- ranger(LC_Class~.,
                     data=featureDat,
                     num.trees=ntrees,
                     importance='none',
                     probability=FALSE)
names(ranger.base$predictions) <- rownames(featureDat)

# confusion matrix and accuracy
ranger.base$confusion.matrix
1-ranger.base$prediction.error

########## Filtering Step 1: get rid of bat-shit crazy confusion cases  ###########
 
bad.pix <-(featureDat[,'LC_Class'] == 'Bare' & ranger.base$predictions == 'Forest') |
  (featureDat[,'LC_Class'] == 'Bare' & ranger.base$predictions == 'Water') |
  (featureDat[,'LC_Class'] == 'Developed' & ranger.base$predictions == 'Water') |
  (featureDat[,'LC_Class'] == 'Developed' & ranger.base$predictions == 'Snow/Ice') |
  (featureDat[,'LC_Class'] == 'Forest' & ranger.base$predictions == 'Bare') |
  (featureDat[,'LC_Class'] == 'Forest' & ranger.base$predictions == 'Water') |
  (featureDat[,'LC_Class'] == 'Forest' & ranger.base$predictions == 'Herbaceous') |
  (featureDat[,'LC_Class'] == 'Herbaceous' & ranger.base$predictions == 'Forest') |
  (featureDat[,'LC_Class'] == 'Herbaceous' & ranger.base$predictions == 'Water') |
  (featureDat[,'LC_Class'] == 'Herbaceous' & ranger.base$predictions == 'Snow/Ice') |
  (featureDat[,'LC_Class'] == 'Shrub' & ranger.base$predictions == 'Water') |
  (featureDat[,'LC_Class'] == 'Shrub' & ranger.base$predictions == 'Snow/Ice') |
  (featureDat[,'LC_Class'] == 'Snow/Ice' & ranger.base$predictions != 'Snow/Ice') |
  (featureDat[,'LC_Class'] == 'Water' & ranger.base$predictions != 'Water') 

# and re-estimate model with new subsetted training data
ranger.sub1 <- ranger(LC_Class~.,
                     data=featureDat,
                     num.trees=ntrees,
                     importance='none',
                     probability=FALSE)
names(ranger.sub1$predictions) <- rownames(featureDat)

# Estimate ranger in probability mode to so we can get margins
ranger.sub1.probs <- ranger(LC_Class~.,
                           data=featureDat, 
                           num.trees=ntrees,
                           importance='none',
                           probability=TRUE)$predictions
ranger.probs.preds <- colnames(ranger.sub1.probs)[apply(ranger.sub1.probs,1,which.max)]

# compute margin on first, second most likely class, predicted class, true class
margins.all <- getRangerMarg(trueClass=featureDat[,1],
                             predClass=ranger.sub1$predictions,
                             probs=ranger.sub1.probs)

# let's look at mis-classified
misclassified.pix <- subset(margins.all,margins.all[,'missClass'])  # ~3700 pixels

# get upper value on upper 1/2 of margins for misclassified!
egreg.thresh.high <- boxplot(margins.all[,'Margin']~margins.all[,'missClass'],plot=F)$stats[3,2] # upper 2 quartiles 

##### Filtering step 2: remove based on egregious & low margin cases  ####
low.margin.thresh <- 0.05

# split training data into egregious/low margin versus non-egregious sufficient margin
bad.pix <- (margins.all[,'missClass'] & (margins.all[,'Margin'] > egreg.thresh.high)) | # egregiously misclassified
  (margins.all[,'Margin'] < low.margin.thresh)                                          # indistinguishable

# Remove "bad pixels"
goodTraining <- featureDat[!bad.pix,]

# write data sets to file.
clean.rows <- rownames(goodTraining)
clean.dat.allfeatures <- glancedat[clean.rows,]
write.csv(glancedat[clean.rows,c(1,51:2)],file='data/NA_Filtered_70Features.csv',row.names=FALSE)
write.csv(clean.dat.allfeatures,file='data/NA_Filtered_AllFeatures.csv',row.names=FALSE)

