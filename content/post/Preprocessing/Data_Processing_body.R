# Get data from OSF
body <- foreign::read.spss('https://osf.io/43xv5/download',
  to.data.frame = TRUE, use.value.labels = FALSE)

# Selecting relevant variables
body <- subset(body, select = c('id', 'Age', 'Sex',
  'IAS_TotalScore', 'BPQ_BodyAwareness'))

# renaming variables
names(body)[4:5] <- c('IAS', 'BPQ')
