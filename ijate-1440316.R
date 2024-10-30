# Supplementary R script for the article:
# Aybek, E. C., Arıkan, S., & Ertaş, G. (2024). A practical guide to item bank calibration with multiple 
# matrix sampling. International Journal of Assessment Tools in Education, 11(4), 647-659. https://doi.org/10.21449/ijate.1440316

# This script merges the raw data from the Concerto platform with the customized data that contains the demographic information.
# Then makes the item calibrations with the multipleGroup() and mirt() functions of the mirt package.


# Read the Raw Concerto Data
rawData <- readxl::read_xlsx("./data/concerto-ham2.xlsx") # Revise the path as your own case

# Read the Customized Concerto Data (Demographics, etc.)
customData <- readxl::read_xlsx("./data/linearResponse.xlsx") # Revise the path as your own case

# Convert rawData to Wide Format
rawData <- dplyr::as_tibble(rawData)
rawData <- reshape2::dcast(rawData,
                           session_id ~ item_id,
                           value.var = "score")
rawData <- tibble::as_tibble(rawData)

# Changing the Column Name session to session_id
# for the Customized Data. This is also unnecessary
# if you set your variable name as session_id at the first place
colnames(customData)[2] <- "session_id"

# Merging Customized Data and the Transformed Wide Formatted Data
mergedData <- merge(customData, rawData, by = "session_id", all.y = TRUE)

# Removing the Unnecessary Columns that Came from the Customized Data
remove <- c(7, 8, 9, 11)
mergedData <- mergedData[,-remove]

# Removing the all NA rows
rows2rm <- which(is.na(mergedData$ad) & # Name
                   is.na(mergedData$no) & # School number
                   is.na(mergedData$cinsiyet) & # Gender
                   is.na(mergedData$okul)) # School
mergedData <- mergedData[-rows2rm, ]

# Removing the first six columns which contains demographics.
mergedData <- mergedData[,-c(1:6)]

# Changing the Column names as testId and Item IDs.
colnames(mergedData) <- c("testId", paste0("X", colnames(mergedData)[-1]))

# Extracting column names for using later to indexing the items to remove
nms <- colnames(mergedData)

# Set group variable as testId
group <- mergedData$testId
group <- as.numeric(gsub("\\D", "", group))
group <- paste0("g", group)

# Removing the items that have problems
mergedData <- mergedData[,-which(nms == "X4121102" | nms == "X4131502" | nms == "X4331103" | nms == "X4115102")]
mergedData$testId <- group # Assign testIds to group variable [basically it will be g1, g2, g3 instead of booklet1, booklet2, booklet3]

# Updating the nms with the new column names
nms <- colnames(mergedData)

# Extracting the number of groups (booklets)
nGroup <- length(levels(as.factor(group)))

# Removing the items that violates local independence / item fit
# This section can be confusing. We updated the item names according to the results of the analayzes below.
# After each analysis we added new nms == "X...." to the list below.
mergedData <- mergedData[,-which(nms == "X4124105" | nms == "X4322102" | nms == "X421203" |
                                    nms == "X4113201" | nms == "X4141204" | nms == "X4151303" |
                                    nms == "X4143801" | nms == "X4111502" | nms == "X4146305" |
                                    nms == "X4331105" | nms == "X4156503" | nms == "X4152103" |
                                    nms == "X4332203" | nms == "X4162102" | nms == "X4312103" |
                                    nms == "X4341102" | nms == "X4214101" | nms == "X4351105" |
                                    nms == "X4365104" | nms == "X4146402" | nms == "X4164203" |
                                    nms == "X4113301" | nms == "X4131302")]

# Preparing the objects that will be used in the for loop.
itemParTmp <- list()
itemFit <-NULL
q3Table <- NULL
ti <- rep(0, 801)

# For loop for each group (booklet) in the data
for(i in 1:(nGroup-1)){
  g1 <- paste0("g",i)
  g2 <- paste0("g",i+1)

  # Filtering the data for the current groups.
  groupData <- mergedData[
    which(mergedData$testId == g1 | mergedData$testId == g2), ]

  # Removing the columns that have all NA values (e.g. items in the other booklets)
  groupData <- groupData[,colSums(is.na(groupData)) < nrow(groupData)]

  # Extracting the item names as the nmsTmp object.
  # We removed the first column which is the testId
  nmsTmp <- colnames(groupData[,-1])

  # Running mirt with multipleGroup function
  mg <- mirt::multipleGroup(groupData[,-1], # We removed the first column which is the testId
                            1,
                            groupData$testId,
                            itemtype = "Rasch",
                            invariance = c(nmsTmp, "free_means", "free_var"))

  # Running multipleGroup() function again to calculate the SE of item parameters
  mg2 <- mirt::multipleGroup(groupData[,-1],
                             1,
                             groupData$testId,
                             itemtype = "Rasch",
                             pars = mirt::mod2values(mg), # We used the previous model to calculate the SE
                             TOL = NaN,
                             SE = T,
                             invariance = c(nmsTmp, "free_var", "free_means"))
  # Extracting the item parameters
  itemParTmp[[i]] <- mirt::coef(mg2, printSE = T, IRTpars = T)

  # Estimating theta levels
  mirtTheta <- mirt::fscores(mg2, group = g1)

  # Calculating the item fit statistics
  itemFitTmp <- mirt::itemfit(mg2, fit_stats = "X2")
  itemFitTmp1 <- itemFitTmp[[1]][which(itemFitTmp[[1]]$df.X2 > 0), ]
  itemFitTmp2 <- itemFitTmp[[2]][which(itemFitTmp[[2]]$df.X2 > 0), ]
  itemFit <- rbind(itemFit, itemFitTmp1, itemFitTmp2)

  # Calculating the test information
  tiTmp <- mirt::testinfo(mg2, Theta = matrix(seq(-4,4,.01)), group = g1)
  ti <- ti + tiTmp

  # Calculating the Q3 statistics for the local independence
  q3 <- mirt::residuals(mg2, type = "Q3", suppress = 0.20)

  # Extracting the item pairs that violates the local independence
  q3Index <- which(q3$g1 > 0.20 & q3$g1 < 1, arr.ind = TRUE)
  if(length(q3Index > 0)){
    q3IndexValue <- q3$g1[which(q3$g1 > 0.20 & q3$g1 < 1)]
    q3RowIndex <- q3Index[, 1]
    q3ColIndex <- q3Index[, 2]
    q3RowName <- rownames(q3$g1)[q3RowIndex]
    q3ColName <- colnames(q3$g1)[q3ColIndex]
    q3TableTmp <- data.frame(itemNo1 = q3RowName,
                             itemNo2 = q3ColName,
                             q3Value = q3IndexValue)
    q3Table <- rbind(q3Table, q3TableTmp)
  }
}

# Combining the item parameters for each loop
itemPar <- matrix(unlist(itemParTmp[[1]][[1]][-nGroup]), byrow = F)

# Preparing the item parameters to save as a clean data frame
itemPar2DL <- NULL
for(i in 1:35){
  bse <- NULL
  for(j in 1:length(itemParTmp[[i]][[1]])){
    namez <- names(itemParTmp[[i]][[1]])
    btmp <- itemParTmp[[i]][[1]][[j]][1,2]
    setmp <- itemParTmp[[i]][[1]][[j]][2,2]
    bsetmp <- data.frame(item = namez[j], b = btmp, se = setmp)
    bse <- rbind(bse, bsetmp)
    print(paste0(i, " - ", j))
  }
  itemPar2DL <- rbind(itemPar2DL, bse)
}

itemPar2DL <- itemPar2DL[which(itemPar2DL$item != "GroupPars"),]
itemPar2DL$item <- as.factor(itemPar2DL$item)

itemPar2DL <- aggregate(itemPar2DL[, 2:3], list(itemPar2DL$item), mean)

# Calculating the p statistics according to the Classical Test Theory
p <- colMeans(mergedData[,-1], na.rm = T)
p <- dplyr::tibble(item = colnames(mergedData)[-1], p = p)

colnames(itemPar2DL)[1] <- "item"

# Merging the item parameters with the p statistics
itemPar2DL <- dplyr::left_join(itemPar2DL, p, by = "item")

# Calibration with the standard method
method2 <- mirt::mirt(mergedData[, -1], model = 1, itemtype = "Rasch", SE = T)

# Extracting the item parameters for the standard method
method2Coef <- mirt::coef(method2, IRTpars = T, printSE = T)

# Extracting the b parameters and SEs
method2Pars <- NULL
for(i in 1:(length(method2Coef)-1)){
  method2ParsTmp <- c(names(method2Coef)[i],
                      method2Coef[[i]][1,2],
                      method2Coef[[i]][2,2])
  method2Pars <- rbind(method2Pars, method2ParsTmp)
}

colnames(method2Pars) <- c("item", "b2", "se2")
rownames(method2Pars) <- NULL
method2Pars <- as.data.frame(method2Pars)
method2Pars$b2 <- as.numeric(method2Pars$b2)
method2Pars$se2 <- as.numeric(method2Pars$se2)

# Merging the item parameters with the standard method parameters
itemPar2DL2 <- dplyr::left_join(itemPar2DL, as.data.frame(method2Pars), by = "item")

# Calculating the information for each item for both methods
Pt <- function(theta, b){
  P <- 1 / (1 + exp(-(theta - b)))
  return(P)
}

PtM1<- NULL
PtM2 <- NULL
for(i in 1:nrow(itemPar2DL2)){
  PtTmpM1 <- Pt(seq(-4, 4, 0.001), itemPar2DL2$b[i])
  PtTmpM1 <- PtTmpM1 * (1- PtTmpM1)
  PtM1 <- cbind(PtM1, PtTmpM1)

  PtTmpM2 <- Pt(seq(-4, 4, 0.001), itemPar2DL2$b2[i])
  PtTmpM2 <- PtTmpM2 * (1- PtTmpM2)
  PtM2 <- cbind(PtM2, PtTmpM2)
}

PtDfSum <- data.frame(info1 = rowSums(PtM1),
                      info2 = rowSums(PtM2))

# Data visualization
# Test information function
library(ggplot2)
ggplot(PtDfSum) +
  aes() +
  geom_line(aes(x = seq(-4, 4, 0.001), y = info1, color = "Multiple Group"), lwd = 1, alpha = 0.3) +
  geom_line(aes(x = seq(-4, 4, 0.001), y = info2, color = "Standard"), lwd = 1, alpha = 0.3) +
  scale_color_manual(name = "Methods",
                    values = c("Multiple Group" = "#5894f5", "Standard" = "red")) +
  xlab("Theta") +
  ylab("Information") +
  theme(legend.position = c(0.85, 0.8),
        panel.background = NULL) +
  xlim(-5, 5)

# b parameter distribution
ggplot(itemPar2DL2) +
  geom_histogram(aes(x=b, fill = "Multiple Group"), alpha = 0.3) +
  geom_histogram(aes(x=b2, fill = "Standard"), alpha = 0.3) +
  scale_fill_manual(name = "Methods",
                     values = c("Multiple Group" = "#5894f5", "Standard" = "red")) +
  xlab("b parameters") +
  ylab("Frequency") +
  theme(legend.position = c(0.2, 0.8),
        panel.background = NULL) +
  xlim(-5, 5)

# b parameter vs. SE
ggplot(itemPar2DL2) +
  geom_point(aes(x = b, y = se, color = "Multiple Group"),  alpha = 0.3) +
  geom_point(aes(x = b2, y = se2, color = "Standard"), alpha = 0.3) +
  scale_color_manual(name = "Methods",
                     values = c("Multiple Group" = "#5894f5", "Standard" = "red")) +
  xlab("b parameters") +
  ylab("Standard Error") +
  theme(legend.position = c(0.8, 0.8),
        panel.background = NULL) +
  xlim(-5, 5)

# IRT b parameters vs. CTT p statistics
ggplot(itemPar2DL2) +
  geom_point(aes(x = b, y = p, color = "Multiple Group"), alpha = 0.3) +
  geom_point(aes(x = b2, y = p, color = "Standard"), alpha = 0.3) +
  scale_color_manual(name = "Methods",
                     values = c("Multiple Group" = "#5894f5", "Standard" = "red")) +
  xlab("b parameters") +
  ylab("p statistics") +
  ylim(0, 1) +
  theme(legend.position = c(0.8, 0.8),
        panel.background = NULL) +
  xlim(-5, 5)

# IRT b parameters for both methods
ggplot(itemPar2DL2) +
  geom_point(aes(x = b, y = b2), color = "#5894f5", alpha = 0.3) +
  xlab("b parameters") +
  ylab("b2 parameters") +
  theme(legend.position = c(0.8, 0.8),
        panel.background = NULL)

# Comparing b parameters with t - test
t.test(itemPar2DL2$b, itemPar2DL2$b2, var.equal = FALSE)
lsr::cohensD(itemPar2DL2$b, itemPar2DL2$b2)

# Comparing SEs with t - test
t.test(itemPar2DL2$se, itemPar2DL2$se2)
lsr::cohensD(itemPar2DL2$se, itemPar2DL2$se2)

# Correlation between IRT b parameters and the CTT p statistics for both methods
cor(itemPar2DL2$b,itemPar2DL2$p)
cor(itemPar2DL2$b2, itemPar2DL2$p)

# Significance test for the correlation coefficients
library(diffcor)
diffcor.two(cor(itemPar2DL2$b, itemPar2DL2$p),
            cor(itemPar2DL$p, itemPar2DL2$b2),
            n1 = nrow(itemPar2DL2),
            n2 = nrow(itemPar2DL2))
