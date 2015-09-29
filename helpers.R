# Data Specification ---------------------------------------

# Only defined for relative volumes but "relative" is replaced with "absolute" when actually loading the data, but only for the combined_vols files, not the gf files.
datadefs <- rbind(c(name="FMR1 (-/Y) (B6)", 
                    gf="gf_FMR1", 
                    data="combined_vols_FMR1",
                    term="Genotype", 
                    G1="WT", 
                    G2="KO", 
                    group="FXS"),
                  c(name="FMR1 (-/y) (FVB)", 
                    gf="gf_FMR1_FVB", 
                    data="combined_vols_FMR1_FVB",
                    term="Genotype", 
                    G1="WT", 
                    G2="KO", 
                    group="FXS"),
                  c(name="Mecp2 (-/y)", 
                    gf="gf_Mecp2_p60", 
                    data="combined_vols_Mecp2_p60",
                    term="Genotype", 
                    G1="WT", 
                    G2="KO", 
                    group="Rett"),
                  c(name="NL3 R451C", 
                    gf="gf_NL3", 
                    data="combined_vols_NL3",
                    term="Genotype", 
                    G1="WT", 
                    G2="KO", 
                    group="NLGN"),
                  c(name="En2 (-/-)", 
                    gf="gf_En2", 
                    data="combined_vols_En2",
                    term="Genotype", 
                    G1="WT", 
                    G2="KO", 
                    group="En2"),
                  c(name="16p11.2 (dp/+)", 
                    gf="gf_16p11", 
                    data="combined_vols_16p11",
                    term="Genotype", 
                    G1="WT", 
                    G2="dp", 
                    group="16p11"),
                  c(name="16p11.2 (df/dp)", 
                    gf="gf_16p11", 
                    data="combined_vols_16p11",
                    term="Genotype", 
                    G1="WT", 
                    G2="dfdp", 
                    group="16p11"),
                  c(name="16p11.2 (df/+)", 
                    gf="gf_16p11", 
                    data="combined_vols_16p11",
                    term="Genotype", 
                    G1="WT", 
                    G2="df", 
                    group="16p11"),
                  c(name="15q11-13 (patDp/+)", 
                    gf="gf_15q", 
                    data="combined_vols_15q",
                    term="Genotype", 
                    G1="WT", 
                    G2="DEL", 
                    group="15q"),
                  c(name="CNTNAP2 (-/-)", 
                    gf="gf_CNTNAP2", 
                    data="combined_vols_CNTNAP2",
                    term="Genotype", 
                    G1="WT", 
                    G2="KO", 
                    group="22q11"),
                  c(name="BTBR (B6)", 
                    gf="gf_BTBR", 
                    data="combined_vols_BTBR",
                    term="Genotype", 
                    G1="B6", 
                    G2="BTBR", 
                    group="BTBR"),
                  c(name="BTBR (FVB)", 
                    gf="gf_BTBR", 
                    data="combined_vols_BTBR",
                    term="Genotype", 
                    G1="FVB", 
                    G2="BTBR", 
                    group="BTBR"),
                  c(name="NRXN1a (-/-)", 
                    gf="gf_NRXN1a", 
                    data="combined_vols_NRXN1a",
                    term="Genotype", 
                    G1="WT", 
                    G2="KO", 
                    group="NRXN"),
                  c(name="NRXN1a (-/+)", 
                    gf="gf_NRXN1a", 
                    data="combined_vols_NRXN1a",
                    term="Genotype", 
                    G1="WT", 
                    G2="Het", 
                    group="NRXN"),
  #                   c(name="SLC6A4 Ala56 (B6)", 
  #                     gf="gf_SERT_KI_Male", 
  #                     data="combined_vols_SERT_KI_Male",
  #                     term="Genotype", 
  #                     G1="WT", 
  #                     G2="KI", 
  #                     group="SERT"),
                  c(name="SLC6A4 Ala56 (B6)", 
                    gf="gf_SERT_KI", 
                    data="combined_vols_SERT_KI",
                    term="Genotype", 
                    G1="WT", 
                    G2="KI", 
                    group="SERT"),
  #                   c(name="SLC6A4 Ala56 (129)", 
  #                     gf="gf_SERT_KI_129_Male", 
  #                     data="combined_vols_SERT_KI_129_Male",
  #                     term="Genotype", 
  #                     G1="WT", 
  #                     G2="KI", 
  #                     group="SERT"),
                  c(name="SLC6A4 Ala56 (129)", 
                    gf="gf_SERT_KI_129", 
                    data="combined_vols_SERT_KI_129",
                    term="Genotype", 
                    G1="WT", 
                    G2="KI", 
                    group="SERT"),
  #                   c(name="SLC6A4 KO (-/-)", 
  #                     gf="gf_SERT_KO_Male", 
  #                     data="combined_vols_SERT_KO_Male",
  #                     term="Genotype", 
  #                     G1="WT", 
  #                     G2="KO", 
  #                     group="SERT"),
                  c(name="SLC6A4 KO (-/-)", 
                    gf="gf_SERT_KO", 
                    data="combined_vols_SERT_KO",
                    term="Genotype", 
                    G1="WT", 
                    G2="KO", 
                    group="SERT"),
                  c(name="SHANK3 (-/-)", 
                    gf="gf_SHANK3", 
                    data="combined_vols_SHANK3",
                    term="Genotype", 
                    G1="WT", 
                    G2="KO", 
                    group="SHANK3"),
                  c(name="SHANK3 (-/+)", 
                    gf="gf_SHANK3", 
                    data="combined_vols_SHANK3",
                    term="Genotype", 
                    G1="WT", 
                    G2="Het", 
                    group="SHANK3"),
                  c(name="AndR (12Q)", 
                    gf="gf_AndR", 
                    data="combined_vols_AndR",
                    term="Genotype", 
                    G1="WT", 
                    G2="12Q", 
                    group="AndR"),
                  c(name="AndR (48Q)", 
                    gf="gf_AndR", 
                    data="combined_vols_AndR",
                    term="Genotype", 
                    G1="WT", 
                    G2="48Q", 
                    group="AndR"),
                  c(name="BALB/CJ (B6)", 
                    gf="gf_BALBC", 
                    data="combined_vols_BALBC",
                    term="Genotype", 
                    G1="WT", 
                    G2="BALBC", 
                    group="BALBC"),
                  c(name="XO", 
                    gf="gf_Turner", 
                    data="combined_vols_Turner",
                    term="Genotype", 
                    G1="XX", 
                    G2="XO", 
                    group="Turner"),
                  c(name="GTF2i (dp/dp)", 
                    gf="gf_Gtf2i", 
                    data="combined_vols_Gtf2i",
                    term="Genotype", 
                    G1="WT", 
                    G2="XS", 
                    group="Gtf2i"),
                  c(name="GTF2i (+/-)", 
                    gf="gf_Gtf2i", 
                    data="combined_vols_Gtf2i",
                    term="Genotype", 
                    G1="WT", 
                    G2="YT", 
                    group="Gtf2i"),
                  c(name="ITGB3 (-/-)", 
                    gf="gf_ITGB3", 
                    data="combined_vols_ITGB3",
                    term="Genotype", 
                    G1="WT", 
                    G2="KO", 
                    group="ITGB3"))

datadefs <- as.data.frame(datadefs, stringsAsFactors=FALSE)


# Loading Data ------------------------------------------------------------

GfMetadata <- function(gfFiles) {
  # Returns a named vector containing metadata fields that are used in AT LEAST ONE of the gf files in the app.
  
  gfMetadata = c("Treatment"=FALSE, "Sex"=FALSE, "Background"=FALSE, "FactorAge"=FALSE, "RawAge"=FALSE)
  for (file in gfFiles) {
    gfFile = paste(getwd(), "/data/", file, ".txt", sep="")
    tempDf = read.table(gfFile)
    tempDfCols = colnames(tempDf)
    for (item in names(gfMetadata)) {
      if (item %in% tempDfCols) {
        gfMetadata[[item]] = TRUE
      }
    }
  }
  
  return(gfMetadata)
}


LoadData <- function() {
  # Loads the gf files and both the relative and absolute combined vols data files if they exist.
  
  dataFiles = datadefs$data
  gfFiles = datadefs$gf

  # load gf files
  for (file in gfFiles) {
    gfFile = paste(getwd(), "/data/", file, ".txt", sep="")
    assign(x=file, value=read.table(file=gfFile, header=TRUE, sep=" ", check.names=FALSE), envir=parent.frame())
  }
    
  # load relative volume files
  for (file in dataFiles) {
    relativeFile = paste(getwd(), "/data/", file, "_relative.txt", sep="")
    if (file.exists(relativeFile)) {
      assign(x=paste(file, "_relative", sep=""), value=read.table(file=relativeFile, header=TRUE, sep=" ", check.names=FALSE), envir=parent.frame())
    }
  }
  
  # load absolute volume files
  for (file in dataFiles) {
    absoluteFile = paste(getwd(), "/data/", file, "_absolute.txt", sep="")
    if (file.exists(absoluteFile)) {
      assign(x=paste(file, "_absolute", sep=""), value=read.table(file=absoluteFile, header=TRUE, sep=" ", check.names=FALSE), envir=parent.frame())
    }
  }
}


SimpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
      sep="", collapse=" ")
}


IndividualData <- function(datadefs, volumeType) {
  # Summary:
  #   Creates a data table containing brain region volume data for each individual mouse.
  # Args:
  #   datadefs: A data frame containing information about each strain.  Defined at the top of helpers.R.
  #   volumeType: Either 'relative' or 'absolute'.
  # Returns:
  #   A data table containing volume data for each individual mouse for each brain region.

  # <app dir>/data/ is the base path for data
  basePath = paste(getwd(), '/data/', sep='')
  
  # Get example combined vols file from first row of the datadefs data frame.
  absoluteFile = paste(basePath, datadefs$data[1], '_absolute.txt', sep='')
  relativeFile = paste(basePath, datadefs$data[1], '_relative.txt', sep='')
  if (file.exists(absoluteFile)) {
    combinedVolsDf = read.table(absoluteFile, check.names=FALSE)
  } else if (file.exists(relativeFile)) {
    combinedVolsDf = read.table(relativeFile, check.names=FALSE)
  } else {
    # This isn't the best way of handling the error, but at least it gives the user some clue as to why the app failed.
    stop(paste(absoluteFile, 'and', relativeFile, 'do not exist.  Check that datadefs is specified properly in helpers.R.'))
  }
  
  # Find metadata columns that were specified in the gf files.
  gfMetadata = GfMetadata(datadefs$gf)
  gfMetadataTrue = which(gfMetadata)
  numMetadataFactors = length(gfMetadataTrue)
  
  dirtyColNames = colnames(combinedVolsDf)
  formattedColNames = sapply(dirtyColNames, SimpleCap)
  numBrainRegions = length(formattedColNames)

  # Create individualData data table to store data for each mouse on each brain region.
  individualData = data.table(matrix(ncol = 2 + numMetadataFactors + numBrainRegions))
  individualData = individualData[-1, ]  # remove NAs from initialization
  colLabels = c(formattedColNames, "Strain", "Genotype", names(gfMetadataTrue))
  setnames(individualData, colLabels)

  # Loop through the gf files that need to be processed and extract the data.
  for (i in 1:nrow(datadefs)) {

    # Get data for a single strain as a data.table.
    row = datadefs[i, ]
    filename = paste(row$data, '_', volumeType, sep='')

    if (file.exists(paste(basePath, filename, '.txt', sep=''))) {

      tempData = get(filename)
      tempData = as.data.table(tempData)
      gfFile = get(row$gf)
       
      # Check that the combined vols column names are the same as those used previously.
      if (!all(colnames(tempData)==dirtyColNames)) {
        stop('All your combined vols files must have identical column names and be in the same order.')
      }

      # Set proper column names.  Look up names from a dictionary (named vector).
      setnames(tempData, formattedColNames)
      tempData[, Strain:=row$name]  # Fast version of tempData$Strain = row$name OR tempData[, "Strain"] = row$name
      tempData[, Genotype:=gfFile$Genotype]

      # Get optional column names (Treatment, RawAge, FactorAge, Sex, Background).
      # If gfFile does not contain that column, fill it with NA values.
      tryCatch({  # Computationally expensive
        for (i in 1:length(gfMetadataTrue)) {
          metadataColumn = names(gfMetadataTrue)[i]
          if (metadataColumn %in% colnames(gfFile)) {
            tempData[, metadataColumn] = get(metadataColumn, gfFile)
          } else {
            tempData[, metadataColumn] = NA
          }
        }
      }, error = function(err) {
        
      })

      # Append data to total dataset.
      individualData = rbindlist(list(individualData, tempData), use.names=TRUE, fill=TRUE)
    }
  }
  
  # Make long format and set appropriate column names.
  individualData = reshape2:::melt(individualData, id=c('Strain', 'Genotype', names(gfMetadataTrue)))
  setnames(individualData, c('Strain', 'Genotype', names(gfMetadataTrue), 'Region', 'Volume'))
  individualData$Genotype = as.factor(individualData$Genotype)
  individualData$Region = as.factor(individualData$Region)
  individualData$Strain = as.factor(individualData$Strain)
  
  # individualData = na.omit(individualData)

  return(individualData)
}


StatsSummaryTable <- function(df1, df2) {
  # Summary:
  #   Takes two data frames, each representing a group of mice.  
  #   Computes descriptive stats (Mean, SD) for each group along with statistical tests comparing the groups (t-test, Cohen's d, etc.).

  means1 = with(df1, tapply(Volume, Region, mean))
  sds1 = with(df1, tapply(Volume, Region, sd))
  means2 = with(df2, tapply(Volume, Region, mean))
  sds2 = with(df2, tapply(Volume, Region, sd))
  d = abs(means1 - means2)/sds1
  pcent = ((means2 - means1)/means1)*100

  # Round for display in interactive table.
  r_means1 = round(means1, 2)
  r_sds1 = round(sds1, 2)
  r_means2 = round(means2, 2)
  r_sds2 = round(sds2, 2)
  r_d = round(d, 2)
  r_pcent = round(pcent, 2)

  pvals = CalculateStats(df1, df2)

  summaryTable = data.frame("Region"=rownames(r_means1), "G1Mean"=r_means1, "G1Stdev"=r_sds1, "G2Mean"=r_means2, "G2Stdev"=r_sds2, "PercentDiff"=r_pcent, "Effect"=r_d)
  summaryTable = merge(summaryTable, pvals, by='Region')
  summaryTable$P_value = round(summaryTable$P_value, 3)
  summaryTable$FDR_P_value = round(summaryTable$FDR_P_value, 3)
  rownames(summaryTable) = NULL

  return(summaryTable)
}


CalculateStats <- function(df1, df2) {
  data = rbind.fill(df1, df2)
  pvals = ddply(data, .(Region), ttesthelper)
  setnames(pvals, c('Region', 'P_value'))
  pvals$FDR_P_value = p.adjust(p=pvals$P_value, method='fdr')
  setnames(pvals, c('Region', 'P_value', 'FDR_P_value'))

  pvals
}


ttesthelper <- function(x) {
  t.test(x[,'Volume'] ~ x[,'Group'])$p.value
}


# Quality Assurance -------------------------------------------------------

CheckCombinedVolsFiles <- function() {
  # Returns TRUE if all combined vols files in the app contain the same column names; otherwise, FALSE.
  
  dataFiles = datadefs$data
  gfFiles = datadefs$gf
  
  # get region names
  volumeFile = paste(getwd(), "/data/", dataFiles[1], "_relative.txt", sep="")
  if (file.exists(volumeFile)) {
    volumeDf = read.table(volumeFile)
    regionNames = colnames(volumeDf)
  } else {
    volumeFile = paste(getwd(), "/data/", dataFiles[1], "_absolute.txt", sep="")
    volumeDf = read.table(volumeFile)
    regionNames = colnames(volumeDf)
  }
  
  # load relative volume files
  for (file in dataFiles) {
    relativeFile = paste(getwd(), "/data/", file, "_relative.txt", sep="")
    if (file.exists(relativeFile)) {
      volumeDf = read.table(relativeFile)
      otherRegionNames = colnames(volumeDf)
      if (!all(otherRegionNames == regionNames)) {
        return(FALSE)
      }
    }
  }
  
  # load absolute volume files
  for (file in dataFiles) {
    absoluteFile = paste(getwd(), "/data/", file, "_absolute.txt", sep="")
    if (file.exists(absoluteFile)) {
      volumeDf = read.table(absoluteFile)
      otherRegionNames = colnames(volumeDf)
      if (!all(otherRegionNames == regionNames)) {
        return(FALSE)
      }
    }
  }
  
  return(TRUE)
}