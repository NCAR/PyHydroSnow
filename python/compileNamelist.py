# Functions to edit the R namelist file for analysis/plotting.
# This will edit the file symbolicly linked in place.

# Logan Karsten
# National Center for Atmopsheric Research
# Research Applications Laboratory

import fileinput
import sys
import os
from pyHydroEvalUtils import editLine as el
from pyHydroEvalUtils import returnDate as rt
import ioMgmntMod

def editNamelist(pathIn,args,dbIn,rank):
    # Go through various options chosen by user and edit namelist file accordingly
    numModIn = len(args.modelProjects)
    numModDb = len(dbIn.alias)
    for i in range(0,numModDb):
        if dbIn.alias[i] == args.modelProjects[0]:
            indDbOrig = i

    # Calculate model tags associated with alias values chosen.
    tagInds = []
    tags = []
    aliasTags = []
    modPaths = []
    forcPaths = []
    for i in range(0, numModDb):
        for j in range(0, numModIn):
            if dbIn.alias[i] == args.modelProjects[j]:
                tagInds.append(i)
                tags.append(dbIn.tag[i])
                aliasTags.append(dbIn.alias[i])
                modPaths.append(dbIn.modelInDir[i])
                forcPaths.append(dbIn.forceInDir[i])

    # Compose tag string that will be placed into namelist file
    tagStr = "c("
    for i in range(0, numModIn):
        if i == (numModIn - 1):
            tagStr = tagStr + "'" + tags[i] + "')"
        else:
            tagStr = tagStr + "'" + tags[i] + "', " 	

    # Compose model/force path list string
    pathListStr = "c("
    forcPathListStr = "c("
    for i in range(0, numModIn):
        if i == (numModIn - 1):
            pathListStr = pathListStr + "'" + modPaths[i] + "')"
            forcPathListStr = forcPathListStr + "'" + forcPaths[i] + "')"
        else:
            pathListStr = pathListStr + "'" + modPaths[i] + "', "
            forcPathListStr = forcPathListStr + "'" + forcPaths[i] + "', "
            
    print tagInds
    print tags
    print aliasTags
    print modPaths
    print forcPaths
    # Edit ensemble information if ensembles present
    ensListStr = "c("
    ensTagStr = "c("
    numEns = len(dbIn.ensList[indDbOrig])
    if numModIn > 1 and numEns > 1:
        # Cannot do analysis between different groups of ensembles from different models at this time.
        print "ERROR: Cannot peform cross model analysis with ensembles at this time."
        raise
    ensList = dbIn.ensList[indDbOrig]
    ensTag = dbIn.ensTag[indDbOrig]
    if numEns > 0:
        searchStr = "readEnsemble <- FALSE"
        replaceStr = "readEnsemble <- TRUE"
        el(pathIn,searchStr,replaceStr)
        for i in range(0, numEns):
            if i == (numEns - 1):
                ensListStr = ensListStr + "'" + ensList[i] + "')"
                ensTagStr = ensTagStr + "'" +  ensTag[i] + "')"
            else:
                ensListStr = ensListStr + "'" + ensList[i] + "', "
                ensTagStr = ensTagStr + "'" + ensTag[i] + "', "
        searchStr = "ensembleList <- NULL"
        replaceStr = "ensembleList <- " + ensListStr
        el(pathIn,searchStr,replaceStr)
        searchStr = "ensembleTagList <- NULL"
        replaceStr = "ensembleTagList <- " + ensTagStr
        el(pathIn,searchStr,replaceStr)

    print ensListStr
    print ensTagStr
    print numEns
    # Edit high resolution routing domain file.
    searchStr = "hydFile <- NULL"
    replaceStr = "hydFile <- " + "'" + dbIn.fullDomFile[indDbOrig] + "'"
    el(pathIn,searchStr,replaceStr)
    print replaceStr

    # Edit the geo file entry.
    searchStr = "geoFile <- NULL"
    replaceStr = "geoFile <- " + "'" + dbIn.geoFile[indDbOrig] + "'"
    el(pathIn,searchStr,replaceStr)
    print replaceStr

    # Edit rout link file information if it exists
    if len(dbIn.routeLinkFile[indDbOrig]) != 0:
        searchStr = "routeLinkFile <- NULL"
        replaceStr = "routeLinkFile <- " + "'" + dbIn.routeLinkFile[indDbOrig] + "'"
        el(pathIn,searchStr,replaceStr)
        print replaceStr
        searchStr = "reachRting <- FALSE"
        replaceStr = "reachRting <- TRUE"
        el(pathIn,searchStr,replaceStr)
        print replaceStr

    # Edit resolution information
    searchStr = "resMod <- NULL"
    replaceStr = "resMod <- " + dbIn.geoRes[indDbOrig]
    el(pathIn,searchStr,replaceStr)
    print replaceStr

    # Edit the aggregation factor information
    searchStr = "aggfact <- NULL"
    replaceStr = "aggfact <- " + dbIn.agg[indDbOrig]
    el(pathIn,searchStr,replaceStr)
    print replaceStr

    # Edit mask file.
    if len(dbIn.mskFile[indDbOrig]) != 0:
        searchStr = "maskFile <- NULL"
        replaceStr = "maskFile <- " + "'" + dbIn.mskFile[indDbOrig] + "'" 
        el(pathIn,searchStr,replaceStr)
        print replaceStr

    # Edit basin subsetting option, if specified by user
    if args.subset:
        if len(dbIn.basinSubFile[indDbOrig]) == 0:
            print "ERROR: Basin Subset File Not Specified for Model Project."
            sys.exit(1)
        searchStr = "subSet <- NULL"
        replaceStr = "subSet <- read.table('" + dbIn.basinSubFile[indDbOrig] + \
        "', sep=\"\\t\", header=TRUE, colClasses=c(\"character\"))"
        el(pathIn,searchStr,replaceStr) 
        print replaceStr
        
    # Edit the padding information for plotting.
    if args.pad:
        searchStr = "padSteps <- 0"
        replaceStr = "padSteps <- " + args.pad
        el(pathIn,searchStr,replaceStr)
        print replaceStr
        
    # Edit tmp directory.
    searchStr = "tmpDir <- NULL"
    replaceStr = "tmpDir <- " + "'" + dbIn.topDir[indDbOrig] + \
    "/" + dbIn.alias[indDbOrig] + "/tmp" + "'"
    el(pathIn,searchStr,replaceStr)
    print replaceStr
    
    # Place model directories and tag listings into namelist file
    searchStr = "modPathList <- NULL"
    replaceStr = "modPathList <- " + pathListStr
    el(pathIn,searchStr,replaceStr)
    print replaceStr
    searchStr = "modTagList <- NULL"
    replaceStr = "modTagList <- " + tagStr
    el(pathIn,searchStr,replaceStr)
    print replaceStr

    # Edit snow database location entry
    if len(dbIn.snowDB[indDbOrig]) != 0:
        searchStr = "snowDB <- NULL"
        replaceStr = "snowDB <- '" + dbIn.snowDB[indDbOrig] + "'"
        print replaceStr
        
    # Edit the streamflow database location entry
    if len(dbIn.streamDB[indDbOrig]) != 0:
        searchStr = "streamDB <- NULL"
        replaceStr = "streamDB <- '" + dbIn.streamDB[indDbOrig] + "'"
        print replaceStr
        
    # Edit the plotting directory 
    searchStr = "plotDir <- NULL"
    replaceStr = "plotDir <- '" + dbIn.topDir[indDbOrig] + "/" + \
    dbIn.alias[indDbOrig] + "/analysis_out/plotting'"
    print replaceStr    
    
    # Establish analysis directory to store statistics/read files
    statDir = dbIn.topDir[indDbOrig] + "/" + dbIn.alias[indDbOrig] + \
    "/analysis_out/read_datasets/"
        
    # Edit all analysis/read dates in file
    str1 = "', format='%Y-%m-%d %H:%M', tz='UTC')"
    str2 = "', format='%Y-%m-%d', tz='UTC')"
    if args.begADate is not None:
        begADateObj = rt(args.begADate)
        endADateObj = rt(args.endADate)
        begAStr1 = begADateObj.strftime("%Y-%m-%d")
        begAStr2 = begADateObj.strftime("%Y-%m-%d %H:%M")
        endAStr1 = endADateObj.strftime("%Y-%m-%d")
        endAStr2 = endADateObj.strftime("%Y-%m-%d %H:%M")	

        searchStr = "readModStart <- NULL"
        replaceStr = "readModStart <- as.POSIXct('" + begAStr2 + str1
        el(pathIn,searchStr,replaceStr)
        print replaceStr

        searchStr = "readModEnd <- NULL"
        replaceStr = "readModEnd <- as.POSIXct('" + endAStr2 + str1
        el(pathIn,searchStr,replaceStr)
        print replaceStr

        searchStr = "readSnodasStart <- NULL"
        replaceStr = "readSnodasStart <- as.POSIXct('" + begAStr1 + str2
        el(pathIn,searchStr,replaceStr)
        print replaceStr

        searchStr = "readSnodasEnd <- NULL"
        replaceStr = "readSnodasEnd <- as.POSIXct('" + endAStr1 + str2 
        el(pathIn,searchStr,replaceStr)
        print replaceStr

        searchStr = "analysisStartDate <- NULL"
        replaceStr = "analysisStartDate <- as.POSIXct('" + begAStr2 + str1
        el(pathIn,searchStr,replaceStr)
        print replaceStr
        
        searchStr = "analysisEndDate <- NULL"
        replaceStr = "analysisEndDate <- as.POSIXct('" + endAStr2 + str1
        el(pathIn,searchStr,replaceStr)
        print replaceStr
        
    # Edit baseline namelist options corresponding to arguments passed in.
    strTmp = ''
    for i in range(0,numModIn):
        if i == (numModIn - 1):
            strTmp = strTmp + aliasTags[i]
        else:
            strTmp = strTmp + aliasTags[i] + "_"
    if args.begADate is not None:
        begADateObj = rt(args.begADate)
        endADateObj = rt(args.endADate)
        begAStr1 = begADateObj.strftime("%Y%m%d%H%M")
        endAStr1 = endADateObj.strftime("%Y%m%d%H%M")

    if args.snRead is not None:
        if int(args.snRead) == 1:
            searchStr = "readPointSnow <- FALSE"
            replaceStr = "readPointSnow <- TRUE"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
            modFileOut = statDir + begAStr1 + "_" + endAStr1 + "_" + \
            "_SNOW_POINT_MODEL.Rdata"
            
            searchStr = "modReadFilePathSnow <- NULL"
            replaceStr = "modReadFilePathSnow <- '" + modFileOut + "'"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
        elif int(args.snRead) == 2:
            searchStr = "readPointSnow <- FALSE"
            replaceStr = "readPointSnow <- TRUE"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
            searchStr = "readSnodas <- FALSE"
            replaceStr = "readSnodas <- TRUE"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
            searchStr = "snodasPathList <- NULL"
            replaceStr = "snodasPathList <- '" + dbIn.snodasPath[indDbOrig] + "'"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
            modFileOut = statDir + begAStr1 + "_" + endAStr1 + "_" + \
            "_SNOW_POINT_MODEL_SNODAS.Rdata"
            
            searchStr = "modReadFilePathSnow <- NULL"
            replaceStr = "modReadFilePathSnow <- '" + modFileOut + "'"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
        elif int(args.snRead) == 3:
            searchStr = "readPointSnow <- FALSE"
            replaceStr = "readPointSnow <- TRUE"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
            searchStr = "readBasinSnow <- FALSE"
            replaceStr = "readBasinSnow <- TRUE"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
            modFileOut = statDir + begAStr1 + "_" + endAStr1 + "_" + \
            "_SNOW_BASIN_MODEL.Rdata"

            searchStr = "modReadFilePathSnow <- NULL"
            replaceStr = "modReadFilePathSnow <- '" + modFileOut + "'"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
        elif int(args.snRead) == 4:
            searchStr = "readPointSnow <- FALSE"
            replaceStr = "readPointSnow <- TRUE"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
            searchStr = "readBasinSnow <- FALSE"
            replaceStr = "readBasinSnow <- TRUE"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
            searchStr = "readSnodas <- FALSE"
            replaceStr = "readSnodas <- TRUE"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
            searchStr = "snodasPathListSnow <- NULL"
            replaceStr = "snodasPathListSnow <- '" + dbIn.snodasPath[indDbOrig] + "'"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
            modFileOut = statDir + begAStr1 + "_" + endAStr1 + "_" + \
            "_SNOW_BASIN_MODEL_SNODAS.Rdata"

            searchStr = "modReadFilePathSnow <- NULL"
            replaceStr = "modReadFilePathSnow <- '" + modFileOut + "'"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
    if args.stRead is not None:
        if int(args.stRead) == 1:
            searchStr = "readStreamFrxst <- FALSE"
            replaceStr = "readStreamFrxst <- TRUE"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
            modFileOut = statDir + begAStr1 + "_" + endAStr1 + "_" + strTmp + "_FRXST.Rdata"
            searchStr = "modReadFilePathStream <- NULL"
            replaceStr = "modReadFilePathStream <- '" + modFileOut + "'"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
        elif int(args.stRead) == 2:
            searchStr = "readStreamChrtout <- FALSE"
            replaceStr = "readStreamChrtout <- TRUE"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
            modFileOut = statDir + begAStr1 + "_" + endAStr1 + "_" + strTmp + "_CHRTOUT.Rdata"
            searchStr = "modReadFilePathStream <- NULL"
            replaceStr = "modReadFilePathStream <- '" + modFileOut + "'"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
    if args.snRun is not None:
        if int(args.snRun) == 1:
            analysisOutFile = statDir + begAStr1 + "_" + endAStr1 + "_" + strTmp + \
            "_PT_SNOW_STAT.Rdata"
            searchStr = "analysisOutPath <- NULL"
            replaceStr = "analysisOutPath <- '" + analysisOutFile + "'"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
            searchStr = "pointSnowAnalysis <- FALSE"
            replaceStr = "pointSnowAnalysis <- TRUE"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
            for checkStr in ['"_SNOW_POINT_MODEL.Rdata"']:
                try:
                    ioMgmntMod.modReadInCheckSnow(indDbOrig,begADateObj,endADateObj,pathIn,args,dbIn,(strTmp + checkStr))
                    status = 1
                    break
                except:
                    continue
                if status == 0:
                    print "ERROR: Failure to find input model file for analysis."
                    sys.exit(1)
            
        elif int(args.snRun) == 2:
            analysisOutFile = statDir + begAStr1 + "_" + endAStr1 + "_" + strTmp + \
            "_PT_SNOW_STAT.Rdata"
            searchStr = "analysisOutPath <- NULL"
            replaceStr = "analysisOutPath <- '" + analysisOutFile + "'"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
            searchStr = "plotFlag <- FALSE"
            replaceStr = "plotFlag <- TRUE"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
            searchStr = "pointSnowAnalysis <- FALSE"
            replaceStr = "pointSnowAnalysis <- TRUE"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
            for checkStr in ['"_SNOW_POINT_MODEL.Rdata"']:
                try:
                    ioMgmntMod.modReadInCheckSnow(indDbOrig,begADateObj,endADateObj,pathIn,args,dbIn,(strTmp + checkStr))
                    status = 1
                    break
                except:
                    continue
                if status == 0:
                    print "ERROR: Failure to find input model file for analysis."
                    sys.exit(1)
            
        elif int(args.snRun) == 3:
            analysisOutFile = statDir + begAStr1 + "_" + endAStr1 + "_" + strTmp + \
            "_PT_SNOW_SNODAS_STAT.Rdata"
            searchStr = "analysisOutPath <- NULL"
            replaceStr = "analysisOutPath <- '" + analysisOutFile + "'"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
            searchStr = "pointSnowAnalysis <- FALSE"
            replaceStr = "pointSnowAnalysis <- TRUE"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
            for checkStr in ['"_SNOW_POINT_MODEL_SNODAS.Rdata"']:
                try:
                    ioMgmntMod.modReadInCheckSnow(indDbOrig,begADateObj,endADateObj,pathIn,args,dbIn,(strTmp + checkStr))
                    status = 1
                    break
                except:
                    continue
                if status == 0:
                    print "ERROR: Failure to find input model file for analysis."
                    sys.exit(1)
            
        elif int(args.snRun) == 4:
            analysisOutFile = statDir + begAStr1 + "_" + endAStr1 + "_" + strTmp + \
            "_PT_SNOW_SNODAS_STAT.Rdata"
            searchStr = "analysisOutPath <- NULL"
            replaceStr = "analysisOutPath <- '" + analysisOutFile + "'"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
            searchStr = "plotFlag <- FALSE"
            replaceStr = "plotFlag <- TRUE"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
            searchStr = "pointSnowAnalysis <- FALSE"
            replaceStr = "pointSnowAnalysis <- TRUE"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
            for checkStr in ['"_SNOW_POINT_MODEL_SNODAS.Rdata"']:
                try:
                    ioMgmntMod.modReadInCheckSnow(indDbOrig,begADateObj,endADateObj,pathIn,args,dbIn,(strTmp + checkStr))
                    status = 1
                    break
                except:
                    continue
                if status == 0:
                    print "ERROR: Failure to find input model file for analysis."
                    sys.exit(1)
            
        elif int(args.snRun) == 5:
            analysisOutFile = statDir + begAStr1 + "_" + endAStr1 + "_" + strTmp + \
            "_PT_SNOW_BAS_STAT.Rdata"
            searchStr = "analysisOutPath <- NULL"
            replaceStr = "analysisOutPath <- '" + analysisOutFile + "'"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
            searchStr = "pointSnowAnalysis <- FALSE"
            replaceStr = "pointSnowAnalysis <- TRUE"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
            searchStr = "regionSnowAnalysis <- FALSE"
            replaceStr = "regionSnowAnalysis <- TRUE"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
            for checkStr in ['"_SNOW_BASIN_MODEL.Rdata"']:
                try:
                    ioMgmntMod.modReadInCheckSnow(indDbOrig,begADateObj,endADateObj,pathIn,args,dbIn,(strTmp + checkStr))
                    status = 1
                    break
                except:
                    continue
                if status == 0:
                    print "ERROR: Failure to find input model file for analysis."
                    sys.exit(1)
            
        elif int(args.snRun) == 6:
            analysisOutFile = statDir + begAStr1 + "_" + endAStr1 + "_" + strTmp + \
            "_PT_SNOW_BAS_STAT.Rdata"
            searchStr = "analysisOutPath <- NULL"
            replaceStr = "analysisOutPath <- '" + analysisOutFile + "'"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
            searchStr = "plotFlag <- FALSE"
            replaceStr = "plotFlag <- TRUE"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
            searchStr = "pointSnowAnalysis <- FALSE"
            replaceStr = "pointSnowAnalysis <- TRUE"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
            searchStr = "regionSnowAnalysis <- FALSE"
            replaceStr = "regionSnowAnalysis <- TRUE"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
            for checkStr in ['"_SNOW_BASIN_MODEL.Rdata"']:
                try:
                    ioMgmntMod.modReadInCheckSnow(indDbOrig,begADateObj,endADateObj,pathIn,args,dbIn,(strTmp + checkStr))
                    status = 1
                    break
                except:
                    continue
                if status == 0:
                    print "ERROR: Failure to find input model file for analysis."
                    sys.exit(1)
            
        elif int(args.snRun) == 7:
            analysisOutFile = statDir + begAStr1 + "_" + endAStr1 + "_" + strTmp + \
            "_PT_SNOW_SNODAS_BAS_STAT.Rdata"
            searchStr = "analysisOutPath <- NULL"
            replaceStr = "analysisOutPath <- '" + analysisOutFile + "'"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
            searchStr = "pointSnowAnalysis <- FALSE"
            replaceStr = "pointSnowAnalysis <- TRUE"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
            searchStr = "regionSnowAnalysis <- FALSE"
            replaceStr = "regionSnowAnalysis <- TRUE"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
            for checkStr in ['"_SNOW_BASIN_MODEL_SNODAS.Rdata"']:
                try:
                    ioMgmntMod.modReadInCheckSnow(indDbOrig,begADateObj,endADateObj,pathIn,args,dbIn,(strTmp + checkStr))
                    status = 1
                    break
                except:
                    continue
                if status == 0:
                    print "ERROR: Failure to find input model file for analysis."
                    sys.exit(1)
        elif int(args.snRun) == 8:
            analysisOutFile = statDir + begAStr1 + "_" + endAStr1 + "_" + strTmp + \
            "_PT_SNOW_SNODAS_BAS_STAT.Rdata"
            searchStr = "analysisOutPath <- NULL"
            replaceStr = "analysisOutPath <- '" + analysisOutFile + "'"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
            searchStr = "plotFlag <- FALSE"
            replaceStr = "plotFlag <- TRUE"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
            searchStr = "pointSnowAnalysis <- FALSE"
            replaceStr = "pointSnowAnalysis <- TRUE"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
            searchStr = "regionSnowAnalysis <- FALSE"
            replaceStr = "regionSnowAnalysis <- TRUE"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
            for checkStr in ['"_SNOW_BASIN_MODEL_SNODAS.Rdata"']:
                try:
                    ioMgmntMod.modReadInCheckSnow(indDbOrig,begADateObj,endADateObj,pathIn,args,dbIn,(strTmp + checkStr))
                    status = 1
                    break
                except:
                    continue
                if status == 0:
                    print "ERROR: Failure to find input model file for analysis."
                    sys.exit(1)
        elif int(args.snRun) == 9:
            analysisOutFile = statDir + begAStr1 + "_" + endAStr1 + "_" + strTmp + \
            "_SNOW_SNODAS_BAS_STAT.Rdata"
            searchStr = "analysisOutPath <- NULL"
            replaceStr = "analysisOutPath <- '" + analysisOutFile + "'"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
            searchStr = "regionSnowAnalysis <- FALSE"
            replaceStr = "regionSnowAnalysis <- TRUE"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
            for checkStr in ['"_SNOW_BASIN_MODEL_SNODAS.Rdata"']:
                try:
                    ioMgmntMod.modReadInCheckSnow(indDbOrig,begADateObj,endADateObj,pathIn,args,dbIn,(strTmp + checkStr))
                    status = 1
                    break
                except:
                    continue
                if status == 0:
                    print "ERROR: Failure to find input model file for analysis."
                    sys.exit(1)
        elif int(args.snRun) == 10:
            analysisOutFile = statDir + begAStr1 + "_" + endAStr1 + "_" + strTmp + \
            "_SNOW_SNODAS_BAS_STAT.Rdata"
            searchStr = "analysisOutPath <- NULL"
            replaceStr = "analysisOutPath <- '" + analysisOutFile + "'"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
            searchStr = "plotFlag <- FALSE"
            replaceStr = "plotFlag <- TRUE"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
            searchStr = "regionSnowAnalysis <- FALSE"
            replaceStr = "regionSnowAnalysis <- TRUE"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
            for checkStr in ['"_SNOW_BASIN_MODEL_SNODAS.Rdata"']:
                try:
                    ioMgmntMod.modReadInCheckSnow(indDbOrig,begADateObj,endADateObj,pathIn,args,dbIn,(strTmp + checkStr))
                    status = 1
                    break
                except:
                    continue
                if status == 0:
                    print "ERROR: Failure to find input model file for analysis."
                    sys.exit(1)
        elif int(args.snRun) == 11:
            analysisOutFile = statDir + begAStr1 + "_" + endAStr1 + "_" + strTmp + \
            "_SNOW_SNODAS_BAS_STREAM_STAT.Rdata"
            searchStr = "analysisOutPath <- NULL"
            replaceStr = "analysisOutPath <- '" + analysisOutFile + "'"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
            searchStr = "regionSnowAnalysis <- FALSE"
            replaceStr = "regionSnowAnalysis <- TRUE"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
            for checkStr in ['"_SNOW_BASIN_MODEL_SNODAS.Rdata"']:
                try:
                    ioMgmntMod.modReadInCheckSnow(indDbOrig,begADateObj,endADateObj,pathIn,args,dbIn,(strTmp + checkStr))
                    status = 1
                    break
                except:
                    continue
                if status == 0:
                    print "ERROR: Failure to find input model file for analysis."
                    sys.exit(1)
            for checkStr in ['_FRXST.Rdata','_CHRTOUT.Rdata']:
                try:
                    ioMgmntMod.modReadInCheckStream(indDbOrig,begADateObj,endADateObj,pathIn,args,dbIn,(strTmp + checkStr))
                    status = 1
                    break
                except:
                    continue
                if status == 0:
                    print "ERROR: Failure to find input model file for analysis."
                    sys.exit(1)
        elif int(args.snRun) == 12:
            analysisOutFile = statDir + begAStr1 + "_" + endAStr1 + "_" + strTmp + \
            "_SNOW_SNODAS_BAS_STREAM_STAT.Rdata"
            searchStr = "analysisOutPath <- NULL"
            replaceStr = "analysisOutPath <- '" + analysisOutFile + "'"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
            searchStr = "plotFlag <- FALSE"
            replaceStr = "plotFlag <- TRUE"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
            searchStr = "regionSnowAnalysis <- FALSE"
            replaceStr = "regionSnowAnalysis <- TRUE"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
            for checkStr in ['"_SNOW_BASIN_MODEL_SNODAS.Rdata"']:
                try:
                    ioMgmntMod.modReadInCheckSnow(indDbOrig,begADateObj,endADateObj,pathIn,args,dbIn,(strTmp + checkStr))
                    status = 1
                    break
                except:
                    continue
                if status == 0:
                    print "ERROR: Failure to find input model file for analysis."
                    sys.exit(1)
            for checkStr in ['_FRXST.Rdata','_CHRTOUT.Rdata']:
                try:
                    ioMgmntMod.modReadInCheckStream(indDbOrig,begADateObj,endADateObj,pathIn,args,dbIn,(strTmp + checkStr))
                    status = 1
                    break
                except:
                    continue
                if status == 0:
                    print "ERROR: Failure to find input model file for analysis."
                    sys.exit(1)
                    
    if args.stRun is not None:
        if int(args.stRun) == 1:
            analysisOutFile = statDir + begAStr1 + "_" + endAStr1 + "_" + strTmp + \
            "_STREAM_PT_STAT.Rdata"
            searchStr = "analysisOutPath <- NULL"
            replaceStr = "analysisOutPath <- '" + analysisOutFile + "'"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
        elif int(args.stRun) == 2:
            analysisOutFile = statDir + begAStr1 + "_" + endAStr1 + "_" + strTmp + \
            "_STREAM_ENS_STAT.Rdata"
            searchStr = "analysisOutPath <- NULL"
            replaceStr = "analysisOutPath <- '" + analysisOutFile + "'"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
        elif int(args.stRun) == 3:
            analysisOutFile = statDir + begAStr1 + "_" + endAStr1 + "_" + strTmp + \
            "_STREAM_PT_BC_STAT.Rdata"
            searchStr = "analysisOutPath <- NULL"
            replaceStr = "analysisOutPath <- '" + analysisOutFile + "'"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
        elif int(args.stRun) == 4:
            analysisOutFile = statDir + begAStr1 + "_" + endAStr1 + "_" + strTmp + \
            "_STREAM_ENS_BC_STAT.Rdata"
            searchStr = "analysisOutPath <- NULL"
            replaceStr = "analysisOutPath <- '" + analysisOutFile + "'"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
        elif int(args.stRun) == 5:
            analysisOutFile = statDir + begAStr1 + "_" + endAStr1 + "_" + strTmp + \
            "_STREAM_PT_BASEBC_STAT.Rdata"
            searchStr = "analysisOutPath <- NULL"
            replaceStr = "analysisOutPath <- '" + analysisOutFile + "'"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
        elif int(args.stRun) == 6:
            analysisOutFile = statDir + begAStr1 + "_" + endAStr1 + "_" + strTmp + \
            "_STREAM_ENS_BASEBC_STAT.Rdata"
            searchStr = "analysisOutPath <- NULL"
            replaceStr = "analysisOutPath <- '" + analysisOutFile + "'"
            el(pathIn,searchStr,replaceStr)
            print replaceStr
            
