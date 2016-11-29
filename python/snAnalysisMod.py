# Module file that contains several functions for reading/analyzing snow
# data for WRF-Hydro. 

# Logan Karsten
# National Center for Atmospheric Research
# Research Applications Laboratory

import ioMgmntMod
import subprocess
import os

def readSnow(args,dbIn,begDateObj,endDateObj,size,rank):
    # Top level module to read in either point analysis/model, aggregated
    # basin values, or both, depending on user inputs.
    
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
    for i in range(0, numModDb):
        for j in range(0, numModIn):
            if dbIn.alias[i] == args.modelProjects[j]:
                tagInds.append(i)
                tags.append(dbIn.tag[i])
                aliasTags.append(dbIn.alias[i])
                modPaths.append(dbIn.modelInDir[i])
                
    # Compose tag string that will be placed into namelist file
    tagStr = "c("
    for i in range(0, numModIn):
        if i == (numModIn - 1):
            tagStr = tagStr + "'" + tags[i] + "')"
        else:
            tagStr = tagStr + "'" + tags[i] + "', "
    tagStr  = "modTags <- " + tagStr + "\n"

    # Compose model/force path list string
    pathListStr = "c("
    for i in range(0, numModIn):
        if i == (numModIn - 1):
            pathListStr = pathListStr + "'" + modPaths[i] + "')"
        else:
            pathListStr = pathListStr + "'" + modPaths[i] + "', "
    pathListStr = "modPaths <- " + pathListStr + "\n"
            
    # Compose datetime strings
    begDateStr = "dateStart <- as.POSIXct('" + begDateObj.strftime('%Y-%m-%d %H') + \
                 ":00', format='%Y-%m-%d %H:%M', tz='UTC')\n" 
    endDateStr = "dateEnd <- as.POSIXct('" + endDateObj.strftime('%Y-%m-%d %H') + \
                 ":00', format='%Y-%m-%d %H:%M', tz='UTC')\n"
            
    # Establish constants
    jobDir = dbIn.topDir[indDbOrig] + "/" + dbIn.alias[indDbOrig] + "/" + args.jobName
    jobDirStr = "jobDir <- '" + jobDir + "'\n"
        
    # Establish path to SNODAS data
    if len(dbIn.snodasPath[indDbOrig]) != 0:
        snodasStr = "snodasPath <- '" + dbIn.snodasPath[indDbOrig] + "'\n"
        
    # Establish path to geogrid file
    geoStr = "geoFile <- '" + dbIn.geoFile[indDbOrig] + "'\n"
        
    # Compose strings conveying MPI size/rank information
    sizeStr = "size <- " + str(size) + "\n"
    rankStr = "rank <- " + str(rank) + "\n"
    
    # Create empty temporary text file. This will be used by R to read in options for
    # reading in data.
    tmpRFile = jobDir + "/R_NAMELIST_TMP.R"
    try:
        ioMgmntMod.openTmpFile(tmpRFile)
    except:
        print "ERROR: Unable to create temporary R file."
        raise
        
    # Place basic information into R file
    try:
        ioMgmntMod.writeStrToFile(tmpRFile,jobDirStr)
        ioMgmntMod.writeStrToFile(tmpRFile,tagStr)
        ioMgmntMod.writeStrToFile(tmpRFile,pathListStr)
        ioMgmntMod.writeStrToFile(tmpRFile,begDateStr)
        ioMgmntMod.writeStrToFile(tmpRFile,endDateStr)
        ioMgmntMod.writeStrToFile(tmpRFile,sizeStr)
        ioMgmntMod.writeStrToFile(tmpRFile,rankStr)
        ioMgmntMod.writeStrToFile(tmpRFile,geoStr)
    except:
        print("ERROR: Unable to write basic R information to temporary file.")
        raise
        
    # Situation #1 - Read in model snow fields at points given observations file.
    if args.snRead == "1":
        outFile = "outFile <- '" + jobDir + "/SN_PT_MOD_" + begDateObj.strftime('%Y%m%d%H') + \
                  "_" + endDateObj.strftime('%Y%m%d%H') + ".Rdata'\n"
        obsStr = "ptObsFile <- '" + args.inFile + "'\n"
        try:
            ioMgmntMod.writeStrToFile(tmpRFile,obsStr)
            ioMgmntMod.writeStrToFile(tmpRFile,outFile)
        except:
            print "ERROR: Unable to write to temporary R file."
            raise
            
        cmd = "Rscript ./R/SNOW_POINT_READ.R " + tmpRFile
        #try:
        #    subprocess.call(cmd,shell=True)
        #except:
        #    print "ERROR: Failure to execute snow reads"
        #    raise
        
    # Situation #2 - Read in model + SNODAS fields at points given observations file.
    if args.snRead == "2":
        outFile = "outFile <- '" + jobDir + "/SN_PT_MOD_SNODAS_" + begDateObj.strftime('%Y%m%d%H') + \
                  "_" + endDateObj.strftime('%Y%m%d%H') + ".Rdata'\n"
        obsStr = "ptObsFile <- '" + args.inFile + "'\n"
        if len(dbIn.snodasPath[indDbOrig]) == 0:
            print "ERROR: Path to SNODAS data necessary for reads."
            raise
        try:
            ioMgmntMod.writeStrToFile(tmpRFile,obsStr)
            ioMgmntMod.writeStrToFile(tmpRFile,snodasStr)
            ioMgmntMod.writeStrToFile(tmpRFile,outFile)
        except:
            print "ERROR: Unable to write to temporary R file."
            raise
            
        cmd = "Rscript ./R/SNOW_POINT_READ_SNODAS.R " + tmpRFile
        #try:
        #    subprocess.call(cmd,shell=True)
        #except:
        #    print "ERROR: Failure to execute snow reads"
        #    raise
        
    # Situation #3 - Read in model snow fields aggregated to basins plus point obs.
    if args.snRead == "3":
        outFile = "outFile <- '" + jobDir + "/SN_PTBAS_MOD_" + begDateObj.strftime('%Y%m%d%H') + \
                  "_" + endDateObj.strftime('%Y%m%d%H') + ".Rdata'\n"
        obsStr = "ptObsFile <- '" + args.inFile + "'\n"
        bsnMskStr = "bsnMskFile <- '" + args.bsnMskFile + "'\n"
        try:
            ioMgmntMod.writeStrToFile(tmpRFile,obsStr)
            ioMgmntMod.writeStrToFile(tmpRFile,bsnMskStr)
            ioMgmntMod.writeStrToFile(tmpRFile,outFile)
        except:
            print "ERROR: Unable to write to temporary R file."
            raise
            
        cmd = "Rscript ./R/SNOW_BASIN_POINT_READ.R " + tmpRFile
        #try:
        #    subprocess.call(cmd,shell=True)
        #except:
        #    print "ERROR: Failure to execute snow reads"
        #    raise
            
    # Situation #4 - Read in model + SNODAS fields aggregated to basins plus point obs.
    if args.snRead == "4":
        outFile = "outFile <- '" + jobDir + "/SN_PTBAS_MOD_SNODAS_" + begDateObj.strftime('%Y%m%d%H') + \
                  "_" + endDateObj.strftime('%Y%m%d%H') + ".Rdata'\n"
        obsStr = "ptObsFile <- '" + args.inFile + "'\n"
        if len(dbIn.snodasPath[indDbOrig]) == 0:
            print "ERROR: Path to SNODAS data necessary for reads."
            raise
        bsnMskStr = "bsnMskFile <- '" + args.bsnMskFile + "'\n"
        try:
            ioMgmntMod.writeStrToFile(tmpRFile,obsStr)
            ioMgmntMod.writeStrToFile(tmpRFile,bsnMskStr)
            ioMgmntMod.writeStrToFile(tmpRFile,snodasStr)
            ioMgmntMod.writeStrToFile(tmpRFile,outFile)
        except:
            print "ERROR: Unable to write to temporary R file."
            raise
            
        cmd = "Rscript ./R/SNOW_BASIN_POINT_READ_SNODAS.R " + tmpRFile
        #try:
        #    subprocess.call(cmd,shell=True)
        #except:
        #    print "ERROR: Failure to execute snow reads"
        #    raise
            
    # Situation #5 - Read in model snow fields aggregated to basins.
    if args.snRead == "5":
        outFile = "outFile <- '" + jobDir + "/SN_BAS_MOD_" + begDateObj.strftime('%Y%m%d%H') + \
                  "_" + endDateObj.strftime('%Y%m%d%H') + ".Rdata'\n"
        bsnMskStr = "bsnMskFile <- '" + args.bsnMskFile + "'\n"
        try:
            ioMgmntMod.writeStrToFile(tmpRFile,bsnMskStr)
            ioMgmntMod.writeStrToFile(tmpRFile,outFile)
        except:
            print "ERROR: Unable to write to temporary R file."
            raise
            
        cmd = "Rscript ./R/SNOW_BASIN_READ.R " + tmpRFile
        print cmd
        try:
            subprocess.call(cmd,shell=True)
        except:
            print "ERROR: Failure to execute snow reads"
            raise
        
    # Situation #6 - Read in mode + SNODAS fields aggregated to basins. 
    if args.snRead == "6":
        outFile = "outFile <- '" + jobDir + "/SN_BAS_MOD_SNODAS_" + begDateObj.strftime('%Y%m%d%H') + \
                  "_" + endDateObj.strftime('%Y%m%d%H') + ".Rdata'\n"
        if len(dbIn.snodasPath[indDbOrig]) == 0:
            print "ERROR: Path to SNODAS data necessary for reads."
            raise
        bsnMskStr = "bsnMskFile <- '" + args.bsnMskFile + "'\n"
        try:
            ioMgmntMod.writeStrToFile(tmpRFile,bsnMskStr)
            ioMgmntMod.writeStrToFile(tmpRFile,snodasStr)
            ioMgmntMod.writeStrToFile(tmpRFile,outFile)
        except:
            print "ERROR: Unable to write to temporary R file."
            raise
            
        cmd = "Rscript ./R/SNOW_BASIN_READ_SNODAS.R " + tmpRFile
        try:
            subprocess.call(cmd,shell=True)
        except:
            print "ERROR: Failure to execute snow reads"
            raise
            
    print tmpRFile
    # Remove temporary R namelist file
    #try:
    #    os.remove(tmpRFile)
    #except:
    #    print "ERROR: Failure to remove temporary R namelist file."
    #    raise