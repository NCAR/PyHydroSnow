# Module file for reading in snow observations from web database. 

# Logan Karsten
# National Center for Atmospheric Research
# Research Applications Laboratory

import MySQLdb
import pandas as pd
import numpy as np
from netCDF4 import Dataset
import os
import datetime
import subprocess

def extractObs(begRDateObj,endRDateObj,outDir,geoFile,networkFile='',stnFile='',maskFile=''):
    # Establish paths
    fileOut = outDir + "/" + "SNOW_DB_OBS_" + begRDateObj.strftime('%Y%m%d%H') + \
              "_" + endRDateObj.strftime('%Y%m%d%H') + '.nc'
              
    # Check to make sure geogrid file exists
    if not os.path.isfile(geoFile):
        print "ERROR: Unable to locate input geogrid file"
        raise

    # Establish DB information
    hostName = 'hydro-c1-web.rap.ucar.edu'
    userName = 'logan'
    psWd = 'DHood$1948'
    dbName = 'NWM_snow_obs'
    # Initialize connection with SQL
    try:
        dbSnow = MySQLdb.connect(hostName,userName,psWd,dbName)
    except:
        print "ERROR: Unable to connect to NWM Snow Database."
        raise
    print 'CONNECTED TO DB'

    # Establish datetime strings to be used in command syntax            
    bDateStr = begRDateObj.strftime('%Y-%m-%d %H') + ':00:00'
    eDateStr = endRDateObj.strftime('%Y-%m-%d %H') + ':00:00'
        
    # Pull all SWE observations from time period.
    cmd = "select * from NWM_SWE where date_obs>'" + bDateStr + "' and " + \
    "date_obs<'" + eDateStr + "'"
              
    # Create cursor object to execute SQL command
    conn = dbSnow.cursor()
        
    print 'ESTABLISHED CONNECTIVITY OBJECT'
    # Execute command to pull SWE observations
    try:
        conn.execute(cmd)
        resultSWE = conn.fetchall()
    except:
        print "ERROR: Unable to pull SWE observations for analysis period."
        raise
        
    print 'EXECUTED SQL COMMAND'
    # Proceed to pull snow depth observations
    cmd = "select * from NWM_SD where date_obs>'" + bDateStr + "' and " + \
    "date_obs<'" + eDateStr + "'"
        
    try:
        conn.execute(cmd)
        resultSD = conn.fetchall()
    except:
        print "ERROR: Unable to pull Snow Depth observations for analysis period."
        raise
            
    print 'EXTRACTED SNOW OBS FROM FETCH'
    # Proceed to pull metadata entries. This information will be used to 
    # extract networks, etc.
    cmd = "select * from NWM_snow_meta"
        
    try:
        conn.execute(cmd)
        resultMeta = conn.fetchall()
    except:
        print "ERROR: Unable to extract snow metadata table information"
        raise
            
    print 'EXTRACTED METADATA INFORMATION'
    # Close the SQL connection
    conn.close()
        
    # If no ovservations were pulled, raise error.
    if len(resultSWE) == 0 and len(resultSD) == 0:
        print "ERROR: No observations extracted from database."
        raise
            
    # Create output NetCDF file for R to read in during analysis for processing
    # into basins, etc.
    fileNC = snowObsNC(fileOut,resultSWE,resultSD,resultMeta,networkFile,stnFile,maskFile)
        
    # Process into R dataset. 
    if maskFile:
        print "MASK"
        cmd = "Rscript process_SNOW_OBS.R " + fileNC + " " + geoFile + \
        " " + maskFile
    else:
        print "NO MASK"
        cmd = "Rscript process_SNOW_OBS.R " + fileNC + " " + geoFile

    subprocess.call(cmd,shell=True)
    #try:            
    #    subprocess.call(cmd,shell=True)
    #except:
    #    print "ERROR: Unable to process snow observations into R dataset"
    #    raise
        
def snowObsNC(fileOut,resultSWE,resultSD,resultMeta,networkFile,stnFile,mskFile):
    # Function to output extracted snow observations to NetCDF file. This
    # file will be read in by R for processing.
    
    # First check to make sure output file doesn't already exist
    if os.path.isfile(fileOut):
        print "ERROR: Output Snow file: " + fileOut + " already exists"
        raise
        
    # Establish EPOCH datetime object
    epoch = datetime.datetime.utcfromtimestamp(0)
    
    # Establish data lengths
    siteLen = len(resultMeta)
    numSweObs = len(resultSWE)
    numSdObs = len(resultSD)

    # Initialize empty array to hold ID values
    uniquesOut = []
    idsOut = []
    networksOut = []
    latsOut = []
    lonsOut = []    
    # If network subsetting file exists, read it in.
    if networkFile:
        networks = pd.read_csv(networkFile)
        for i in range(0,siteLen):
            for j in range(0,len(networks.network)):
                metaSplit = [x.strip() for x in resultMeta[i][1].split(',')]
                if networks.network[j] in metaSplit:
                    uniquesOut.append(resultMeta[i][0])
                    networksOut.append(resultMeta[i][1])
                    idsOut.append(resultMeta[i][2])
                    latsOut.append(resultMeta[i][3])
                    lonsOut.append(resultMeta[i][4])
    elif stnFile:
        stationSub = pd.read_csv(stnFile)
        for i in range(siteLen):
            for j in range(0,len(stationSub.uniqueID)):
                if stationSub.uniqueID[j] in resultMeta[i][0]:
                    uniquesOut.append(resultMeta[i][0])
                    networksOut.append(resultMeta[i][1])
                    idsOut.append(resultMeta[i][2])
                    latsOut.append(resultMeta[i][3])
                    lonsOut.append(resultMeta[i][4])
    else:
        for i in range(0,siteLen):
            uniquesOut.append(resultMeta[i][0])
            networksOut.append(resultMeta[i][1])
            idsOut.append(resultMeta[i][2])
            latsOut.append(resultMeta[i][3])
            lonsOut.append(resultMeta[i][4])
    
    # Ensure no duplicate unique IDs are present
    #uniquesOut = list(set(uniquesOut))
    #networksOut = list(set(networksOut))
    #idsOut = list(set(idsOut))
    #latsOut = list(set(latsOut))
    #lonsOut = list(set(lonsOut))
    
    uniqueSWEOut = []
    uniqueSDOut = []
    sweOut = []
    sdOut = []
    # Datetime information will be stored as hours since EPOCH. That is
    # easier than trying to keep the datetime string.
    sweDateOut = []
    sdDateOut = []
    
    # Loop through and create output arrays
    # SWE first
    for i in range(0,numSweObs):
        if resultSWE[i][0] in uniquesOut:
            uniqueSWEOut.append(resultSWE[i][0])
            sweOut.append(resultSWE[i][1])
            sweDateOut.append(int((resultSWE[i][2]-epoch).total_seconds()/3600.0))
        
    # Snow Depth next
    for i in range(0,numSdObs):
        if resultSD[i][0] in uniquesOut:
            uniqueSDOut.append(resultSD[i][0])
            sdOut.append(resultSD[i][1])
            sdDateOut.append(int((resultSD[i][2]-epoch).total_seconds()/3600.0))
            
    # Create output NetCDF file.
    idOut = Dataset(fileOut,'w')
    
    # Dimensions
    metaDim1 = idOut.createDimension('numStations',len(uniquesOut))
    sweDim1 = idOut.createDimension('numSweObs',len(sweOut))
    sdDim1 = idOut.createDimension('numSdObs',len(sdOut))
    
    # Global attributes
    idOut.institution = 'National Center for Atmospheric Research'
    idOut.comment = 'Observations originally provided by the Office of Water Prediction'
    
    # Variables
    obsIds = idOut.createVariable('ptUniqueIds','i4',('numStations'),zlib=True,complevel=2)
    latVar = idOut.createVariable('ptLatitude','f4',('numStations'),zlib=True,complevel=2)
    lonVar = idOut.createVariable('ptLongitude','f4',('numStations'),zlib=True,complevel=2)
    sweObs = idOut.createVariable('sweObs','f4',('numSweObs'),zlib=True,complevel=2)
    sweObs.units = 'mm'
    sweObs.numObs = len(sweOut)
    sweObsIds = idOut.createVariable('sweObsIds','i4',('numSweObs'),zlib=True,complevel=2)    
    sweObsDates = idOut.createVariable('sweObsDates','i4',('numSweObs'),zlib=True,complevel=2)
    sweObsDates.units = 'Hours since 1970-01-01 00:00:00'    
    sdObs = idOut.createVariable('sdObs','f4',('numSdObs'),zlib=True,complevel=2)
    sdObs.units = 'mm'
    sdObs.numObs = len(sdOut)    
    sdObsIds = idOut.createVariable('sdObsIds','i4',('numSdObs'),zlib=True,complevel=2)    
    sdObsDates = idOut.createVariable('sdObsDates','i4',('numSdObs'),zlib=True,complevel=2)
    sdObsDates.units = 'Hours since 1970-01-01 00:00:00'
    
    # Place date into output file
    obsIds[:] = uniquesOut
    latVar[:] = latsOut
    lonVar[:] = lonsOut
    if len(sweOut) == 0:
        print 'WARNING: 0 SWE Observations Extracted From Database.'
    else:
        sweObs[:] = np.array(sweOut,dtype=np.float32)
        sweObsIds[:] = np.array(uniqueSWEOut, dtype=np.int32)
        sweObsDates[:] = np.array(sweDateOut, dtype=np.int32)
    if len(sdOut) == 0:
        print 'WARNING: 0 SD Observations Extracted From Database.'
    else:
        sdObs[:] = np.array(sdOut,dtype=np.float32)
        sdObsIds[:] = np.array(uniqueSDOut,dtype=np.int32)
        sdObsDates[:] = np.array(sdDateOut,dtype=np.int32)
    
    # Close output file
    idOut.close()
    
    # Return output NetCDF file to be processed into R dataset
    return fileOut
