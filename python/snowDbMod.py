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

def extractObs(args,db,size,rank,begADateObj,endADateObj):
    # Calculate database index where desired projects live.
    numModIn = len(args.modelProjects)
    numModDb = len(db.alias)
    for i in range(0,numModDb):
        if db.alias[i] == args.modelProjects[0]:
            indDbOrig = i
            
    # Pull snow network subsetting file out
    snowSubFile = db.snowNetSubFile[indDbOrig]
    
    # Only process if master processor
    if rank == 0:
        # Compose strings for output file path
        if args.snowNet == 1:
            snowNetStr = "NETSUB"
        else:
            snowNetStr = "NETALL"
            
        print snowNetStr
        if args.subset == 1:
            subStr = "BASSUB"
        else:
            subStr = "SUBALL"
        print subStr            
            
        # Establish paths
        fileOut = db.topDir[indDbOrig] + "/" + db.alias[indDbOrig] + "/analysis_out/" + \
                  "read_datasets/SNOW_DB_OBS_" + args.begADate + "_" + \
                  args.endADate + "_" + snowNetStr + "_" + subStr + ".nc"
                  
        print fileOut
        print db.snowDbHost[indDbOrig]
        print db.snowDbUser[indDbOrig]
        print db.snowDbPwd[indDbOrig]
        # Initialize connection with SQL
        try:
            dbSnow = MySQLdb.connect(db.snowDbHost[indDbOrig],db.snowDbUser[indDbOrig],\
                                     db.snowDbPwd[indDbOrig],'NWM_snow_obs')
        except:
            print "ERROR: Unable to connect to NWM Snow Database."
            raise
        print 'CONNECTED TO DB'

        # Establish datetime strings to be used in command syntax            
        bDateStr = begADateObj.strftime('%Y-%m-%d %H') + ':00:00'
        eDateStr = endADateObj.strftime('%Y-%m-%d %H') + ':00:00'
        
        # Pull all SWE observations from time period.
        cmd = "select * from NWM_SWE where date_obs>'" + bDateStr + "' and " + \
              "date_obs<'" + eDateStr + "'"
              
        print cmd
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
        
        print cmd
        try:
            conn.execute(cmd)
            resultMeta = conn.fetchall()
        except:
            print "ERROR: Unable to extract snow metadata table information"
            raise
            
        print 'EXTRACTED METADATA INFORMATION'
        # Close the SQL connection
        conn.close()
        
        # Create output NetCDF file for R to read in during analysis for processing
        # into basins, etc.
        snowObsNC(args,db,fileOut,resultSWE,resultSD,resultMeta,snowSubFile)
        
def snowObsNC(args,db,fileOut,resultSWE,resultSD,resultMeta,snowSubFile):
    # Function to output extracted snow observations to NetCDF file. This
    # file will be read in by R for processing.
    
    # First check to make sure output file doesn't already exist
    if os.path.isfile(fileOut):
        print "ERROR: Output Snow file: " + fileOut + " already exists"
        raise
        
    # Establish EPOCH datetime object
    epoch = datetime.datetime.utcfromtimestamp(0)
    
    print epoch
    # Establish data lengths
    siteLen = len(resultMeta)
    numSweObs = len(resultSWE)
    numSdObs = len(resultSD)

    print str(siteLen)
    print str(numSweObs)
    print str(numSdObs)
    
    # Initialize empty array to hold ID values
    uniquesOut = []
    idsOut = []
    networksOut = []
    latsOut = []
    lonsOut = []    
    # If network subsetting file exists, read it in.
    if len(snowSubFile) != 0:
        networks = pd.read_csv(snowSubFile)
        for i in range(0,siteLen):
            for j in range(0,len(networks.network)):
                metaSplit = [x.strip() for x in resultMeta[i][1].split(',')]
                if networks.network[j] in metaSplit:
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
    uniquesOut = list(set(uniquesOut))
    networksOut = list(set(networksOut))
    idsOut = list(set(idsOut))
    #latsOut = list(set(latsOut))
    #lonsOut = list(set(lonsOut))
    
    print str(len(uniquesOut))
    
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
        
    print str(len(sweOut))
    # Snow Depth next
    for i in range(0,numSdObs):
        if resultSD[i][0] in uniquesOut:
            uniqueSDOut.append(resultSD[i][0])
            sdOut.append(resultSD[i][1])
            sdDateOut.append(int((resultSD[i][2]-epoch).total_seconds()/3600.0))
            
    print str(len(sdOut))
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
    #latVar = idOut.createVariable('ptLatitude','f4',('numStations'),zlib=True,complevel=2)
    #lonVar = idOut.createVariable('ptLongitude','f4',('numStations'),zlib=True,complevel=2)
    sweObs = idOut.createVariable('sweObs','f4',('numSweObs'),zlib=True,complevel=2)
    sweObs.units = 'mm'
    sweObsIds = idOut.createVariable('sweObsIds','i4',('numSweObs'),zlib=True,complevel=2)    
    sweObsDates = idOut.createVariable('sweObsDates','i4',('numSweObs'),zlib=True,complevel=2)
    sweObsDates.units = 'Hours since 1970-01-01 00:00:00'    
    sdObs = idOut.createVariable('sdObs','f4',('numSdObs'),zlib=True,complevel=2)
    sdObs.units = 'mm'    
    sdObsIds = idOut.createVariable('sdObsIds','i4',('numSdObs'),zlib=True,complevel=2)    
    sdObsDates = idOut.createVariable('sdObsDates','i4',('numSdObs'),zlib=True,complevel=2)
    sdObsDates = 'Hours since 1870-01-01 00:00:00'
    
    # Place date into output file
    print str(len(uniquesOut))
    #print str(len(latsOut))
    #print str(len(lonsOut))
    print str(len(sweOut))
    print str(len(uniqueSWEOut))
    print str(len(sweDateOut))
    print str(len(sdOut))
    print str(len(uniqueSDOut))
    print str(len(sdDateOut))
    obsIds[:] = uniquesOut
    #latVar[:] = latsOut
    #lonVar[:] = lonsOut
    sweObs[:] = np.array(sweOut,dtype=np.float32)
    sweObsIds[:] = np.array(uniqueSWEOut, dtype=np.int32)
    sweObsDates[:] = np.array(sweDateOut, dtype=np.int32)
    sdObs[:] = np.array(sdOut,dtype=np.float32)
    sdObsIds[:] = np.array(uniqueSDOut,dtype=np.int32)
    sdObsDates[:] = np.array(sdDateOut,dtype=np.int32)
    
    # Close output file
    idOut.close()