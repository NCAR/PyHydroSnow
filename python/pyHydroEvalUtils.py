# Set of utility functions used by the main evaluation calling
# program. Mostly system/os/argument related for checking flow.

# Logan Karsten
# National Center for Atmospheric research
# Research Applications Laboratory

import os
import datetime
import pickle
import shutil
import fileinput
import sys
import string

def checkArgs(parser):
    # First check to ensure dates passed make sense
    if parser.begADate:
        if len(parser.begADate) != 10:
            print "ERROR: begADate not proper length."
            raise
        if not parser.endADate:
            print "ERROR: Associated endADate required."
            raise
    if parser.endADate:
        if len(parser.endADate) != 10:
            print "ERROR: endADate not proper length."
            raise
        if not parser.begADate:
            print "ERROR: Associated begADate required."
            raise
    if parser.snRead:
        if (int(parser.snRead) < 1) or (int(parser.snRead) > 4):
            print "ERROR: Invalid Snow Read Value."
            raise
    if parser.stRead:
        if (int(parser.stRead) < 1) or (int(parser.stRead) > 2):
            print "ERROR: Invalid Streamflow Read Value."
            raise
    if parser.snRun:
        if (int(parser.snRun) < 1) or (int(parser.snRun) > 12):
            print "ERROR: Invalid Snow Analysis Value."
            raise
    if parser.stRun:
        if (int(parser.stRun) < 1) or (int(parser.stRun) > 6):
            print "ERROR: Invalid Streamflow Analysis Value."
            raise
    if parser.subset:
        if (int(parser.subset) < 0) or (int(parser.subset) > 1):
            print "ERROR: Invalid Subset Flag."
            raise            
    if parser.pad:
        if (int(parser.pad) < 1):
            print "ERROR: Padding value must be greater than 0."
            raise
    if parser.snowNet:
        if (int(parser.snowNet) < 1):
            print "ERROR: Invalid snownet Flag."
            raise

def checkSNArgs(parser):
    # Check arguments for the snow database extraction program.
    if parser.begRDate:
        if len(parser.begRDate) != 10:
            print "ERROR: begRDate not proper length."
            raise
        if not parser.endRDate:
            print "ERROR: Associated endADate required."
            raise
    if parser.endRDate:
        if len(parser.endRDate) != 10:
            print "ERROR: endRDate not proper length."
            raise
        if not parser.begRDate:
            print "ERROR: Associated begADate required."
            raise
    if not parser.outDir or len(parser.outDir) == 0:
        print "ERROR: No output directory specified."
        raise
    if parser.outDir == '.' or parser.outDir == './':
        print 'ERROR: Please specify full path'
        raise
    if parser.netList and parser.stnList:
        print "ERROR: Please choose either station subsetting or network subsetting."
        raise
    if not parser.geoFile or len(parser.geoFile) == 0:
        print "ERROR; Please provide a geogrid file to the program."
        raise
    if parser.netList:
        if len(parser.netList) == 0:
            print "ERROR: Zero length network subsetting list passed to program."
            raise
    if parser.stnList:
        if len(parser.stnList) == 0:
            print "ERROR: Zero length station subsetting list passed to program."
            raise
    if parser.mskFile:
        if len(parser.mskFile) == 0:
            print "ERROR: Zero length mask file passed to program."
            raise
            
def returnDate(dIn):
	# Convert a date string in YYYYMMDDHH format to a datetime object
	dIn = str(dIn)
	dOut = datetime.datetime(year=int(dIn[0:4]),month=int(dIn[4:6]),day=int(dIn[6:8]),hour=int(dIn[8:10]))
	return dOut

def readDb(pathIn):
	# Read data in from pickle file
	with open(pathIn,'rb') as input:
		dbTmp = pickle.load(input)
	return dbTmp

def checkDb(args,dbIn):
	# Check to make sure model alias values are present in model project database
	numModIn = len(args.modelProjects)
	numModDb = len(dbIn.alias)

	for i in range(0,numModIn):
		check = 0 
		for j in range(0,numModDb):
			if dbIn.alias[j] == args.modelProjects[i]:
				check = 1
		if check != 1:
			print "ERROR: Model: " + args.modelProjects[i] + " not present in model project database."
			print "       Please add to database using addMod.py"
			raise

def initNamelist(args,dbIn,rank):
    # Establish namelist link based off current process id
    nLnk = "./parm/namelist_" + str(os.getpid()) + "_RANK" + str(rank) + ".R"
    
    # Establish index of 1st model project in the database.
    numModIn = len(args.modelProjects)
    numModDb = len(dbIn.alias)

    for i in range(0,numModDb):
        if dbIn.alias[i] == args.modelProjects[0]:
            indDbOrig = i

    strTmp = "_"
    # Establish parent directory path where original namelist file will live.
    nameListFileOrig = "namelist_" + datetime.datetime.now().strftime("%Y%m%d%H%M%S") + "_" + str(args.begADate) + ".R" + \
                       str(args.endADate) + "_" + strTmp.join(args.modelProjects) + "_" + \
                       str(args.snRead) + "_" + str(args.stRead) + "_" + str(args.snRun) + "_" + \
                       str(args.stRun) + "_subset_" + str(args.subset) + "_pad_" + str(args.pad) + \
                       "_RANK_" + str(rank) + ".R"
    nameListPathOrig = dbIn.topDir[indDbOrig] + "/" + dbIn.alias[indDbOrig] + "/namelists/" + nameListFileOrig

    # Copy template file over to directory
    shutil.copyfile('./parm/namelist_template.R',nameListPathOrig)

    # Create symbolic link in corresponding model project namelist directories
    for i in range(1, numModIn):
        for j in range(0, numModDb):
            if dbIn.alias[j] == args.modelProjects[i]:
                nameLnkPath = dbIn.topDir[j] + "/" + dbIn.alias[j] + "/namelists/" + nameListFileOrig
                os.symlink(nameListPathOrig,nameLnkPath)
    # Create symbolic link in current directory for when analysis is to be ran
    os.symlink(nameListPathOrig,nLnk)
    
    return nameListPathOrig, nLnk

def editLine(fileIn,searchExp,replaceExp):
	# Edit line in text file, replacing search string with passed string.
	check = 0 # Only replace once. Some lines have same string segments.
		  # Don't want to replicate process multiple times
	for line in fileinput.input(fileIn,inplace=1):
		if searchExp in line:
			if check == 0:
				line = line.replace(searchExp,replaceExp)
				check = check + 1 
			else:
			 	check = check + 1	
		sys.stdout.write(line)

def findInFile(bDate,eDate,str1,str2):
	# Find expected input file based on expected format
	# Walk the top level directory, parse datetime strings, and
	# check to see if passed dates fall within range. If they do,
	# pass file path back to calling routine to place into namelist
	# file. If no files found, raise exception.

	check = False
	fileOut = '' 
	tags = []
	splitTmp = string.split(str2,'.')
	splitTmp2 = string.split(splitTmp[0],'_')
	if len(splitTmp2) <= 1:
		print "ERROR: Unexpected model string: " + splitTmp[0] + " passed to findInFile."
		raise
	for i in range(0, len(splitTmp2)):
		tags.append(splitTmp2[i])
	for listing in os.walk(str1):
		files = listing[2]
		for file in files:
			split1 = string.split(file,'.')
			if len(split1) != 2:
				continue
			if split1[1] != 'Rdata':
				continue
			split2 = string.split(split1[0],"_")
			if len(split2) != (len(tags)+2):
				continue
			if split2[2:len(split2)] != tags:
				continue
			# Found expected type of file. Check dates.
			d1Str = split2[0]
			d2Str = split2[1]		
			if (len(d1Str) != 12) or (len(d2Str) != 12):
				print "ERROR: Unexpected date length found in: " + file
				raise
			dCheckStart = datetime.datetime(int(d1Str[0:4]),int(d1Str[4:6]),\
				      int(d1Str[6:8]),int(d1Str[8:10]),int(d1Str[10:12]))
			dCheckEnd = datetime.datetime(int(d2Str[0:4]),int(d2Str[4:6]),\
                                    int(d2Str[6:8]),int(d2Str[8:10]),int(d2Str[10:12]))
			if (bDate >= dCheckStart) and (eDate <= dCheckEnd):
				check = True
				fileOut = str1 + file
				break
			else:
				print "WARNING: Found file that contained date range outside of desired range"
				print "         File found: " + file
				print "         Start Date Found: " + dCheckStart.strftime("%Y%m%d%H%M")
				print "         End Date Found: " + dCheckEnd.strftime("%Y%m%d%H%M")
				print "         Start Date Desired: " + bDate.strftime("%Y%m%d%H%M")
				print "         End Date Desired: " + eDate.strftime("%Y%m%d%H%M")

		if check == True:
			break

	# If no files found, raise error
	if check == False:
		print "WARNING: Failed to find necessary input file."
		raise
	else:
		print "MSG: Found input file: " + fileOut
		return fileOut
