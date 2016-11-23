# Program to check for expected input files necessary for plotting and
# various analysis metrics. If found, namelist will be edited to 
# reflect found files. If expected data not found, error will be 
# raised to parent program.

# Logan Karsten
# National Center For Atmospheric Research
# Research Applications Laboratory

#import fileinput
#import sys
#import os
#from pyHydroEvalUtils import editLine as el
#from pyHydroEvalUtils import returnDate as rt
#from pyHydroEvalUtils import findInFile as ff

# It should be noted all input for analysis/stats/plotting follows an
# expected format name of the following:
# topDir/modelProject/analysisDir/ + begDate_endDate + _INFOTAG.Rdata
# These three components are used to determine available datasets

#def modReadInCheckSnow(dbInd,bDate,eDate,nListPath,args,dbIn,strTmp):
#	# Find necessary input Rdata files for model reads
#	# necessary for plotting/statistics.
#
#	# Compose strings to be used in checking for files.
#        strCheck1 = dbIn.topDir[dbInd] + "/" + dbIn.alias[dbInd] + "/analysis_out/read_datasets/"
#
#        try:
#                fileIn = ff(bDate,eDate,strCheck1,strTmp)
#        except:
#                print "WARNING: No input found for : " + strTmp + " files."
#		raise
#
#	# Place into namelist file
#	searchStr = "modReadFilePathSnow <- NULL"
#	replaceStr = "modReadFilePathSnow <- '" + fileIn + "'"
#	el(nListPath,searchStr,replaceStr)

#def modReadInCheckStream(dbInd,bDate,eDate,nListPath,args,dbIn,strTmp):
#	# Find necessary input Rdata files for model reads
#	# necessary for plotting/statistics.
#
#	# Compose strings to be used in checking for files.
#        strCheck1 = dbIn.topDir[dbInd] + "/" + dbIn.alias[dbInd] + "/analysis_out/read_datasets/"
#
#        try:
#                fileIn = ff(bDate,eDate,strCheck1,strTmp)
#        except:
#                print "WARNING: No input found for : " + strTmp + " files."
#		raise
#
#	# Place into namelist file
#	searchStr = "modReadFilePathStream <- NULL"
#	replaceStr = "modReadFilePathStream <- '" + fileIn + "'"
#	el(nListPath,searchStr,replaceStr)

def openTmpFile(fileIn):
    # Generic routine to open temporary string to write R options to.
    fileObj = open(fileIn,'a')
    fileObj.close()
    
def writeStrToFile(fileIn,str):
     # Generic function to write output string to text file.
     fileObj = open(fileIn,'a')
     fileObj.write(str)
     fileObj.close()