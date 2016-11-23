# Parent program to perform snow and snow-related hydrologic analysis 
# on WRF-Hydro and NWM output. Program is heavily dependent on a sound
# observation database of snow and streamflow observations, along with
# prepared SNODAS and satellite fields.

# Logan Karsten
# National Center for Amtmospheric Research
# Research Applications Laboratory

import sys
sys.path.insert(0, './python')

#import subprocess
import argparse
import os
import pyHydroEvalUtils
#import snowDbMod
#import datetime
#import compileNamelist
#from mpi4py import MPI
import snAnalysisMod

# Establish MPI objects
#comm = MPI.COMM_WORLD
#size = comm.Get_size()
#rank = comm.Get_rank()
size = 1
rank = 0

def main(argv):
    # Parse arguments passed in.
    parser = argparse.ArgumentParser(description='Main program to run analysis on WRF-Hydro')
    parser.add_argument('modelProjects', metavar='alias', type=str, nargs='+',
	     		        help='A list of model projects to run analysis on')
    parser.add_argument('--jobName',nargs='?', help='Customized name which will be the sub-directory containing results')
    parser.add_argument('--begADate',nargs='?', help='Beginning Date for Analysis/Read in YYYYMMDDHH Format')
    parser.add_argument('--endADate',nargs='?', help='Ending Date for Analysis/Read in YYYYMMDDHH Format') 
    parser.add_argument('--inFile',nargs='?', help='Input file necessary for reading/analysis. Can either be model reads, or observation point file')
    parser.add_argument('--snRead',nargs='?', help='Read flag for snow analysis and observations (1-4)')
    parser.add_argument('--snRun',nargs='?', help='Snow analysis flag (1-6)')
    parser.add_argument('--bsnMskFile',nargs='?', help='Optional basin/region R mask file used for reading/analysis.')
    parser.add_argument('--bsnSubFile',nargs='?', help='CSV text file listing subset of basins/regions to read/process')
      
    args = parser.parse_args()

    # Check to make sure arguments make sense
    try:	
        pyHydroEvalUtils.checkArgs(args)
    except:
        print "ERROR: Improper arguments passed."
        sys.exit(1)

    # Convert date arguments to datetime objects. Also check to ensure dates make sense.
    if args.begADate or args.endADate:
        begADateObj = pyHydroEvalUtils.returnDate(args.begADate)
        endADateObj = pyHydroEvalUtils.returnDate(args.endADate)
        if begADateObj >= endADateObj:
            print "ERROR: Beginning analysis date must be less than ending date."
            sys.exit(1)

    # Check to ensure proper files exist to run analysis
    dbPath = "./parm/modelMeta_db.pkl"
    #namelistTemplate = "./parm/namelist_template.R"

    if not os.path.isfile(dbPath):
        print "ERROR: Database: " + dbPath + " not found."
        sys.exit(1)
    #if not os.path.isfile(namelistTemplate):
    #    print "ERROR: Template R namelist file: " + namelistTemplate + " not found."
    #    sys.exit(1)

    # Read in model project database
    try:
        db = pyHydroEvalUtils.readDb(dbPath)
    except:
        print "ERROR: Model project database failed to read in."
        sys.exit(1)

    # Check that alias names passed in are present in model project database
    try:
        pyHydroEvalUtils.checkDb(args,db)
    except:
        print "ERROR: checkDb failed."
        sys.exit(1)
        
    # Create subdirectory based on job name to hold output analysis/datasets/plots
    try:
        pyHydroEvalUtils.mkJobDir(args,db)
    except:
        print "ERROR: Unable to create job name subdirectory."
        sys.exit(1)
    
    # Run snow reading functions if desired by user.
    try:
        snAnalysisMod.readSnow(args,db,begADateObj,endADateObj,size,rank)
    except:
        print "ERROR: Unable to read snow observations/analysis in."
        sys.exit(1)

    ## Copy template namelist file to namelist directory in model project directory.
    ## If multiple model projects have been chosen for cross-model validation, 
    ## original namelist will be placed in first model project listed, with symbolic
    ## links in remaining model projects.
    #try:
    #    namePath, nameLink = pyHydroEvalUtils.initNamelist(args,db,rank)
    #except:
    #    print "ERROR: Failure to initialize R namelist file."
    #    sys.exit(1)

    #print 'EXTRACTING SNOW OBSERVATIONS'
    ## If observations from Database needed, extract here
    #try:
    #    snowDbMod.extractObs(args,db,size,rank,begADateObj,endADateObj)
    #except:
    #    print "ERROR: Failure to extract snow observations from web database."
    #    os.unlink(nameLink)
    #    sys.exit(1)
        
    ## Begin editing R namelist file
    #try:
    #    compileNamelist.editNamelist(namePath,args,db,size,rank)
    #except: 
    #    print "ERROR: Failure to compile R namelist file."
    #    os.unlink(nameLink)
    #    sys.exit(1)	

    #cmd = "Rscript " + nameLink
    #subprocess.call(cmd,shell=True)	

    # Remove namelist link specific to processor ID
    #try:
    #    os.unlink(nameLink)
    #except:
    #    print "ERROR: Failure to remove link: " + nameLink
    #    sys.exit(1)

    # If Rplots.pdf file exists, remove it.
    #if os.path.isfile('./Rplots.pdf'):
    #    cmd = 'rm -rf Rplots.pdf'
    #    subprocess.call(cmd,shell=True)

if __name__ == "__main__":
	main(sys.argv[1:])
