# Program to extract snow observations from the SQL database at NCAR. 
# Several options are available to the user:
# 1.) Extract all observations within a time period.
# 2.) Extract all observations from subset of networks within a time period.
# 3.) Extract specific observations within a time period (I.E. a list of stations)
# 
# In order to utilize the extraction properly, the user needs to either know
# the network names, or unique ID values desired to extract. This can be done
# interactively by scoping out the meta data tables in the snow database.

# Logan Karsten
# National Center for Atmospheric Research
# Research Applications Laboratory

import sys
sys.path.insert(0, './python')

import argparse
import pyHydroEvalUtils
import snowDbMod

def main(argv):
    # Parse arguments passed in.
    parser = argparse.ArgumentParser(description='Main program to extract snow observations from database')
    parser.add_argument('--begRDate',nargs='?', help='Beginning Date for Read in YYYYMMDDHH Format')
    parser.add_argument('--endRDate',nargs='?', help='Ending Date for Read in YYYYMMDDHH Format')
    parser.add_argument('--outDir',nargs='?', help='Output directory to hold observations')
    parser.add_argument('--geoFile',nargs='?', help='Input geogrid file')
    parser.add_argument('--netList',nargs='?', help='List of Observation Networks for Reading')
    parser.add_argument('--stnList',nargs='?', help='List of Observation Stations for Reading')
    parser.add_argument('--mskFile',nargs='?', help='R mask file for cutouts of regions to process')

    args = parser.parse_args()
    
    print len(args)
    if len(args) < 4:
        print "ERROR: Insufficient arguments passed to program"
        sys.exit(1)
        
    # Check to make sure arguments make sense
    try:	
        pyHydroEvalUtils.checkSNArgs(args)
    except:
        print "ERROR: Improper arguments passed."
        sys.exit(1)
        
    # Convert date arguments to datetime objects. Also check to ensure dates make sense.
    if args.begRDate or args.endRDate:
        begRDateObj = pyHydroEvalUtils.returnDate(args.begRDate)
        endRDateObj = pyHydroEvalUtils.returnDate(args.endRDate)
        if begRDateObj >= endRDateObj:
            print "ERROR: Beginning analysis date must be less than ending date."
            sys.exit(1)
            
    print 'EXTRACTING SNOW OBSERVATIONS'
    # If observations from Database needed, extract here
    try:
        snowDbMod.extractObs(begRDateObj,endRDateObj,args.outDir,args.geoFile,\
                             networkFile=args.netList,stnFile=args.stnList,\
                             maskFile=args.mskFile)
    except:
        print "ERROR: Failure to extract snow observations from web database."
        sys.exit(1)