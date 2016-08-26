# Program to take existing model entry in the database,
# copy it to a new alias name, and replace existing
# options based on input arguments. This is useful
# when multiple iterations of same model domain are 
# done when most options are static except for 
# source model output directory.

# Logan Karsten 
# National Center for Atmospheric Research
# Research Applications Laboratory

import sys
import os
import pickle
import argparse 

sys.path.insert(0, './python')

import setupMod

def main(argv):
    # Parse arguments passed in.
    parser = argparse.ArgumentParser(description='Program to copy existing model entry and modify contents')
    parser.add_argument('inOutProjects', metavar='alias', type=str, nargs='+',
                        help='existing model alias and new model alias names')
    parser.add_argument('--topOut', nargs='?', help='New top output directory for new project')
    parser.add_argument('--modelIn', nargs='?', help='New model input directory for new project')
    parser.add_argument('--tag', nargs='?', help='New model project tag')
    parser.add_argument('--ensList', nargs='?', help='New model project ensembles')
    parser.add_argument('--ensTag', nargs='?', help='New model project ensemble tags')
    parser.add_argument('--forcingDir', nargs='?', help='New model project forcing directory')
    parser.add_argument('--mskFile', nargs='?', help='New model project mask file.')
    parser.add_argument('--geoFile', nargs='?', help='New model project mask file.')
    parser.add_argument('--geoRes', nargs='?', help='New model project LSM resolution.')
    parser.add_argument('--agg', nargs='?', help='New model project aggregation factor.')
    parser.add_argument('--hydFile', nargs='?', help='New model project hydro geo file.')
    parser.add_argument('--statsLink2gage', nargs='?', help='New model project statsLink2gage file.')
    parser.add_argument('--plotLink2gage', nargs='?', help='New model project plotLink2gage file.')
    parser.add_argument('--basSub',nargs='?', help='New model project basin subset file.')
    parser.add_argument('--snPath', nargs='?', help='New SNODAS directory.')
    parser.add_argument('--streamDB',nargs='?', help='New streamflow database.')
    parser.add_argument('--snowDB',nargs='?', help='New snow observations database')
    parser.add_argument('--nCores', nargs='?', help='New model project number of cores to use.')
    
    args = parser.parse_args()
    
    if len(args.inOutProjects) != 2:
        print "ERROR: Incorrect number of arguments passed to program."
        sys.exit(1)
        
    try:
        setupMod.copyModelProject(args,args.inOutProjects[0],args.inOutProjects[1])
    except:
        print "ERROR: Failure to copy new model project."
        sys.exit(1)	
	
if __name__ == "__main__":
    main(sys.argv[1:])
