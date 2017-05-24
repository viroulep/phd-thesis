#!/usr/bin/env python
import itertools
import time
import subprocess
import glob
import sys
import os.path
from optparse import OptionParser


RHEADER = "Runtime Progname Size Blocksize Timesteps Iterations Threads Time Stddev"
if __name__ == "__main__":
    parser = OptionParser()
    parser.add_option('-b', '--basename', dest='basename',
                      help='Basename of logfiles')
    (options, args) = parser.parse_args()
    if not options.basename:
        parser.error("Basename is required !")
    datafile = "rdata." + options.basename + ".dat"
    if os.path.isfile(datafile):
            print "Error : output datafile already exists (" + datafile + ")\n"
            sys.exit(1)
    subprocess.call("echo \"" + RHEADER + "\" > " + datafile, shell=True)
    for f in glob.glob(options.basename + ".*.log"):
        fileinfo = f.split(".")
        if len(fileinfo) != 4:
            print "Error : one filename isn't in the right format (" + f + ")\n"
            sys.exit(1)
        runtime = fileinfo[2]
        xpid = fileinfo[1]
        prependstring = runtime + " "
        cmdLine = ("grep -v -i -e \"#\" -e \"^$\" " + f + " | sed 's/^/"
                   + prependstring + " /' >> " + datafile)
        print cmdLine
        subprocess.call(cmdLine, shell=True)

    print "Done"

