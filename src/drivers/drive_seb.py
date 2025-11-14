# -*- coding: utf-8 -*-

""" This is the Techne driver: takes an input file and options and calls appropriate API functions"""

from optparse import OptionParser, OptionGroup
from convert_omni_seb import ParseQGM
import option_parser
import display_opt

usage = "usage: %prog [options]"
version = "%prog 0.3_re"
desc = "Evaluate Seb models"

parser = OptionParser(usage=usage, version=version, description=desc)
parser.add_option("-q", "--quiet",  dest="verbose", 
                    action="store_false", help="don't print status messages to stdout")
parser.add_option("-v", "--verbose", dest="verbose", 
                    action="store_true", default=True, help="make lots of noise [default]")
parser.add_option("-c", "--cost", dest="cost", 
                    action="store", default=0, help="define the maximum cost of a solution")
parser.add_option("-r", "--run", dest="run_id",
                    action="store", default=0, help="which run are we trying (valid: 0-7)")
parser.add_option("-o", "--cost_increment", dest="increment", 
                    action="store", default=5, help="define the increment by which to adjust the cost threshold")
group = OptionGroup(parser, "Arguments for dealing with optional elements")  
group.add_option("-u", "--use-naive", dest="use_naive", action="store_true", default=True,
                    help="use naive parsing. O(2^N), not recommended.")
group.add_option("-m", "--omax", dest='o_max', action='store', 
                    help="specify an upper bound on the number of options to try to satisfy")
group.add_option("-n", "--omin", dest='o_min', action='store', 
                    help="specify a lower bound on the number of options to try to satisfy")                        
group.add_option("-t", "--use_tabu", dest='use_tabu', action='store_true',
                    help="use tabu search to find a local optimal set.")
group.add_option("-p", "--use_moop", dest='use_moop', action='store_true', 
                    help="use multi-objective optimization to find optimal set. \
                    Need cost figures attached to the optional nodes.") #cost= in graphical editor.
parser.add_option_group(group)
(options, args) = parser.parse_args()
if options.verbose:
    pass
if options.o_min != None:
    options.o_min = int(options.o_min)
if options.o_max != None:
    options.o_max = int(options.o_max)  

# identify if admissible
#initial run with no options.
parser = ParseQGM()  
parser.set_node_ids() #label the nodes with the appropriate ids for SEB
parser.generate_seb()  # the Sebastiani goal SAT approach                     
parser.generate_atms() # the ATMS techne file
parser.print_files() # save the mandatory/non-optional nodes to disk
# admissible = parser.run_seb() # see if the result is admissible.
# if admissible: print "was admissible"
# else: print "not admissible"
# parser.zero_counts()
#load this run's options
#mands, opts, prefs = display_opt.get_options(options.run_id)
#print mands,opts, prefs
#display_opt.clear_model() # set all nodes to plain
#display_opt.set_graph(mands,opts) # set nodes to their appropriate status
# generate option sets
#if options.use_naive:
#    results = option_parser.naive_option(parser, opts, options.o_min, options.o_max)
#    print results



