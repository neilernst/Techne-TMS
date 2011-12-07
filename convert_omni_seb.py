# -*- coding: utf-8 -*-
from appscript import *
import subprocess
import re
import settings as s #constants

class SebException(Exception):
    """ domain specific exception """
    pass
    
class ParseQGM():
    """
    Author nernst
    Use py-appscript to
    convert OmniGraffle QGM models to Seb SAT syntax
    """

    def __init__(self, canvas_name=s.CANVAS_NAME):
        __graffle = app('OmniGraffle Professional 5')
        __doc = __graffle.documents[1] # the top document, note 1-indexed array
        self.__s = __doc.canvases[canvas_name].get()

        self.__shapes = self.__s.shapes.get()
        self.__graphics = self.__s.graphics.get()
        self.__node_cnt = 0
        self.__opt_cnt = 0
        self.__rel_cnt = 0
        self.__true_rel_cnt = 0 # number of relations after AND/OR have been aggregated.
        self.__rel_types = ['AND', 'OR', '+', '-','++', '--', 'p', 'conflict']
        self.__src_map = {'AND':[], 'OR':[], '+':[], '-':[],'++':[], '--':[], 'p':[], 'conflict':[]} # store the node ids that are sources. #em dash is Unicode
        self.__contribs = ['+', '-', '++', '--']
        self.__opt_list  = [] # a list of nodes which are optional
        self.__decomp_str = ''
        self.__contr_str = ''
        self.__des_str = ''    
        self.__just_str = ''     
        self.__mand_str = ''
        self.__conflict_str = ''
        self.__id_sebid_map = {}   
        self.__atms_nodes_map = {'goal':[], 'task':[], 'domain-assumption':[], 'leaf':[]}   # no quality-constraints
        self.__atms_nodes = []
        
    def set_node_ids(self):
        """pass 1 - set the node ids, zero indexed"""
        for element in self.__shapes:
            data = element.user_data.get()
            nodeid = element.id.get()
            if data != None:       
                opt = data['optional'] == 'T'
                if not opt:
                    # set seb_id
                    data['seb_id'] = self.__node_cnt
                    self.__id_sebid_map[self.__node_cnt] = nodeid
                    element.user_data.set(data)
                    self.__node_cnt += 1       
                    # set atms list    
                    # ideally do something like this, where we set the type from the graph 
                    node_type = data['node_type']                 
                    node_name = 'r-' + str(data['seb_id']) # LISP does not allow ints as identifiers
                    self.__atms_nodes_map[node_type].append(node_name)  
                    self.__atms_nodes.append(node_name)
                    
        #pass 2 - set the node ids for optional nodes. Must be named after the mandatory ones.
        # opt_cnt = self.__node_cnt
        #         for element in self.__shapes:
        #             opt = False
        #             text = element.text.get()
        #             data = element.user_data.get()
        #             if data != None:
        #                 opt = data['optional'] == 'T'
        #                 if opt:
        #                     self.__opt_list.append(data['seb_id']) # build a list of nodes which are optional
        #                     # set seb_id
        #                     nodeid = element.id.get()
        #                     data['seb_id'] = opt_cnt
        #                     element.user_data.set(data)
        #                     self.__id_sebid_map[opt_cnt] = nodeid
        #                     opt_cnt += 1
        #         self.__opt_cnt = opt_cnt - self.__node_cnt    
    
    def generate_atms(self):
        """Generate .techne files for use with Techne ATMS implementation"""
        for element in self.__graphics:
            for key in self.__src_map.keys():
                self.__src_map[key] = [] # remove preceding iteration's data
            data = element.user_data.get()
            if element.class_.get() != k.shape or data == None:    ## NOTA BENE!! This does nothing yet is necessary to prevent errors. ::shrug::
                 #print 'not a node' #return #either a line, or a label
                 continue
            # #print element.id.get()  
            in_lines = element.incoming_lines.get()
            for l in in_lines:
                try:
                    l_text = l.user_data.get()['rel_type'] # one of rel_types 
                    seb_id = l.source.user_data.get()['seb_id']                 
                    # TODO only one possible rel_type per node. Check that.
                    self.__src_map[l_text].append('r-' + str(seb_id)) # e.g. ['AND'][r-93,r-94...] 
                except KeyError as ke:
                    print ke, 'no rel_type on that line', l.id.get(), l.user_data.get()['rel_type']
        
            #generate relations
            # AND/OR - the only node types with multiple srcs
            and_srcs = ' '.join(self.__src_map['AND'])
            or_srcs = self.__src_map['OR'] 
            conflict_srcs = self.__src_map['conflict'] # we are assuming there is only binary conflict
            if and_srcs != '':
                self.__just_str +=  '(assert-formula ' + 'r-' + data['seb_id'] + ' (list ' + and_srcs + ') :DA \"a just\" *rekb* )\n'
            if or_srcs != []: 
                for node in or_srcs:
                    self.__just_str +=  '(assert-formula ' + 'r-' + data['seb_id'] + ' (list ' + node + ' ) :DA \"a just\" *rekb* )\n'
            if conflict_srcs != []:
                self.__conflict_str += '(assert-formula  (contradiction *rekb*) (list ' + conflict_srcs[0] + ' r-' + data['seb_id'] + ') :DA "hurt4" *rekb*)\n'

            try:
                if data['mandatory'] == 'T':
                    m_node_id = data['seb_id']
                    self.__mand_str += '(assert-mandatory r-' + m_node_id + ' *rekb* T)\n' 
            except KeyError as ke:
                pass
                
    def generate_seb(self):
        opt = False
        for element in self.__graphics:
            for key in self.__src_map.keys():
                self.__src_map[key] = [] # remove preceding iteration's data
            data = element.user_data.get()
            # if element.class_.get() == k.line and data != None:
            #     text = data['rel_type'] #AND/OR/+/- etc.
            if element.class_.get() != k.shape or data == None:
                #print 'not a node' #return #either a line, or a label
                continue
            #print element.id.get()
            opt = data['optional'] == 'T'
            if not opt:     
                in_lines = element.incoming_lines.get() 
                for l in in_lines:
                    try:
                        l_text = l.user_data.get()['rel_type'] # one of rel_types 
                        seb_id = l.source.user_data.get()['seb_id'] 
                        self.__src_map[l_text].append(seb_id) # e.g. ['+'][93,94] ...
                    except KeyError as ke:
                        print ke, 'no rel_type on that line', l.id.get(), l.user_data.get()['rel_type']
            
            #for non-optional nodes, generate relations
            rel_type = ''
            # purge optional nodes from list of sources for this sink
            for rel_type in self.__rel_types: 
                tmp = []
                for x in self.__src_map[rel_type]: # all nodes which are sources 
                    if x not in self.__opt_list:
                        tmp.append(x)
                self.__src_map[rel_type] = tmp
            
            # AND/OR - the only node types with multiple srcs
            and_srcs = ' '.join(self.__src_map['AND'])
            or_srcs = ' '.join(self.__src_map['OR'])
            if and_srcs != '':
                self.__decomp_str +=  'R AND ' + data['seb_id'] + ' ' + and_srcs + '\n'
                self.__true_rel_cnt += 1
            if or_srcs != '':
                self.__decomp_str +=  'R OR ' + data['seb_id'] + ' ' + or_srcs + '\n'
                self.__true_rel_cnt += 1
            # contributions
            srcs = ''
            for c in self.__contribs:
                srcs = self.__src_map[c]
                if srcs != []:
                    for src in srcs:
                        self.__contr_str +=  'R ' + c + ' ' + data['seb_id'] + ' ' + src + '\n'
                        self.__true_rel_cnt += 1

            if data['mandatory'] == 'T':
                m_node_id = data['seb_id']
                self.__des_str += 'TS ' + m_node_id + ' ;\n-PD ' + m_node_id + ' ;\n'
                # here we will add to the .des file the goal ids that must be "TS" and -PD
    
    def get_optional_ids(self):
        """ return a list of graffle ids of nodes labeled as optional""" 
        #print "num optional: ", self.__opt_cnt
        options_id_map = {}
        for option in self.__opt_list:
            # print option, options_id_map, self.__id_sebid_map
            # try:
            options_id_map[option] = self.__id_sebid_map[int(option)]
            # except KeyError: 
            #     print "option not in list of ids"
            #     return None
        return options_id_map
        
    def set_node_status(self, nodeid, op='to_mandatory'):
        """ given a nodeid, set the user data"""
        node_data = self.__s.shapes.ID(nodeid).user_data.get()
        if op == 'to_mandatory':
            node_data['optional'] = u'F'
            node_data['mandatory'] = u'T'
        elif op == 'to_optional':
            node_data['optional'] = u'T'
            node_data['mandatory'] = u'F'
        elif op == 'to_unknown':
            node_data['optional'] = u'F'
            node_data['mandatory'] = u'F'
        else:
            print "unrecognized node status operation", op
        self.__s.shapes.ID(nodeid).user_data.set(node_data)
        
    def print_files(self):  
        """ legacy support"""
        self.print_seb_files()   
        self.print_atms_files()   
                   
    def find_node_type(self, node):
        if node == '':
            return
        if node in self.__atms_nodes_map['goal']:
            return "GOAL"
        if node in self.__atms_nodes_map['task'] or node in self.__atms_nodes_map['leaf']:
            return "TASK"
        if node in self.__atms_nodes_map['domain-assumption']:
            return "GOAL"   
            
    def print_atms_files(self):
        atms_file = s.OUT_DIR + s.ATMS_FILE          
        node_str = ''
        out = open(atms_file, 'w')
        header = '(load "/Users/nernst/Dropbox/research/src/tms/techne-atms.dx64fsl")\n(load "/Users/nernst/Dropbox/research/src/tms/techne-psm.lisp")\n(defvar *rekb* (create-rekb "pcidss"))\n(setq \n'
        out.write(header.encode('utf-8'))    
        for node in self.__atms_nodes:  
            node_type = self.find_node_type(node)
            node_str += '\t' + node + ' (declare-atomic nil \"'+ node + '\" :'+ node_type + ' *rekb*)\n'   
        out.write(node_str.encode('utf-8'))
        footer = ')\n\n;;Justifications\n'
        out.write(footer.encode('utf-8'))  
        out.write(self.__just_str.encode('utf-8')) 
        out.write(u'\n;;Conflicts\n')
        out.write(self.__conflict_str.encode('utf-8'))   
        out.write(self.__mand_str.encode('utf-8'))
        out.close()
        
    def print_seb_files(self):   
        out_file  = s.OUT_DIR + s.GOAL_FILE
        des_file = s.OUT_DIR + s.DES_FILE 

        out = open(out_file, 'w')
        header = 'D ' + str(self.__node_cnt) + ' ' + str(self.__true_rel_cnt) + '\n'
        print header
        out.write(header.encode('utf-8'))
        for i in range(self.__node_cnt):
            out.write(u'N NO NO\n')
        out.write(self.__contr_str.encode('utf-8'))
        out.write(self.__decomp_str.encode('utf-8'))
        out.close()

        des = open(des_file, 'w')
        des.write('\n')
        des.write(self.__des_str.encode('utf-8'))
        des.close()
    
    def zero_counts(self):
        self.__node_cnt = 0
        self.__rel_cnt = 0
        self.__true_rel_cnt =0
        self.__opt_cnt = 0

    def run_seb(self):
        """ run the Sebastiani algorithm (via Chaff) and return admissibility """
        
        sat_dir = '/Users/nernst/Dropbox/research/projects/goal-reasoning/backward_prop/GRTool-Src/Solvers/src/Goalreasoner/lin/Goalsolve/'
        p = subprocess.Popen(args=[sat_dir + 'goalsolve', '-n', '/tmp/re2010.goal', '/tmp/re2010.des'],
                             stdin=subprocess.PIPE, stderr=subprocess.PIPE, stdout=subprocess.PIPE,
                             cwd=sat_dir) 
        stdout, stderr = p.communicate()
        sat_pattern = re.compile('\+ The problem admits solutions   \+')
        unsat_pattern = re.compile('\+ The problem admits no solution \+')
        chaff_err_pattern = re.compile('Wrong output from chaff')
        sat = sat_pattern.search(stderr)
        unsat = unsat_pattern.search(stderr)
        if unsat != None:
            print 'Error occured in SAT run or no admissible result'
            print stderr
            raise SebException
        else:
            return True
            
    def negate_repl(matchobj):
        """ replace 'literal' with '-literal' and vice-versa"""
        neg, literal = matchobj.groups(1)
        if neg == '-':
            return literal
        else:
            return '-' + literal
            
    def negate(sat_assign):
        """ given a satisfying assignment, return that assignment of the initial labels, negated"""
        new = re.sub('(-*)(\d+)', negate_repl, sat_assign)
        return new 

if __name__ == '__main__':
    q = ParseQGM()
    
