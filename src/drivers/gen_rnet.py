"""
A generator which takes a random digraph and
turns it into a series of Techne assertions and 
declarations
"""

import networkx as nx
import pydot
import random

testfile = "/Users/nernst/Desktop/gn-test.dot"
rekb_name = "*rekb*"
rekb_str = "(defvar "+ rekb_name + " (create-rekb \"Random rekb\"))\n"
load_str = "(load \"/Users/nernst/Dropbox/research/techne/src/tms/techne-psm.lisp\")\n"
n = 600
num_out = 600 # the number nodes to add extra arcs to. Every num_out nodes.
k_prob = 1
ratio = 50 #how many contradictions to fill
kernel = lambda x:x**k_prob # probability of attaching to a node with existing edges. Higher the exponent the more likely you are (graph is flatter)
break_val = 3 # if a goal has more than X incoming links, group them into alternatives.
# write to file
out = file("examples/rand-" + str(n) + "-r300.techne", 'w')

def main():
    # generate the graph
    s = nx.gn_graph(n,kernel) # growing network, no copying 
    # see ht tp://networkx.lanl.gov/reference/generated/networkx.generators.directed.gn_graph.html

    # find assumptions - leaf nodes, with no incoming links
    dests = set([edge[1] for edge in s.edges()]) # non-leafs

    # add more out-degrees
    add_out_deg(n,s,dests,num_out)

    # generate the nodes-atoms
    nodes_str = "(setf "
    for node in s.nodes():
        if node in dests:
            nodes_str += "\tnode-" + str(node) + " (declare-atomic nil \"Goal-" + str(node) + "\" :GOAL " + rekb_name + ")\n"
        else: 
            nodes_str += "\tnode-" + str(node) + " (declare-atomic nil \"Task-" + str(node) + "\" :TASK " + rekb_name + ")\n"
    nodes_str += ")"

    #nx.to_pydot(s).write(testfile)

    #generate the formulas from the edges.
    form_str = ""
    for consequent in dests:
        antecedents = s.predecessors(consequent)
        if len(antecedents) >= break_val:
            # break into alternatives.
            ants = chunks(antecedents,break_val)
        else:
            ants = [antecedents]
        i = 0
        for group in ants:
            form_str += "(assrt-formula node-" + str(consequent) + " (list " + " ".join(["node-"+ str(x) for x in group]) \
                         + ") :DA \"phi-" + str(consequent) + "-" + str(i) + "\" " + rekb_name + ")\n"
            i += 1

    contr_str = add_contra(n,s,ratio)
    connect_percent = sum(nx.algorithms.centrality.degree_centrality(s).values()) / n # % of all nodes a node connects to.
    out.write(";; Leaves: " + str(n - len(dests)) + "  Connectivity: %" + str(connect_percent * 100) + " Contradictions: " + str(n / ratio) + "\n" \
                  + load_str + rekb_str + nodes_str + "\n" + form_str + contr_str)

def add_contra(n,s,ratio):
    """ add con_size contradiction nodes randomly """
    contr_string = ""
    con_size = n / ratio # 1 contradiction every ratio nodes.
    #can't be zero and can't have existing edge between them.
    for i in range(con_size):
        while True:
            c1 = random.choice(s.nodes())
            c2 = random.choice(s.nodes())
            if c1 != 0 and c2 != 0 and c1 != c2: # a value of 0 indicates the root
                contr_string += "(assrt-formula (contradiction " + rekb_name + ") (list node-" + str(c1) + " node-" \
                               + str(c2) + ") :DA \"psi-" + str(i) + "\" " + rekb_name + ")\n" 
                break
    return contr_string

def add_out_deg(n,s,dests,num_out):
    """ modify the tree. For every third node in the current tree, add a new edge to a node higher up in the tree (no cycles)"""
    reversed = range(n)
    reversed.reverse()
    
    for i in reversed:
        if i % num_out == 0: #every third node:
            added = False
            while not added and i != 0:
                attempt = random.choice(list(dests))
                # can never go from smaller number to greater number - smaller is always dest.
                if attempt < i:
                    s.add_edge(i,attempt)
                    #print "added edge (" + str(i) + "," + str(attempt) + ")"
                    added = True

def chunks(l, n):
    """ split l into n-sized chunks c/o 
    http://stackoverflow.com/questions/312443/how-do-you-split-a-list-into-evenly-sized-chunks-in-python/1751478#1751478"""
    return [l[i:i+n] for i in range(0, len(l), n)]

if __name__ == '__main__':
    main()
