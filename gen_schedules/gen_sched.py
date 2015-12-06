#!/usr/bin/python -tt
# gen_sched.py
#
# Generates schedules for people and writes them into files.

import sys, random, os

DEFAULT_DIR = "schedules"
DEFAULT_NUM_SCHEDS = 10

def parse_vertices(graph_file):
    vertices = []
    for line in graph_file: 
        vertices.append(line.rstrip('.'))
    return vertices

def make_directory(name):
    if not os.path.exists(name):
        os.mkdir(name, 0700)

def make_sched(vertices):
    

def usage():
    print "Usage: gen_sched.py -g filename -n num_scheds -o output_directory"

def main(args):
    directory = DEFAULT_DIR
    num_scheds = DEFAULT_NUM_SCHEDS
    graph_file = None

    for i in range(0, len(args)):
        if args[i] == "-g":
            try 
                graph_file = open(args[i + 1], 'r')
            except
                sys.stderr.write("Bad file/n")
        elif args[i] == "-n":
            num_scheds = int(args[i + 1])
        elif args[i] == "-o":
            directory = args[i + 1]
            make_directory(directory)
    if graph_file = None:
        usage()
        return

    vertices = parse_vertices(graph_file)
    graph_file.close()

    for i in range(0, num_scheds):

    print vertices

    print random.randint(0, 10)
    return

if __name__ == '__main__':
    main(sys.argv)
