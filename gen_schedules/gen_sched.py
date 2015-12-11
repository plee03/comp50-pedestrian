#!/usr/bin/python -tt
# gen_sched.py
# By Peter Lee, Caitlin Klein
#
# Generates schedules for people and writes them into text files, given a graph
# containing vertices and edges.

import sys, random, os

DEFAULT_DIR = "schedules"       # Default directory for schedule files
DEFAULT_NUM_SCHEDS = 10         # Default number of schedules
DEFAULT_INTERVAL = 180          # Default interval of locations in schedules
MAXTIME = 1440                  # Generates times and locations up to this time

def parse_vertices(graph_file):
""" Given an opened file, returns a list of vertex names from the 
    file as strings
""" 
    vertices = []
    for line in graph_file:
        if line[0] == "{" or line == "\n":
            continue
        vertices.append(line.rstrip('.\n'))
    return vertices

def make_directory(name):
""" Makes a directory with the given name if it does not exist already
"""
    if not os.path.exists(name):
        os.mkdir(name, 0700)

def make_sched(vertices, interval):
""" Given a list of vertex names and a time interval, generates a string
    representing a schedule.
"""
        
    schedule = ""
    i = 0
    while i < MAXTIME:
        location = pick_random_vertex(vertices)
        schedule += "{" + str(i) + ", " + location + "}.\n"
        i += interval
    return schedule
def pick_random_vertex(vertices):
""" Given a list of vertex names, returns a pseudorandom vertex
"""
    return vertices[random.randint(0, len(vertices) - 1)]

def write_sched(directory, sched, sched_num):
""" Writes a schedule to the file directory/sched with sched_num appended 
    to the end.
"""
    f = open(directory + "/sched" + str(sched_num) + ".txt", "w")
    f.write(sched)
    f.close()
    
def usage():
""" Prints usage
"""
    sys.stdout.write("Usage: gen_sched.py -g filename -n num_scheds " + 
                     "-o output_directory -i interval\n")

def main(args):
    directory = DEFAULT_DIR
    num_scheds = DEFAULT_NUM_SCHEDS
    interval = DEFAULT_INTERVAL
    graph_file = None

    for i in range(0, len(args)):
        if args[i] == "-g":
            try:
                graph_file = open(args[i + 1], 'r')
            except:
                sys.stderr.write("Bad file/n")
        elif args[i] == "-n":
            num_scheds = int(args[i + 1])
        elif args[i] == "-i":
            interval = int(args[i + 1])
        elif args[i] == "-o":
            directory = args[i + 1]
    if graph_file == None:
        usage()
        return
    make_directory(directory)

    vertices = parse_vertices(graph_file)
    graph_file.close()
    scheds_list_file = open(directory + "/schedules.txt", "w")
    for i in range(0, num_scheds):
        sched = make_sched(vertices, interval)
        write_sched(directory, sched, i)
        scheds_list_file.write("'" + directory + "/sched" + str(i) + ".txt'.\n")
    scheds_list_file.close()
    return

if __name__ == '__main__':
    main(sys.argv)
