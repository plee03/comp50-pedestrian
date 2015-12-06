#!/usr/bin/python -tt
# gen_sched.py
#
# Generates schedules for people and writes them into files.

import sys, random, os

DEFAULT_DIR = "schedules"
DEFAULT_NUM_SCHEDS = 10
DEFAULT_INTERVAL = 180
MAXTIME = 1440
DEFAULT_SCHEDNAME = "sched"

def parse_vertices(graph_file):
    vertices = []
    for line in graph_file:
        if line[0] == "{" or line == "\n":
            continue
        vertices.append(line.rstrip('.\n'))
    return vertices

def make_directory(name):
    if not os.path.exists(name):
        os.mkdir(name, 0700)

def make_sched(vertices, interval):
    schedule = ""
    i = 0
    while i < MAXTIME:
        location = pick_random_vertex(vertices)
        schedule += "{" + str(i) + ", " + location + "}.\n"
        i += interval
    return schedule

def pick_random_vertex(vertices):
    return vertices[random.randint(0, len(vertices) - 1)]

def write_sched(directory, sched, sched_num):
    f = open(directory + "/sched" + str(sched_num) + ".txt", "w")
    f.write(sched)
    f.close()
    
def usage():
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
