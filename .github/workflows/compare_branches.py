#!/usr/bin/env python3

# args: <master throughput> <master phases> <feature throughput> <feature phases>

import sys

PERCENT_FORMAT = "{:+.1f}%"
ABSOLUTE_FORMAT = "{:+d}"

# reads the throughput from the file
def parse_throughput(throughput_file):
    with open(throughput_file) as f:
        line = f.readline()
        _, throughput_string = [field.strip() for field in line.split(",")]
        return int(throughput_string)
        
# reads the phases file and creates a dictionary phase -> phase time
def parse_phases(phases_file):
    with open(phases_file) as f:
        phases = {}
        for line in f:
            phase, _, time_string = [field.strip() for field in line.split(",")]
            time = int(time_string)
            phases[phase] = time
        return phases

# creates a dictionary phase -> (master time, feature time, absolute increase, percent increase)
# all values are strings
def build_phase_comparison(master_phases, feature_phases):
    table = {}
    for phase in master_phases:
        master_time = master_phases[phase]
        feature_time = feature_phases[phase]
        absolute_increase = feature_time - master_time
        percent_increase = absolute_increase / master_time * 100
        table[phase] = (str(master_time), str(feature_time), ABSOLUTE_FORMAT.format(absolute_increase), PERCENT_FORMAT.format(percent_increase))
    return table

# creats a tuple (master throughput, feature throughput, absolute increase, percent increase)
def build_throughput_comparison(master_throughput, feature_throughput):
    absolute_increase = feature_throughput - master_throughput
    percent_increase = absolute_increase / master_throughput * 100
    return (str(master_throughput), str(feature_throughput), ABSOLUTE_FORMAT.format(absolute_increase), PERCENT_FORMAT.format(percent_increase))

def print_table_line(fields):
    print(" | ".join(fields))

def print_table_header(fields):
    print_table_line(fields)
    print_table_line("---" for f in fields)

def print_phase_table(table):
    print_table_header(("phase", "master time", "feature time", "absolute difference", "percent difference"))
    for phase in table:
        print_table_line((phase,) + table[phase])

def print_throughput_table(table):
    print_table_header(("master throughput", "feature throughput", "absolute difference", "percent difference"))
    print_table_line(table)

def main():
    _, master_throughput_file, master_phases_file, feature_throughput_file, feature_phases_file = sys.argv
    master_throughput = parse_throughput(master_throughput_file)
    feature_throughput = parse_throughput(feature_throughput_file)
    master_phases = parse_phases(master_phases_file)
    feature_phases = parse_phases(feature_phases_file)
    
    throughput_comparison = build_throughput_comparison(master_throughput, feature_throughput)
    phase_comparison = build_phase_comparison(master_phases, feature_phases)

    print_throughput_table(throughput_comparison)
    print()
    print_phase_table(phase_comparison)

if __name__ == "__main__":
    main()

