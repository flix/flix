#!/usr/bin/env python3

# args: <master throughput> <master phases> <feature throughput> <feature phases>

import sys
import json

PERCENT_FORMAT = "{:+.1f}%"
ABSOLUTE_FORMAT = "{:+d}"
NA = "-"


# reads the throughput from the file
def parse_throughput(throughput_file):
    with open(throughput_file) as f:
        data = json.load(f)
        return data["throughput"]


# reads the phases file and creates a dictionary phase -> phase time
def parse_phases(phases_file):
    with open(phases_file) as f:
        phases = {}
        data = json.load(f)
        phase_list = data["phases"]
        for phase in phase_list:
            name = phase["phase"]
            time = phase["time"]
            phases[name] = time
        return phases


def build_comparison(master, feature):
    table = {}
    stats = set(master.keys()).union(set(feature.keys()))
    for stat in stats:
        master_stat = master.get(stat)
        feature_stat = feature.get(stat)
        master_str = NA if master_stat is None else str(master_stat)
        feature_str = NA if feature_stat is None else str(feature_stat)
        if master_stat is None or feature_stat is None:
            abs_incr_str = NA
            perc_incr_str = NA
        else:
            abs_incr = feature_stat - master_stat
            perc_incr = abs_incr / master_stat * 100
            abs_incr_str = ABSOLUTE_FORMAT.format(abs_incr)
            perc_incr_str = PERCENT_FORMAT.format(perc_incr)
        table[stat] = (master_str, feature_str, abs_incr_str, perc_incr_str)
    return table


def print_table_line(fields):
    print(" | ".join(fields))


def print_table_header(fields):
    print_table_line(fields)
    print_table_line("---" for f in fields)


def print_table(table, unit, category):
    print_table_header((category, f"master {unit}", f"feature {unit}", "absolute difference", "percent difference"))
    for stat in table:
        print_table_line((stat,) + table[stat])


def main():
    _, master_throughput_file, master_phases_file, feature_throughput_file, feature_phases_file = sys.argv
    master_throughput = parse_throughput(master_throughput_file)
    feature_throughput = parse_throughput(feature_throughput_file)
    master_phases = parse_phases(master_phases_file)
    feature_phases = parse_phases(feature_phases_file)
    
    throughput_comparison = build_comparison(master_throughput, feature_throughput)
    phase_comparison = build_comparison(master_phases, feature_phases)

    print_table(throughput_comparison, "throughput", "stat")
    print()
    print_table(phase_comparison, "time", "phase")


if __name__ == "__main__":
    main()

