import numpy as np
import matplotlib.pyplot as plt

statsList = []

# data structured as follows:
# Operator : #total : #killed : #survived : #unknown : #equivalent
with open("MutStats.txt", "r") as stats:
    data = stats.readlines()
    for line in data:
        line = line.split(":")

        op = line[0]
        total = int(line[1])
        killed = int(line[2])
        survived = int(line[3])
        unknown = int(line[4])
        equiv = int(line[5])

        statsList.append(
            {
                "op": op,
                "total": total,
                "killed": killed,
                "survived": survived,
                "unknown": unknown,
                "equiv": equiv,
            }
        )

# if there are multiple mutation operator that are the same, combine them into one

# combine the stats
for i in range(len(statsList)):
    for j in range(i + 1, len(statsList)):
        if statsList[i]["op"] == statsList[j]["op"]:
            statsList[i]["total"] += statsList[j]["total"]
            statsList[i]["killed"] += statsList[j]["killed"]
            statsList[i]["survived"] += statsList[j]["survived"]
            statsList[i]["unknown"] += statsList[j]["unknown"]
            statsList[i]["equiv"] += statsList[j]["equiv"]
            statsList[j]["op"] = "None"

# print the stats in a readable format
for stat in statsList:
    if stat["op"] != "None":
        print(
            stat["op"]
            + " : "
            + str(stat["total"])
            + " : "
            + str(stat["killed"])
            + " : "
            + str(stat["survived"])
            + " : "
            + str(stat["unknown"])
            + " : "
            + str(stat["equiv"])
        )

ops = []
total = []
killed = []
survived = []
unknown = []
equiv = []

for stat in statsList:
    if stat["op"] != "None":
        ops.append(stat["op"])
        total.append(stat["total"])
        killed.append(stat["killed"])
        survived.append(stat["survived"])
        unknown.append(stat["unknown"])
        equiv.append(stat["equiv"])

# plot the stats as a histogram, there should be a space between each operator

# use a very nice style
plt.style.use("ggplot")

fig, ax = plt.subplots()
index = range(len(ops))
bar_width = 0.1
opacity = 0.8

rects1 = plt.bar(index, total, bar_width, alpha=opacity, color="b", label="Total")
rects2 = plt.bar(
    [i + bar_width for i in index],
    killed,
    bar_width,
    alpha=opacity,
    color="r",
    label="Killed",
)
rects3 = plt.bar(
    [i + 2 * bar_width for i in index],
    survived,
    bar_width,
    alpha=opacity,
    color="g",
    label="Survived",
)
rects4 = plt.bar(
    [i + 3 * bar_width for i in index],
    unknown,
    bar_width,
    alpha=opacity,
    color="y",
    label="Unknown",
)
rects5 = plt.bar(
    [i + 4 * bar_width for i in index],
    equiv,
    bar_width,
    alpha=opacity,
    color="c",
    label="Equivalent",
)

plt.xlabel("Mutation Operators")
plt.ylabel("Number of Mutants")
plt.title("Mutation Operator Statistics")
plt.xticks([i + 2 * bar_width for i in index], ops)
plt.legend()

plt.tight_layout()
plt.savefig('plot1.pdf', dpi=300, bbox_inches='tight')


# make another plot showing the operators that survived order by the number of mutants that survived
sortedSurvived = sorted(statsList, key=lambda x: x["survived"], reverse=True)
ops = []
survived = []

for stat in sortedSurvived:
    if stat["op"] != "None":
        ops.append(stat["op"])
        survived.append(stat["survived"])

fig, ax = plt.subplots()
index = range(len(ops))
bar_width = 0.1
opacity = 0.8

rects1 = plt.bar(index, survived, bar_width, alpha=opacity, color="g", label="Survived")

plt.xlabel("Mutation Operators")
plt.ylabel("Number of Mutants")
plt.title("Mutation Operator Statistics")
plt.xticks(index, ops)
plt.legend()

plt.tight_layout()
plt.savefig('plot2.pdf', dpi=300, bbox_inches='tight')

