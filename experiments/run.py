import glob
import subprocess

STDLIB_LINES = 33689
ITERATIONS = 1

def average(lst):
    return float(sum(lst)) / float(len(lst))

def sec(x):
    return round(float(x) / 1_000_000_000, 1)

for pkg in glob.glob("*.fpkg"):
    print(pkg.replace(".fpkg", ""), end='')

    # Baseline
    times1 = []
    for i in range(ITERATIONS):
        run1 = subprocess.run(["java", "-jar", "flix.jar", "--Xno-optimizer", "--Xno-reifyeff", pkg], capture_output=True, text=True)
        _, time1, _, _ = run1.stdout.split(",")
        times1.append(int(time1))
    lines1, _, totalBytes1, totalClasses1 = run1.stdout.split(",")
    lines1 = int(lines1) - STDLIB_LINES

    #print(f' & {int(lines1):,}', end='')
    #print(f' & {avgTime1}s', end='')
    #print(f' & {int(totalBytes1):,}', end='')
    #print(f' & {int(totalClasses1):,}', end='')
    #print(f' & {avgTime2}s (+{timeRatio}\%)', end='')
    #print(f' & {int(totalBytes2):,} (+{bytesRatio}\%)', end='')
    #print(f' & {int(totalClasses2):,} (+{classRatio}\%)', end='')
    #print(" \\\\")
