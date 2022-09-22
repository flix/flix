import glob
import subprocess

ITERATIONS = 2

def average(lst):
    return sum(lst) / len(lst)

for pkg in glob.glob("*.fpkg"):
    print(pkg, end='')

    # Baseline
    times1 = []
    for i in range(ITERATIONS):
        run1 = subprocess.run(["java", "-jar", "flix.jar", "--Xno-optimizer", "--Xno-reifyeff", pkg], capture_output=True, text=True)
        _, time1, _, _ = run1.stdout.split(",")
        times1.append(int(time1))
    lines1, _, totalBytes1, totalClasses1 = run1.stdout.split(",")
    
    # Extended
    times2 = []
    for i in range(ITERATIONS):
        run2 = subprocess.run(["java", "-jar", "flix.jar", "--Xno-optimizer", pkg], capture_output=True, text=True)
        _, time2, _, _ = run1.stdout.split(",")
        times2.append(int(time2))
    lines2, time2, totalBytes2, totalClasses2 = run2.stdout.split(",")

    avgTime1 = average(times1)
    avgTime2 = average(times2)
    classRatio = round((100 * int(totalClasses2)) / int(totalClasses1))
    bytesRatio = round((100 * int(totalBytes2)) / int(totalBytes1))
    timeRatio = round((100 * avgTime1) / avgTime2)

    print(f' Lines: {int(lines1)}', end='')
    print(f' Classes: {int(totalClasses1)} / {int(totalClasses2)} ({classRatio}%)', end='')
    print(f' Bytes: {int(totalBytes1)} / {int(totalBytes2)} ({bytesRatio}%)', end='')
    print(f' Time: {int(avgTime1)} / {int(avgTime2)} ({timeRatio}%)', end='')
    print()
