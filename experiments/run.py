import glob
import subprocess

ITERATIONS = 1

for pkg in glob.glob("*.fpkg"):
    print(pkg, end='')

    # Baseline
    run1 = subprocess.run(["java", "-jar", "flix.jar", "--Xno-optimizer", "--Xno-reifyeff", pkg], capture_output=True, text=True)
    lines1, time1, totalBytes1, totalClasses1 = run1.stdout.split(",")
    
    # Extended
    run2 = subprocess.run(["java", "-jar", "flix.jar", "--Xno-optimizer", pkg], capture_output=True, text=True)
    lines2, time2, totalBytes2, totalClasses2 = run2.stdout.split(",")

    classRatio = round((100 * int(totalClasses2)) / int(totalClasses1))
    bytesRatio = round((100 * int(totalBytes2)) / int(totalBytes1))

    print(f' Lines: {int(lines1)}', end='')
    print(f' Classes: {int(totalClasses1)} / {int(totalClasses2)} ({classRatio}%)', end='')
    print(f' Bytes: {int(totalBytes1)} / {int(totalBytes2)} ({bytesRatio}%)', end='')
    print()
