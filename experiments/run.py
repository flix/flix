import glob
import subprocess

ITERATIONS = 1

for pkg in glob.glob("*.fpkg"):
    print(pkg)

    # Baseline
    run1 = subprocess.run(["java", "-jar", "flix.jar", "--Xno-optimizer", "--Xno-reifyeff", pkg], capture_output=True, text=True)
    lines1, time1, totalBytes1, totalClasses1 = run1.stdout.split(",")
    
    # Extended
    run2 = subprocess.run(["java", "-jar", "flix.jar", "--Xno-optimizer", pkg], capture_output=True, text=True)
    lines2, time2, totalBytes2, totalClasses2 = run2.stdout.split(",")

    classRatio = round((100 * int(totalClasses2)) / int(totalClasses1))

    print(f'Lines: {int(lines1)}, Classes: {int(totalClasses1)} / {int(totalClasses2)} ({classRatio}%)')
    print()
    print("---------")
    print()
