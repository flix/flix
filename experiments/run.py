import glob
import subprocess

ITERATIONS = 1

for pkg in glob.glob("*.fpkg"):
    print(pkg)
    for i in range(ITERATIONS):
        run = subprocess.run(["java", "-jar", "flix-custom.jar", pkg])
    for i in range(ITERATIONS):
        run = subprocess.run(["java", "-jar", "flix-custom.jar", "--Xno-reifyeff", pkg])
    print()
    print()
    print("---------")
    print()
    print()
