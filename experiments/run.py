import glob
import subprocess
import json 

for i in [0, 1, 2, 3, 4, 5, 6, 1000]:
    run = subprocess.run(["java", "-jar", "flix.jar", "--json", "--Xbenchmark-throughput", "--Xbdd-threshold", str(i)], capture_output=True, text=True)
    data = json.loads(run.stdout)
    minimum = data["throughput"]["min"]
    maximum = data["throughput"]["max"]
    average = data["throughput"]["avg"]
    median = data["throughput"]["median"]
    
    print(f'{int(i):,}', end='')
    print(f' & {int(minimum):,}', end='')
    print(f' & {int(maximum):,}', end='')
    print(f' & {int(average):,}', end='')
    print(f' & {int(median):,}', end='')
    print(" \\\\")
