import glob
import subprocess
import json 

for i in [0, 1, 2, 3, 4, 5, 6, 7, 8]:
    run = subprocess.run(["java", "-jar", "flix.jar", "--json", "--Xbenchmark-throughput", "--Xbdd-threshold", str(i)], capture_output=True, text=True)
    data = json.loads(run.stdout)
    throughput = data["throughput"]["avg"]
    print(f'{int(i):,}', end='')
    print(f' & {int(throughput):,}', end='')
    print(" \\\\")

