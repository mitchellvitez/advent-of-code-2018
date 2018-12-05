import os
from time import time

# Compile
for i in range(1, 26):
    i = str(i).zfill(2)
    if os.path.exists(i):
        os.system(f'ghc -O2 {i}')

# Run
total_time = 0
for i in range(1, 26):
    i = str(i).zfill(2)

    if os.path.exists(i):
        print(f'Day {i}')
        start = time()
        os.system(f'./{i}')
        end = time()
        total_time += end - start
        print('{0:.3f} seconds'.format(end - start))
        print()

print('{0:.3f} seconds total'.format(total_time))
