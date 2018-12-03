import os
from time import time

# Compile
for i in range(1, 26):
    i = str(i).zfill(2)
    if os.path.exists(i):
        os.system(f'ghc -O2 {i}')

# Run
total_start = time()
for i in range(1, 26):
    i = str(i).zfill(2)

    if os.path.exists(i):
        print(f'Day {i}')
        start = time()
        os.system(f'./{i}')
        print('{0:.3f} seconds'.format(time() - start))
        print()

print('{0:.3f} seconds total'.format(time() - total_start))
