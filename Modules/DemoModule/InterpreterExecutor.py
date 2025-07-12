#-------------------------------------------------------------------------------
# Name:        InterpreterExecutor.py
# Purpose:     Showcases the use of extension modules created with Delphi
#              with the new in Python 3.14 InterpreterPoolExecutor
#              You need python 3.14 to run this demo
#              It uses the support module prime_utils which imports
#              the delphi created extension module.
#              Note that each interpreters has its own GIL and
#              they are all running in parallel.
#-------------------------------------------------------------------------------

from concurrent.futures import InterpreterPoolExecutor
from prime_utils import count_primes_in_range
import time

def count_primes(max_num, num_interpreters=4):
    chunk_size = max_num // num_interpreters
    ranges = [(i, min(i + chunk_size - 1, max_num)) for i in range(2, max_num + 1, chunk_size)]
    print(ranges)

    total = 0
    with InterpreterPoolExecutor(max_workers=num_interpreters) as executor:
        results = executor.map(count_primes_in_range, ranges)
        total = sum(results)

    return total

if __name__ == "__main__":
    max_number = 1_000_000
    start_time = time.time()
    prime_count = count_primes(max_number)
    end_time = time.time()

    print(f"Count of prime numbers up to {max_number}: {prime_count}")
    print(f"Time taken: {end_time - start_time:.2f} seconds")