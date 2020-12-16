from DemoModule import delphi_funcs
from timeit import Timer

def count_primes(max_n):
    is_prime = delphi_funcs.is_prime
    res = 0
    for i in range(2, max_n + 1):
        if is_prime(i):
            res += 1
    return res

def test():
    max_n = 1000000
    print(f'Number of primes between 0 and {max_n} = {count_primes(max_n)}')

def main():
    print(f'Elapsed time: {Timer(stmt=test).timeit(1)} secs')

if __name__ == '__main__':
    main()

