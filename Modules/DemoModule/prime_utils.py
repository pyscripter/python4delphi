# prime_utils.py
from DemoModule import is_prime

def count_primes_in_range(arange):
    return sum(1 for n in range(arange[0], arange[1] + 1) if is_prime(n))
