from typing import Iterable, Callable
from functools import reduce, wraps
import time

def log_calls(func: Callable) -> Callable:
    @wraps(func)
    def wrapper(*args, **kwargs):
        args_repr = ", ".join(map(str, args))
        kwargs_repr = ", ".join(f"{k}={v}" for k, v in kwargs.items())
        all_args = ", ".join(filter(None, [args_repr, kwargs_repr]))
        print(f"→ {func.__name__}({all_args})")
        result = func(*args, **kwargs)
        print(f"← {func.__name__} = {result}")
        return result
    return wrapper

@log_calls
def add(a: int, b: int) -> int:
    return a + b


print(add(2, 3))

def timeit(func: Callable) -> Callable:
    @wraps(func)
    def wrapper(*args, **kwargs):
        start = time.time()
        result = func(*args, **kwargs)
        ms = (time.time() - start) * 1000
        print(f"{func.__name__} took {ms:.0f} ms")
        return result
    return wrapper

@timeit
def slow(n: int) -> int:
    time.sleep(0.05)
    return n * n

print(slow(10))

def is_strong_password(s: str) -> bool:
    return (
        len(s) >= 8
        and any(c.isupper() for c in s)
        and any(c.islower() for c in s)
        and any(c.isdigit() for c in s)
        and any(not c.isalnum() for c in s)
    )

print(is_strong_password("Qwerty12!"))
print(is_strong_password("qwerty12"))

def all_close(xs: Iterable[float], tol: float = 1e-6) -> bool:
    xs = list(xs)
    if not xs:
        return True
    first = xs[0]
    return all(abs(x - first) <= tol for x in xs)


print(all_close([1.0, 1.0000005, 0.9999999], tol=1e-5))
print(all_close([1.0, 1.1], tol=1e-3))

def has_prime(nums: Iterable[int]) -> bool:
    def is_prime(n: int) -> bool:
        if n <= 1:
            return False
        if n == 2:
            return True
        if n % 2 == 0:
            return False
        i = 3
        while i * i <= n:
            if n % i == 0:
                return False
            i += 2
        return True

    return any(is_prime(x) for x in nums)

print(has_prime([4, 6, 8, 9]))
print(has_prime([4, 6, 7, 9, 10]))

def scale_and_shift(xs: Iterable[float], scale: float, shift: float) -> list[float]:
    return list(map(lambda x: scale * x + shift, xs))


print(scale_and_shift([1, 2, 3], 2.0, -1.0))

def filter_emails(items: Iterable[str]) -> list[str]:
    def is_email(s: str) -> bool:
        if "@" not in s:
            return False
        local, _, domain = s.partition("@")
        return bool(local) and "." in domain

    return list(filter(is_email, items))

emails = ["a@b.com", "wrong@", "x@y", "john.doe@mail.org"]
print(filter_emails(emails))

def char_hist(s: str) -> dict[str, int]:
    return reduce(
        lambda acc, ch: {**acc, ch: acc.get(ch, 0) + 1},
        s,
        {},
    )

print(char_hist("aba c"))

def top_k(students: list[dict[str, int | str]], k: int) -> list[str]:
    sorted_students = sorted(
        students,
        key=lambda st: (-int(st["score"]), str(st["name"])),
    )
    return [str(st["name"]) for st in sorted_students[:k]]


students = [{"name": "Ann", "score": 90},
            {"name": "Bob", "score": 95},
            {"name": "Ada", "score": 95}]
print(top_k(students, 2))

def minmax_scale(xs: list[float]) -> list[float]:
    if not xs:
        return []
    mn, mx = min(xs), max(xs)
    if mn == mx:
        return [0.0] * len(xs)
    return list(map(lambda x: (x - mn) / (mx - mn), xs))

print(minmax_scale([10, 20, 30]))
print(minmax_scale([5, 5, 5]))
