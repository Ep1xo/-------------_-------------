from collections.abc import Iterable


# Завдання 1. Генератор парних чисел у діапазоні
def even_in_range(start: int, end: int):
    """Генерує парні числа в діапазоні [start, end] включно."""
    for x in range(start, end + 1):
        if x % 2 == 0:
            yield x

print("Завдання 1:")
print(list(even_in_range(1, 10)))
for n in even_in_range(2, 8):
    print(n, end=" ")
print("\n")

# Завдання 2. Ітератор по списку з пропуском елементів
class SkipIterator:
    def __init__(self, data: list, step: int):
        self.data = data
        self.step = max(1, step)
        self.index = 0

    def __iter__(self):
        return self

    def __next__(self):
        if self.index >= len(self.data):
            raise StopIteration
        value = self.data[self.index]
        self.index += self.step
        return value

print("Завдання 2:")
it = SkipIterator([10, 20, 30, 40, 50], step=2)
for x in it:
    print(x, end=" ")
print("\n")

# Завдання 3. Генератор підрядків рядка
def substrings(s: str, k: int):
    """Генерує всі підрядки довжини k з рядка s."""
    if k <= 0:
        return
    n = len(s)
    for i in range(0, n - k + 1):
        yield s[i:i + k]

print("Завдання 3:")
for sub in substrings("abcdef", 3):
    print(sub)
print()

# Завдання 4. Перетворення списку кортежів у словник
def aggregate_pairs(pairs: list[tuple[str, int]]) -> dict[str, int]:
    result: dict[str, int] = {}
    for key, value in pairs:
        if key in result:
            result[key] += value
        else:
            result[key] = value
    return result

print("Завдання 4:")
pairs = [("apple", 3), ("banana", 5), ("apple", 2), ("orange", 4)]
print(aggregate_pairs(pairs))
print()

# Завдання 5. Генератор для плаского обходу вкладених списків
def flatten(data: list[object]):
    """Плоский обхід вкладених списків чисел."""
    for item in data:
        if isinstance(item, list):
            yield from flatten(item)
        else:
            yield item

print("Завдання 5:")
data = [1, [2, 3], [4, [5, 6]], 7]
print(list(flatten(data)))
print()

# Завдання 6. Перетворення словника в список кортежів та навпаки
def dict_to_tuples(d: dict) -> list[tuple]:
    return list(d.items())

def tuples_to_dict(pairs: list[tuple]) -> dict:
    result: dict = {}
    for k, v in pairs:
        result[k] = v
    return result

print("Завдання 6:")
d = {"a": 1, "b": 2, "c": 3}
tups = dict_to_tuples(d)
print(tups)
print(tuples_to_dict(tups))
print()

# Завдання 7. Фільтрація словника за умовою
def filter_dict(d: dict[str, int], threshold: int) -> dict[str, int]:
    return {k: v for k, v in d.items() if v > threshold}

print("Завдання 7:")
products = {
    "apple": 25,
    "banana": 40,
    "orange": 30,
    "kiwi": 55,
}
print(filter_dict(products, 35))
print()

# Завдання 8. Генератор комбінованих даних зі списку і словника
def name_score_pairs(names: list[str], scores: dict[str, int]):
    """
    Генерує пари (name, score) для імен зі списку.
    Якщо ім'я відсутнє в словнику оцінок — пропускається.
    """
    for name in names:
        if name in scores:
            yield (name, scores[name])

print("Завдання 8:")
names = ["Іван", "Ольга", "Богдан", "Хтось"]
scores = {"Іван": 90, "Ольга": 85, "Богдан": 78}
print(list(name_score_pairs(names, scores)))
print()

# Завдання 9. - Ітератор по словнику з фільтрацією ключів
class KeyFilterIterator:
    def __init__(self, d: dict, allowed_keys: Iterable):
        self.d = d
        self.allowed = set(allowed_keys)
        self._iter = iter(d.items())

    def __iter__(self):
        return self

    def __next__(self):
        while True:
            key, value = next(self._iter)  
            if key in self.allowed:
                return (key, value)

print("Завдання 9:")
d = {"a": 1, "b": 2, "c": 3}
allowed = {"b", "c", "x"}
it = KeyFilterIterator(d, allowed)
for kv in it:
    print(kv)
print()

# Завдання 10 Генератор статистики по списку чисел
def running_stats(numbers: list[float]):
    """
    Після кожного числа генерує (count, current_sum, average).
    """
    count = 0
    total = 0.0
    for x in numbers:
        count += 1
        total += x
        avg = total / count
        yield (count, total, avg)

print("Завдання 10:")
for st in running_stats([10, 20, 30]):
    print(st)
