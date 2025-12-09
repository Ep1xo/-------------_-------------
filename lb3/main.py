# Завдання 2. Незмінне оновлення словника (Python)
def update_dict(original: dict, key, value) -> dict:
    new_dict = original.copy()
    new_dict[key] = value
    return new_dict

# Завдання 3. Незмінне додавання до кортежу (Python)
def append_tuple(tpl: tuple, item) -> tuple:
    return tpl + (item,)

# Завдання 5. Імітація push для списку з незмінністю (Python)
def push(stack: list, item) -> list:
    return [item] + stack

print(update_dict({'a': 1, 'b': 2}, 'b', 99))
print(update_dict({'x': 10}, 'y', 5))

print(append_tuple((1, 2, 3), 4))
print(append_tuple((), 99))

print(push([3, 2, 1], 4))
print(push([], 10))