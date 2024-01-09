# Functional Programming 2
## Цель

Освоиться с построением пользовательских типов данных, полиморфизмом, рекурсивными алгоритмами и средствами тестирования (unit testing, property-based testing).

## Требования:

1. Функции:
* добавление и удаление элементов;
* фильтрация;
* отображение (map);
* свертки (левая и правая);
* структура должна быть моноидом.
2. Структуры данных должны быть неизменяемыми.
3. Библиотека должна быть протестирована в рамках unit testing.
4. Библиотека должна быть протестирована в рамках property-based тестирования (как минимум 3 свойства, включая свойства моноида).
5. Структура должна быть полиморфной.
6. Требуется использовать идиоматичный для технологии стиль программирования. Примечание: некоторые языки позволяют получить большую часть API через реализацию небольшого интерфейса. Так как лабораторная работа про ФП, а не про экосистему языка -- необходимо реализовать их вручную и по возможности -- обеспечить совместимость.

## Описание реализации:

Дерево хранится в структуре вида: `(left right key value)` где первые два элемента это другие узлы деревы или nill если там пусто, далее ключ словаря и последним значение. 

#### Описание фукнций:

Функция вставки в дерево нового значения. Также может использоваться для создания нового дерева, тогда `node` должно быть `nil`

```
insert_tree (node key value)
```

Функция поиска значения по ключу

```
get_value_tree (node key)
```

Функция установки значения по ключу

```
set_value_tree (node key value)
```

Функция удаления значения по ключу

```
remove_tree (node key)
```

Функция фильтрации, ожидает на вход функцию вида `(key value) -> {nill/t}`

```
filter_tree (node fun)
```

Функция map, ожидает на вход функцию вида `(value) -> new value`

```
map_tree (node fun)
```

Функция левосторонней свертки, ожидает на вход функцию вида `(value1 value2) -> result`

```
reduce_tree_left (node fun zero)
```

Функция правосторонней свертки, ожидает на вход функцию вида `(value1 value2) -> result`

```
reduce_tree_right (node fun zero)
```

Функция проверки существует ли ключ в дереве

```
is_key_in_tree (node key)
```

Функция объеденения двух деревьев

```
summ_tree (node1 node2)
```

## Тесты:

Тесты делятся на 2 вида unit и property-based. Первые тестируют отдельные функции, а вторые свойства. ПРотестированные свойства: свойства монойда (существование нейтрального элемента), полиморфизм значений, полиморфизм ключей

## Вывод:

В ходе выполнения лабораторной работы, были написаны операции для работы с AVL словарем, и разработаны тесты для его тестирования. В ходе работы была построена структура данных, что оказалось не очень тревиальной задачей в функциональном программировании, и как мне кажется работать с привычнми структурами куда быстрее и проще. Также подкреплены навыки написания рекурсивных функций, функций высшего порядка. Об этих инструментах у меня сложилось более благоприятное впечатление. Без них некоторые достаточно удобные конструкции были бы невозможны.
