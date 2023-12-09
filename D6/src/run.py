f = open ("./../data/input.txt", "r")
lines = f.read().split("\n")

times = list(map(int, filter(lambda x: x != "", lines[0].split(" ")[1:])))
distances = list(map(int, filter(lambda x: x != "", lines[1].split(" ")[1:])))
races = list(zip(times, distances))

result = [len([res for x in range(1,t) if (res := (t-x)*x) > d]) for t,d in races]

product = 1
for r in result:
    product *= r

print(product)