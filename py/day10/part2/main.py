from z3 import *
import re

line_regex = "^\\[([\\.#]+)\\] ([\\d,\\(\\) ]+) \\{([\\d,]+)\\}$"
p = re.compile(line_regex)


def min_button_presses(**kwargs):
    buttons = kwargs["buttons"]
    targets = kwargs["targets"]
    A = [Int("a%s" % i) for i in range(len(buttons))]
    X = [
        targets[i] == sum([A[j] for j in range(len(buttons)) if i in buttons[j]])
        for i in range(len(targets))
    ]
    nonNeg = [a >= 0 for a in A]
    summation = sum(A)

    s = Optimize()
    s.add(X)
    s.add(nonNeg)
    s.minimize(summation)

    s.check()
    m = s.model()
    return sum([m[a].as_long() for a in A])


def parse_line(line):
    m = p.match(line)
    switches, joltage = m.group(2, 3)

    buttons = [[int(y) for y in x[1:-1].split(",")] for x in switches.split()]
    targets = [int(a) for a in joltage.split(",")]

    return {"buttons": buttons, "targets": targets}


with open("input.txt") as file:
    print(sum([min_button_presses(**parse_line(line)) for line in file]))
