import re
import glob


files = glob.glob("*lm*.tex")
for fi in files:
    out = []
    with open(fi, 'r') as f:
        for s in f:
            if s.startswith(' country'):
                next(f) # skip two line
                next(f)
                continue
            out.append(s)

    with open(fi, 'w') as f:
        for s in out:
            f.write(s)

## Can define here modification (e.g. lpop -> Log of working population)
