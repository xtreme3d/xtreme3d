#!/usr/bin/env python

import sys
import os
import re

declStrings = []
defStrings = []

x3d_path = '../source/xtreme3d'
output_path = 'markdown/ru/functions'

directory = ''
if len(sys.argv) < 2:
    directory = x3d_path
else:
    directory = sys.argv[1]

if not os.path.exists(directory):
    sys.exit('Error: input directory \"' + directory + '\" does not exist')

if len(sys.argv) < 3:
    outputDirectory = output_path
else:
    outputDirectory = sys.argv[2]

if not os.path.exists(outputDirectory):
    sys.exit('Error: output directory \"' + outputDirectory + '\" does not exist')

declStrings.append("# Список функций")
declStrings.append("")

for filename in os.listdir(directory):
    path = directory + '/' + filename
    if os.path.isdir(path):
        continue

    print(path)
    htmlFilename = os.path.splitext(filename)[0] + ".html"

    f = open(path)
    lines = f.readlines()
    for line in lines:
        if line.startswith("function"):
            tokens = re.findall(r"[\w']+", line)

            callConv = tokens[-1]
            if callConv == "cdecl":
                name = tokens[1]
                retType = tokens[-2].casefold()
                numArgTokens = len(tokens) - 4
                args = []
                argNames = []
                numArgsOfOneType = 0
                for t in tokens[2:2+numArgTokens]:
                    token = t.casefold()
                    if (token == "real") or (token == "pansichar"):
                        for i in range(0, numArgsOfOneType):
                            args.append(token)
                        numArgsOfOneType = 0
                    else:
                        numArgsOfOneType += 1
                        argName = "a" + t.title()
                        argNames.append(argName)
                
                funcDecl = "[%s](%s#%s)<br>" % (name, htmlFilename, name.lower())
                
                declStrings.append(funcDecl)

    declStrings.append("")

funcsListFile = open(outputDirectory + "/funclist.md", "w", encoding="utf-8")
for s in declStrings:
    funcsListFile.write("%s\n" % s)
