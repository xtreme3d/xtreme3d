#!/usr/bin/env python

import sys
import os
import re

declStrings = []
bindStrings = []

x3d_path = '../../source/xtreme3d'
output_path = 'xtreme3d'

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

declStrings.append("import ctypes")
declStrings.append("")
declStrings.append("x3d = ctypes.CDLL(\'./xtreme3d.dll\')")
declStrings.append("")

for filename in os.listdir(directory):
    path = directory + '/' + filename
    if os.path.isdir(path):
        continue

    print(path)
    declStrings.append("# " + filename)

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
                
                argTypes = "x3d." + name + ".argtypes" + " = "
                resType = "x3d." + name + ".restype" + " = "

                if len(args) == 0:
                    argTypes += "[]"
                else:
                    argTypes += "["
                    for i, a in enumerate(args):
                        if a == "real":
                            argTypes += "ctypes.c_double"
                        elif a == "pansichar":
                            argTypes += "ctypes.c_char_p"
                        if (i < len(args)-1):
                            argTypes += ", "
                    argTypes += "]"

                if retType == "real":
                    resType += "ctypes.c_double";
                elif retType == "pansichar":
                    resType += "ctypes.c_char_p";

                declStrings.append(argTypes)
                declStrings.append(resType)

                argsString = ""
                for i, a in enumerate(argNames):
                    argsString += a
                    if (i < len(argNames)-1):
                        argsString += ", "
                funcHeader = "def %s(%s):" % (name, argsString)
                funcBody =   "    return x3d.%s(%s)" % (name, argsString)
                declStrings.append(funcHeader)
                declStrings.append(funcBody)
                declStrings.append("")

    declStrings.append("")

bindingFile = open(outputDirectory + '/x3dfuncs.py', 'w')
for s in declStrings:
    bindingFile.write("%s\n" % s)
