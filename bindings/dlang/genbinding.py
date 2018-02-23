import sys
import os
import re

declStrings = []
bindStrings = []

directory = sys.argv[1]
for filename in os.listdir(directory):
    path = directory + '/' + filename
    if os.path.isdir(path):
        continue

    f = open(path)
    lines = f.readlines()
    for line in lines:
        if line.startswith("function"):
            tokens = re.findall(r"[\w']+", line)
            callConv = tokens[-1]
            if callConv == "stdcall":
                name = tokens[1]
                retType = tokens[-2]
                numArgTokens = len(tokens) - 4
                args = []
                numArgsOfOneType = 0
                for t in tokens[2:2+numArgTokens]:
                    if (t == "real") or (t == "pchar"):
                        for i in range(0, numArgsOfOneType):
                            args.append(t)
                        numArgsOfOneType = 0
                    else:
                        numArgsOfOneType += 1
                
                declStr = "_x3d_func"
                if len(args) == 0:
                    declStr += '0'
                else:
                    for a in args:
                        if a == "real":
                            declStr += 'D'
                        elif a == "pchar":
                            declStr += 'P'
                if retType == "real":
                    declStr += '_D';
                elif retType == "pchar":
                    declStr += '_P';
                declStr += ' ' + name
                declStr += ';'
                declStrings.append(declStr)
                
                bindStr = "bindFunc(cast(void**)&%s, \"%s\");" % (name, name)
                bindStrings.append(bindStr)
    declStrings.append("")
    bindStrings.append("")
                
for s in declStrings:
    print s
        
print " "
            
for s in bindStrings:
    print s
