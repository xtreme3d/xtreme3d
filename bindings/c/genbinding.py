#!/usr/bin/env python

import sys
import os
import re

declStrings = []
defStrings = []

x3d_path = '../../source/xtreme3d'
output_path = 'include'

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

declStrings.append("/* Xtreme3D header */")
declStrings.append("")
declStrings.append("#ifndef XTREME3D_H")
declStrings.append("#define XTREME3D_H")
declStrings.append("")
declStrings.append("#include <xtreme3d_constants.h>")
declStrings.append("#include <xtreme3d_color_codes.h>")
declStrings.append("#include <xtreme3d_input_codes.h>")
declStrings.append("")
declStrings.append("#ifdef __cplusplus")
declStrings.append("extern \"C\" {")
declStrings.append("#endif")
declStrings.append("")

defStrings.append("LIBRARY xtreme3d")
defStrings.append("EXPORTS")

for filename in os.listdir(directory):
    path = directory + '/' + filename
    if os.path.isdir(path):
        continue

    print(path)
    declStrings.append("/* " + filename + " */")

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
                
                funcDecl = "__declspec(dllimport) ";
                
                if retType == "real":
                    funcDecl += "double ";
                elif retType == "pansichar":
                    funcDecl += "char* ";
                
                funcDecl += name;
                
                if len(args) == 0:
                    funcDecl += "(void)"
                else:
                    funcDecl += "("
                    for i, a in enumerate(args):
                        if a == "real":
                            funcDecl += "double"
                        elif a == "pansichar":
                            funcDecl += "char*"
                        if (i < len(args)-1):
                            funcDecl += ", "
                    funcDecl += ")"
                funcDecl += ";"
                
                declStrings.append(funcDecl)
                
                defStrings.append("    " + name)

    declStrings.append("")

declStrings.append("#ifdef __cplusplus")
declStrings.append("}")
declStrings.append("#endif")
declStrings.append("")
declStrings.append("#endif /* XTREME3D_H */")

bindingFile = open(outputDirectory + '/xtreme3d.h', 'w')
for s in declStrings:
    bindingFile.write("%s\n" % s)

defFile = open('xtreme3d.def', 'w')
for s in defStrings:
    defFile.write("%s\n" % s)
