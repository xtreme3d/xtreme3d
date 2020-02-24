#!/usr/bin/env python

import sys
import os
import re

declStrings = []
bindStrings = []

x3d_path = '../../source/xtreme3d'
output_path = './'

directory = ''
if len(sys.argv) < 2:
    directory = x3d_path
else:
    directory = sys.argv[1]

if not os.path.exists(directory):
    sys.exit('Error: input directory \"' + directory + '\" does not exist')

outputDirectory = ''
if len(sys.argv) < 3:
    outputDirectory = output_path
else:
    outputDirectory = sys.argv[2]

if not os.path.exists(outputDirectory):
    sys.exit('Error: output directory \"' + outputDirectory + '\" does not exist')

declStrings.append("local ffi = require(\"ffi\")")
declStrings.append("local utils = require(\"utils\")")
declStrings.append("")
declStrings.append("ffi.cdef[[")


for filename in os.listdir(directory):
    path = directory + '/' + filename
    if os.path.isdir(path):
        continue
    
    declStrings.append("    // " + filename)
    
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
                argNames = []
                numArgsOfOneType = 0
                for t in tokens[2:2+numArgTokens]:
                    if (t == "real") or (t == "pchar"):
                        for i in range(0, numArgsOfOneType):
                            args.append(t)
                        numArgsOfOneType = 0
                    else:
                        numArgsOfOneType += 1
                        argName = "a" + t.title()
                        argNames.append(argName)
                
                argTypes = ""
                resType = ""
                
                for i, a in enumerate(args):
                    if a == "real":
                        argTypes += "double"
                    elif a == "pchar":
                        argTypes += "char*"
                    if (i < len(args)-1):
                        argTypes += ", "
                
                if retType == "real":
                    resType += "double";
                elif retType == "pchar":
                    resType += "char*";
                
                declStrings.append("    %s %s(%s);" % (resType, name, argTypes))

declStrings.append("]]")
declStrings.append("")
declStrings.append("local x3d = ffi.load(\"xtreme3d.dll\")")
declStrings.append("")
declStrings.append("return x3d")
declStrings.append("")

bindingFile = open(outputDirectory + '/x3d.lua', 'w')
for s in declStrings:
    bindingFile.write("%s\n" % s)

