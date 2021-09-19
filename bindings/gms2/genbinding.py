#!/usr/bin/env python

import sys
import os
import re

declStrings = []
bindStrings = []

x3d_path = '../../source/xtreme3d'
output_path = '/'

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

#declStrings.append("var dll = \"xtreme3d.dll\";");

definitions = []
functions = []

for filename in os.listdir(directory):
    path = directory + '/' + filename
    if os.path.isdir(path):
        continue
    
    print(filename)
    definitions.append("\t// " + filename)

    f = open(path, "r")
    lines = f.readlines()
    for line in lines:
        if line.startswith("function"):
            tokens = re.findall(r"[\w']+", line)

            callConv = tokens[-1]
            if callConv == "cdecl" or callConv == "stdcall":
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
                
                definition = "\tglobal._" + name + " = external_define(dll, \"" + name + "\", dll_" + callConv + ", ";
                if retType == "real":
                    definition += "ty_real, ";
                elif retType == "pchar":
                    definition += "ty_string, ";
                definition += str(len(args));
                if (len(args) > 0):
                    definition += ", "
                for i, a in enumerate(args):
                    if a == "real":
                        definition += "ty_real"
                    elif a == "pchar":
                        definition += "ty_string"
                    if (i < len(args)-1):
                        definition += ", "
                definition += ");";
                definitions.append(definition);
                
                wrapperFunctionHeader = "function " + name + "(";
                for i, a in enumerate(argNames):
                    wrapperFunctionHeader += a
                    if (i < len(argNames)-1):
                        wrapperFunctionHeader += ", "
                wrapperFunctionHeader += ") {";
                functions.append(wrapperFunctionHeader);
                
                wrapperFunctionBody = "\treturn external_call(global._" + name;
                if (len(argNames) > 0):
                    wrapperFunctionBody += ", "
                for i, a in enumerate(argNames):
                    wrapperFunctionBody += a
                    if (i < len(argNames)-1):
                        wrapperFunctionBody += ", "
                wrapperFunctionBody += ");";
                functions.append(wrapperFunctionBody);
                functions.append("}");
                functions.append("");
        
declStrings.append("function dll_init(dll) {");
declStrings += definitions
declStrings.append("}")
declStrings.append("")
declStrings += functions

with open('constants.txt', 'r') as file:
    constants = file.read()

bindingFile = open('xtreme3d.gml', 'w')
for s in declStrings:
    bindingFile.write("%s\n" % s)
bindingFile.write("%s\n" % constants)
