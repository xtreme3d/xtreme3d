#------------------------------------------------------------------------------
# ConsoleSetLibPath.py
#   Initialization script for cx_Freeze which manipulates the path so that the
# directory in which the executable is found is searched for extensions but
# no other directory is searched. The environment variable LD_LIBRARY_PATH is
# manipulated first, however, to ensure that shared libraries found in the
# target directory are found. This requires a restart of the executable because
# the environment variable LD_LIBRARY_PATH is only checked at startup.
#------------------------------------------------------------------------------

import os
import sys
import zipimport

FILE_NAME = sys.executable
DIR_NAME = os.path.dirname(sys.executable)

paths = os.environ.get("LD_LIBRARY_PATH", "").split(os.pathsep)

if DIR_NAME not in paths:
    paths.insert(0, DIR_NAME)
    os.environ["LD_LIBRARY_PATH"] = os.pathsep.join(paths)
    os.execv(sys.executable, sys.argv)

sys.frozen = True
sys.path = sys.path[:4]

os.environ["TCL_LIBRARY"] = os.path.join(DIR_NAME, "tcl")
os.environ["TK_LIBRARY"] = os.path.join(DIR_NAME, "tk")


def run():
    m = __import__("__main__")
    importer = zipimport.zipimporter(os.path.dirname(os.__file__))
    name, ext = os.path.splitext(os.path.basename(os.path.normcase(FILE_NAME)))
    moduleName = "%s__main__" % name
    code = importer.get_code(moduleName)
    exec(code, m.__dict__)
    
    versionInfo = sys.version_info[:3]
    if versionInfo >= (2, 5, 0) and versionInfo <= (2, 6, 4):
        module = sys.modules.get("threading")
        if module is not None:
            module._shutdown()

