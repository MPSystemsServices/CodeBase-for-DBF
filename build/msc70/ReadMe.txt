The files in this directory are the result of a successful import and compile of the CodeBase source
project and code files into C4dll.dll.  Compilation was done with Visual Studio 2010 C/C++ compiler.
The result c4dll.dll runs correctly with the CodeBaseWrapper.dll compiled with VS 2010 as well.

The end product works correctly with Python 2.6 and CodeBaseTools.py.

This establishes that the CodeBase product can be rebuilt, if needed, by VS2010.

Key to a successful build was including zlib.dll and zlib.lib in the project.

IT IS RECOMMENDED THAT YOU MAKE USE OF MORE RECENT VERSIONS OF Microsoft Visual Studio.
The build directory contains vcxproj files configured to work with VS 2022.  See the
SourceCodeReadMe_Sept_2023.md file for full details on compilation with modern VS versions.

M-P Systems Services, Inc.
February, 2026
