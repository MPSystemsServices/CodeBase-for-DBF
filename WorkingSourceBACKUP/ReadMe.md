# Source Code for 4 Versions of C4DLL.DLL

C4DLL.DLL is the heart of the client/user side of the CodeBase product originally supplied by Sequiter, Inc.  Sequiter also
offered a server application which used the DBF read/write components in C4DLL.DLL to access physical DBF tables under control
of client-side software.

C4DLL.DLL existed in 4 Windows versions targeted to reading/writing Visual FoxPro 3.0 and higher DBF tables:

1. Stand-Alone, 32-Bit: This was the 32-bit version of a DLL which would enable any language module that could access a C language
DLL to call functions to read and write DBF files directly.  This can only be accessed by C or other programs running as 32-bit Windows
applications.

1. Stand-Alone, 64-Bit: The 64-bit version of the version above.  It can only be accessed by C or other programs running as
64-bit Windows applications.

1. Client Server/User, 32-Bit: Similar to #1 above, accessed by a C or other program running as 32-bit Windows applications, but
accesses the target DBF tables by communicating with the CodeBase server application, which must be running and addressable by
IP address and port number.  The function calling signatures are generally the same as for the stand alone versions with
a few additional functions.

1. Client Server/User, 64-Bit: Like #3 above, but accessed only by C or other programs running as 64-bit Windows applications.  Note
that the Server application runs as a 32-bit application when accessed by clients running this module.
## Compiling from Source Code
The source code is the same for ALL of the above versions, except for compiler directives in D4ALL.H which enable/disable various features at 
compile time.  Accordingly, we have put ALL client application source code into one directory, currently:
d:\codebase\WorkingSource

D4ALL.H versions in Working Source also have some compiler directives not in the original Sequiter versions, which provide for more trouble-free compilation by recent versions of Visual Studio C compiler.

In that directory there are 4 directories which contain customized versions of D4ALL.H to produce each of the 4 versions of the
client application with compatibility set to Visual FoxPro:

- HDR_VFP_SERVER_CLIENT_32
- HDR_VFP_SERVER_CLIENT_64
- HDR_VFP_STAND_ALONE_32
- HDR_VFP_STAND_ALONE_64

You need to copy the appropriate D4ALL.H from one of these directories to WorkingSource in order to get the desired version of C4DLL.DLL.  To help with that
we provided 4 .BAT files which will copy the appropriate D4ALL.h into WorkingSource to produce the desired result.  Note
That there are also 4 different build directories which have vcxproj files geared to compilation by Visual Studio 2022,
each producing a different output type.  You must use both the correct build directory AND the correct .BAT file to set
up the appropriate contents of WorkingSource:

- Run: HDR_VFP_SERVER_CLIENT_32.BAT, then use D:\codebase\build\MVStudio_2022_Project_VFP_SERVER_CLIENT_32
- Run: HDR_VFP_SERVER_CLIENT_64.BAT, then use D:\codebase\build\MVStudio_2022_Project_VFP_SERVER_CLIENT_64
- Run: HDR_VFP_STAND_ALONE_32.BAT, then use D:\codebase\build\MVStudio_2022_Project_VFP_STAND_ALONE_32
- Run: HDR_VFP_STAND_ALONE_64.BAT, then use D:\codebase\build\MVStudio_2022_Project_VFP_STAND_ALONE_64

NOTE ON Compilation for versions OTHER THAN Visual FoxPro:
This information ONLY applies to Stand-Alone versions of C4DLL.DLL, NOT to Client/Server versions of C4DLL.DLL.  In Client/Server setups,
it is the server application which must be compiled either for Visual FoxPro OR for Clipper compatibility.  The Client/Server
version of the C4DLL.DLL is agnostic relative to the type of DBF files accessed, as it never accesses the tables directly.
## Running Your Compiled Code
Note carefully the naming conventions for the DLL outputs from the compilation configurations in this repository.  32-bit versions of the DLL
will be named c4dll.dll for both the Stand Alone and the Client/Server versions.  The 64-bit versions for both Stand Alone and Client/Server
versions will be named c4dll64.dll.

The C4DLL.DLL when initialized will be set to default to use of FoxPro 2.5 tables.  The codeBase.compatibility value will default to 25.
If you want to access tables created by FoxPro 2.6, you'll need to set codeBase.compatibility to 26 at initialization time in your application.
To access the full range of field types supported by Visual FoxPro, you'll need to set codeBase.compatibility to 30 before
opening any tables.

If you are using Clipper type indexes (.MDX or .NDX) this compile will NOT read or write to the indexes correctly.  You'll
need to set the appropriate values in D4ALL.H to support Clipper indexes, and that resulting compiled version of C4DLL.DLL
will NOT properly handle Visual FoxPro tables with .CDX indexes.


