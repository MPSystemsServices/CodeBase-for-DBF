# CodeBase-for-DBF
## Copyright and Release to Open Source
CodeBase is copyright Sequiter, Inc., and was released to Open Source by Sequiter, Inc. in 2018.  A PDF containing the agreement to release the software and the applicable GNU Lesser GPL V.3.0 license is included in this repository.  This formerly proprietary software is now available to the larger community for development and refinement.  This repository was created in GitHub by M-P Systems Services, Inc., under an agreement with Sequiter to make their compiled products and source code available to the Open Source community.

This ReadME file and the content of this repository were updated in February, 2026, to include new modules and compiled 64-bit versions of the DLLs in addition to the 32-bit versions previously released.  Also included in this latest update are source code files for the Admin Console and the Server along with additional documentation files and development tools supplied in 2020 by Sequiter.

The files in this repository contain the final commercial version of CodeBase 6.5, Release 3, including the last official bug fix released on January 13, 2015.  The original readme.txt file supplied by Sequiter with its product delivery will be found in the main directory of this repository as <i>readme_Sequitur_Code_Base.txt</i>

## Documentation contents
- [Software Description](#software-description)
- [Why Use CodeBase](#why-use-codebase)
- [Windows and other Platform Options](#windows-and-other-platform-options)
- [Using CodeBase](#using-codebase)
- [Compatibility with Visual FoxPro](#compatibility-with-visual-foxpro)
- [Other Variations of DBF Tables and indexes](#other-variations-of-dbf-tables-and-indexes)
- [CodeBase Server application and the Admin Console](#codebase-server-application-and-the-admin-console)
- [Capacities and Limits for Table access](#capacities-and-limits-for-table-access)
- [Documentation](#documentation)
- [Quick Start](#quick-start)
- [Examples and Demo Programs](#examples-and-demo-programs)
- [Compiling from Source](#compiling-from-source)

## Software Description
CodeBase(tm) is a library of callable routines for reading, writing, indexing, and otherwise handling .DBF type data tables written in C and exposed as a linkable DLL file.  The .DBF format was originally developed by Ashton-Tate in the 1980s as part of their dBase II product, but has been enormously enhanced and extended in the following years.  A number of variants of .DBF tables evolved, the most recent being the Visual FoxPro variations introduced by Microsoft, which greatly expanded the range of field types and introduced the concept of "database containers" which provided capabilities previously found only in high-end SQL databases like SQL Server or PostgreSQL.  CodeBase evolved with the times, and added support for most but not all of the Microsoft enhancements.  Most notably missing is support for the database container concept, although the product can access, update, and reindex .DBF tables contained in a database container that are also being accessed simultaneously by Visual FoxPro applications.

CodeBase also provides support for the high performance .CDX type indexes introduced in Visual ProPro, which provide more flexibility and sophistication in index construction (and high performance data retrieval) than most SQL type databases provide.

Client application tools (Stand Alone client) and Client Server version tools are provided in Windows DLL (Dynamic Link Library) format along with .LIB files used by compilers and linkers to integrate the library with the compiled application.  The Server (supporting Client Server operations) and the Administration Console are stand alone Windows executables (.EXE).

Under the original commercial offering CodeBase consisted of a number of modules that provided both direct (stand alone) and client server access from compiled applications to .DBF tables and their indexes:
- Stand Alone version for 32-bit applications
- Stand Alone version for 64-bit applications
- Client Server version for 32-bit applications
- Client Server version for 64-bit applications
- Server for access by Client Server versions above - compiled to 32-bit executable
- Administration Console application for managing individual DBF tables
- SQL Inquiry ODBC Module accessible by Client Server versions above.

Source code for all of these except for the SQL Inquiry tool is included in this repository.  The SQL tool licensed by Sequiter was obtained from a third party supplier, and Sequiter was not in a legal position to release that code for Open Source access.  Compiled versions of the Adminstration Console for Windows and the Server are included here, as are Sequiter's latest source code for these applications.

To our knowledge, all employees of Sequiter Software who developed and maintained this product have retired, and Sequiter Software is now in a completely different business.  No further support or information will be forthcoming from Sequiter regarding this product.
## Why Use CodeBase
Most open source tools for accessing data require some degree of facility with SQL language and queries.  While leading SQL database systems may offer great sophistication, their complexity is also often overkill for everyday requirements.  Conversely, simpler versions like SQLite have limited functionality in terms of field data types, multi-user access, speed, index construction, record level locking and other useful features which are built in with CodeBase and the DBF table format.
When combined with <b>Python CodeBase Tools</b> in a complementary GitHub repository, which dramatically simplifies the use of CodeBase while supporting the most widely used general programming platform, CodeBase can provide a highly efficient, performant, easy to program, and reliable platform to build analytical tools and medium sized multi-user applications.

CodeBase (and its predecessor Visual FoxPro) supports a "navigational" approach to data management rather than the convoluted SQL relational approach.  This means that once a table is opened, the programmer can traverse the table forward and backward with simple navigational commands, inspect a "current" record, locate a record very quickly by a primary key, complex specialized index key, or record number, and otherwise have immediate and direct access to the records in the table.  Further, it is extremely easy to interrogate a table to find out what its structure is and the nature and composition of the indexes used for rapidly selecting records.  Finally, creating temporary tables, populating them, and later removing them after use are extremely fast and easy with CodeBase making implementation of data "persistence", either temporary or permanent, easy when required by your applications.  And, as you might suppose, there is no need for a "database administrator" to intervene on the programmer's behalf to add fields, create new tables, or otherwise manipulate the data.

For more sophisticated applications, CodeBase provides for defining complex relations between tables (similar to those which can be defined in SQL code) supporting navigation not only through a single table, but also through multiple related tables connected by pre-defined primary and foreign keys.  To support the access, searching, and updating of multiple related tables simultaneously, CodeBase also supports a proprietary transaction tracking tool which ensures that if multiple tables are being updated, that the updates are successfully applied to ALL tables or no changes are applied at all.  This helps ensure data integrity in systems with multiple tables with complex inter-connections.
## Windows and other Platform Options
CodeBase(tm) source code provides compilation switches to support LINUX and WINDOWS compilers.  The compiled versions in THIS repository were based on using the Windows configuration switches and settings for Visual FoxPro table compatibility.  Users needing to use these modules under Linux variants are invited to compile the code with the appropriate #DEFINE settings for their target platform.  The maintainers of this repository are unable to provide support or information on compiling the CodeBase tools for Linux or any other non-Windows platform.

Both 32-bit and 64-bit versions of the Windows compiled DLLs are included in this repository.  Compilation setups are provided to compile the Stand Alone and Client Server versions using Visual Studio 2022, which was used to create the compiled DLL files in this repository.
Before attempting to recompile any of the DLLs, it is important for you to read the contents of the file [ReadMe.md](WorkingSource/ReadMe.md) found in the WorkingSource directory.  See also the section of this readme [Compiling from Source](#compiling-from-source) below for more details.
## Using CodeBase
Any application development language that can access a C DLL compiled for 32-bit or 64-bit operation under Windows should be able to access this dll and the functions contained in it.  This would include 32-bit and 64-bit versions of C, VisualBasic, C#, and many other programming environments. It should be noted, however, that CodeBase in its native form is highly granular, and relatively simple tasks that are accomplished in Visual FoxPro by a single statement or function call, may require many lines of code when calling the c4dll.dll directly.
To address this concern, M-P Systems Services, Inc., has simultaneously released its <b>Python CodeBase Tools</b> which provide a wrapper and set of utilities which greatly simplify data handling with CodeBase, encapsulating its functionality with simple navigational style function calls that mimic or expand on the Visual FoxPro data management functions and commands.

See also the Quick Start section below for more details.
## Compatibility with Visual FoxPro
With compiled DLLs in this repository (see MPSSProductionVersions directory and Quick Start below), applications can use CodeBase to open DBF tables in shared mode simultaneously with VFP applications.  Function calls are provided for record and table locking to provide data protection in such applications.  The CodeBase documentation files (see below) provide detailed information on how Visual FoxPro field types are handled in CodeBase.  Python users may find it helpful to read the Python program files provided in the <b>Python CodeBase Tools</b> repository for more indepth information.  FoxPro supported both .CDX and .IDX format index files for DBF tables, and both of these options are supported by the compiled DLLs; however, the default index name expected when a table is opened has a .CDX extension, but the default index can be changed programmatically to a .IDX file at runtime.
## Other Variations of DBF Tables and Indexes
The CodeBase module source code supports several variants of DBF files including the original dBase IV versions, Clipper versions, FoxPro 2.x versions, and Visual FoxPro versions.  For example Clipper DBF tables often had .NDX or .IDX indexes in their proprietary format, not Visual FoxPro .CDX indexes.  <u>However, the c4dll.dll versions supplied in this distribution were compiled for Visual FoxPro compatibility.</u>  If you need support for Clipper or dBase IV compatibility you can either build the c4dll.dll from the source or contact M-P Systems Services to request the maintainer to do this for you (if/when he has time).  Similarly, dBase IV made use of .MDX index formats, which are NOT supported by the DLLs in this repository. (Note that many dBase IV and Clipper table constructs were actually supported by Visual FoxPro for backwards compatibility and thus may be supported by the VFP version of CodeBase offered here.  We suggest you experiment with compatibility before going to the trouble of a recompile.)
## CodeBase Server application and the Admin Console
Sequiter offered a full client-server version of the application as an extra cost product where client applications would make us of the local client server version of the c4dll.dll or c4dll64.dll CodeBase components to access the s4server.exe application running on the same or connected computer to read and write the DBF tables.  Generally, the functions which are supported by the stand alone version will also work when running with the client-server versions.  The original compiled commercial version of s4server.exe is found in MPSSProductionVersions\Win32ServerApplication.  This version is the one supplied to us by Sequiter. 

<b>In 2020 Sequiter supplied us with the very last versions of their internal development directory for all components except the SQL inquiry tool (see above).</b>  This material is provided without modification by us in the directory CodeBaseServer2020 and its many subdirectories.  Within that is the directory development\cblinux, the role of which we have never explored, but which is provided to the community for further investigation.  Most interesting in this directory are the source files and configuration setups for compiling the Administration Console and the CodeBase Server application: s4server.exe.  We would appreciate hearing of any experiences our users have with this material.

The Getting Started help file (.CHM) in books has extensive information on setting up and configuring a full client/server setup with TCP/IP connections to the server.  The maintainers of this repository have no experience with setting up, configuring, and running a client/server version of CodeBase.  We encourage users to make use of the user messages feature of GitHub to share experiences with this part of the system.
## Capacities and Limits for Table Access
Visual FoxPro was notorious for having a limit of 2GB on the size of its data tables, a limit which often constrains modern data analyses and applications.  CodeBase respects that limit when sharing data tables with Visual FoxPro but has an optional "Large" model mode (selectable programatically during execution) which expands the limit to terabyte dimensions.  Similarly, Visual FoxPro DBF tables have a limit of 255 fields which constrains shared access with VFP applications, but CodeBase by itself is able to access DBF tables with many more fields in both "Standard" and "Large" model modes.  For a complete explanation of limitations, field types, limits imposed by compatibility requirements with VFP, Clipper, and dBase, and so on, you should read the appropriate sections of the CodeBase documentation in the books directory.
## Documentation
See the books directory for help files in compiled Windows help format (CHM).  If you will be using xBase (FoxPro) expressions in your indexes or search expressions, you should read both the Reference Guide and the CBextended help for a complete list of all xBase functions and syntax supported in search expressions and index expressions by CodeBase.  Help files in the books directory include:
- <i>C_Reference_Guide.chm</i> - Function-by-function documentation of all aspects of the CodeBase DLL Module
- <i>C_Users_Guide.chm</i> - Examples of C code and general instructions on how to apply the various discrete functions in CodeBase to accomplish common tasks.
- <i>CBextended.chm</i> - An addendum to the C_Reference_Guide that includes new field types, new dBase-type functions that can be embedded in query strings and index definitions, and comments on their new self-incrementing integer fields.  Note that the CodeBase self-incrementing integer fields are NOT compatible with the comparable integer fields in Visual FoxPro.
- <i>CodeBaseHintsPage.htm</i> - A wide variety of common questions and concerns with detailed solutions.
- <i>CompilingTo64bitDLL.txt</i> - A text file with explanation of how to use the files in the build directory to re-create the CodeBase DLLs in Windows using Visual Studio.
- <i>CPP_Reference_Guide.chm</i> - Comparable to the C_Reference_Guide.chm with C++ examples.
- <i>CPP_User_Guide.chm</i> - Comparable to the C_Users_Guide.chm with C++ examples.
- <i>FAQExamples.htm</i> - Example code linked to by CodeBaseHintsPage.htm
- <i>Getting_Started_Manual.chm</i> - broad overview of setting up and configuring CodeBase both for stand-alone and server operation.
- <i>VB_Reference_Guide.chm</i> - Comparable to the C_Reference_Guide.chm with Visual Basic examples.
- <i>VF_User_Guide.chm</i> - Comparable to the C_Reference_Guide.chm with Visual Basic examples.
## Quick Start
The most recent compiled DLL versions are found in the directory MPSSProductionVersions.  (See above for details of which versions of .DBF and index files are supported by these DLLs.)  The subdirectory names indicate the version of the DLL in each.  A few have a zlib.dll file which must be deployed with the c4dll.dll if present.  The production dll is named c4dll64.dll for the 64-bit versions.  If you are using a compiler that produces a 64-bit executable, you will need the 64-bit version of the CodeBase DLL.  Should you need it, the administration console executable CBAdmin.EXE and its help file are in the subdirectory Win32AdminConsole.  

To implement CodeBase in your C or other application, you should copy the appropriate c4dll.dll or c4dll64.dll to your working development directory along with the associated .lib file.  You will need to consult the documentation for your C or other compiler to determine how to link the DLL file using a reference to the .LIB file.  There are C and C++ examples in the examples\source directory to refer to.  Also the CHM files in the books directory have detailed documentation on every function supplied by the DLL.  Typically you will need to include the master CodeBase include file d4all.h as an #include reference in your C or other code where CodeBase functions and constants are used.

Note that there are 4 versions of the d4all.h found in the WorkingSource directory.  They are found in subdirectories which explicitly define what they are for.  They each make additional #include calls to some of the many other .H files that support use of CodeBase, and you will need to either copy them to your working include directory or make reference to the appropriate path on your local development environment from a clone of this repository.  All required .H files to support your application compile and access to CodeBase functions are found in the WorkingSource subdirectory of this repository.
## Examples and Demo Programs
Sequiter provided users with extensive examples of using the various features of CodeBase.  For C, C++, and Visual Basic examples, see the examples directory and its subdirectory source.  As these examples are quite old, the compilation tools relate to versions of Visual Studio going back to before 2010.  We have made no attempt to update the vcproj files to more recent versions.

Sequiter was justifiably proud of the high performance of its CodeBase product, which is comparable to the lightning fast Visual FoxPro for many data handling functions.  (In our tests, CodeBase c4dll.dll handles functions like appending, copying, querying, and updating tables faster than the single threaded versions of all available SQL database applications.)  Accordingly, they provided a SpeedDemo application which is found in compiled and source code versions in the CodeBaseServer2020\development\SpeedDemo directory.  We have been able to run the cb6demo.exe file to get some results so we can confirm that it does work.  Have fun with it!
## Compiling from Source
For most applications, you'll be able to use the .DLL files found in this repository directly in your application if you are simply interested in compatibility with Visual FoxPro applications on Windows.  However, if you are looking for Clipper compatibility or need to apply custom modifications to the CodeBase product, you will want to do your own compilation.  The maintainers of this repository are Windows programmers and have included MS Visual Studio project files to assist in this recompile for the Windows platform, both 32-bit and 64-bit.  We are open to posting materials provided by others to support recompilation for Linux and its variants.

### Compiling for Windows
The maintainers have been able to compile the 4 core Windows DLL libraries configured for Visual FoxPro compatibility from source.  There is a directory named CodeBaseServer2020 which has compile directories as supplied by Sequiter for the Admin Console and the Server for Windows and Linux.  We have not had the time to try testing those development setups.  You are welcome to explore those on your own.

To compile the client-side DLLs for Visual FoxPro compatibility, there are two steps:
- Configure the Source Code for the version you want
- Launch Microsoft Visual Studio 2022 against the desired .VCXPROJ file and run the "Rebuild Solution" process.

To configure the source code, navigate to the WorkingSource directory for the most recent .C language code which is used for building all 4 versions of the DLL.  There are 4 .BAT files in that directory which can be run to configure the source code to support the specific version you want:
- SetVFP_CLIENT_SA32.BAT - The Stand-Alone client DLL for 32-bit applications
- SetVFP_CLIENT_SA64.BAT - Ths Stand-Alone client DLL for 64-bit applications
- SetVFP_CLIENT_SERVER32.BAT - The Client/Server client DLL for 32-bit applications
- SetVFP_CLIENT_SERVER64.BAT - The Client/Server client DLL for 64-bit applications

These .BAT files simply copy the correct version of d4all.h with the settings for your desired version into the main body of source code.  If you are intending to compile a DLL with Clipper compatibility or other variation, you will need to edit the d4all.h file directly, setting the #define values accordingly.

To launch Visual Studio 2022, navigate with Explorer to the appropriate subdirectory in the build subdirectory of this repository.  The subdirectory names for each of the 4 types of client DLLs are self explanatory.  Right click on the c4dll.vcxproj file and select "Open With".  Choose Visual Studio 2022.  (According to Microsoft, version 2022 is the latest and will not be replaced with another base version any time soon, but there will be sub version updates regularly.  The latest version we used for test compilation is 2022 Version 17.14.26.)  The DLL output for the 32-bit versions (both Stand Alone and Client/Server) will be c4dll.dll, and the DLL output for the 64-bit versions (both Stand Alone and Client/Server) will be c4dll64.dll.

As to use of earlier versions of Visual Studio, you may find that they work correctly at least going back to Visual Studio 2017.  We believe that Sequiter compiled the current version of the code with VS versions going back as far as 2010, so you may have luck with that, but we recommend working with 2022 going forward. 

Note that there should be only a few warning messages from compilation to the 32-bit DLLs, but the compilation to 64-bit DLLs will generate a large number of warning messages indicating illegal conversions of various integer types with "potential data loss".  This reflects current best practices in handling variable sizes across 32-bit and 64-bit platforms.  In general it appears to be safe to ignore these warnings; however it is our hope that one of our users who relies on the 64-bit version will update the source code to eliminate most or all of these warnings for future users.


