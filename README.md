# CodeBase-for-DBF
## Copyright and Release to Open Source
CodeBase(tm) is copyright Sequiter, Inc., and has been released to Open Source by Sequiter, Inc.  A PDF containing the agreement to release the software and the applicable GNU Lesser GPL V.3.0 license is included in this repository.  This formerly proprietary software is now available to the larger community for development and refinement.
## Software Description
CodeBase(tm) is a library of routines for reading, writing, indexing, and otherwise handling .DBF type data tables.  The .DBF format was originally developed by Ashton-Tate in the 1980s as part of their dBaseII product, but has been enormously enhanced and extended in the following years.  A number of variants of .DBF tables evolved, the most recent being the Visual FoxPro variations introduced by Microsoft, which greatly expanded the range of field types and introduced the concept of "database containers" which provided capabilities previously found only in high-end SQL databases like SQL Server or MySQL.  CodeBase evolved with the times, and added support for most but not all of the Microsoft enhancements.  Most notably missing is support for the database container concept, although the product can access .DBF tables contained in the database container.

CodeBase(tm) also provides support for the high performance .CDX type indexes introduced in Visual ProPro, which provide more flexibility and sophistication in index construction than most SQL type databases provide.
## Cross-Platform Features
CodeBase(tm) source code provides compilation switches to support LINUX and WINDOWS compilers.  The version in this repository is set up for compilation under Windows Visual Studio 2010, and that is the way the included base library module c4dll.dll was compiled.  This version is a 32-bit compilation.
## Other Variations of .DBF Tables
The CodeBase(tm) module source code supports several variants of DBF files including the original dBase IV versions, Clipper versions, FoxPro 2.x versions, and Visual FoxPro versions.  However, the c4dll.dll version supplied in this distribution was compiled for Visual FoxPro compatibility only.  If you need support for Clipper or dBase IV compatibility you can either build the c4dll.dll from the source or contact MPSystemsServices to request the maintainer to do this for you (when he has time).
## Using CodeBase(tm)
### Using the DLL
Any application development language that can access a C DLL compiled for 32-bit operation under Windows should be able to access this dll and the functions contained in it.  This would include 32-bit versions of C, PowerBasic, VisualBasic, C#, and many other programming environments. It should be noted, however, that CodeBase(tm) in its native form is highly granular, and relatively simple tasks that are accomplished in Visual FoxPro by a single statement or function call, may require many lines of code when calling the c4dll.dll directly.
To address this concern, M-P Systems Services, Inc., has simultaneously released its Python CodeBase Tools which provide a wrapper and set of utilities which greatly simplify data handling with CodeBase(tm), encapsulating its functionality with simple navigational style function calls.
### Documentation
See the books directory for help files in compiled Windows help format.  If you will be using xBase expressions in your indexes or search expressions, you should read both the Reference Guide and the CBextended help for a complete list of all xBase functions and syntax supported in search expressions and index expressions.  

