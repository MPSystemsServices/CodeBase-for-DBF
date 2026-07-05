      readme.txt (c)Copyright Sequiter Software Inc., 1992-2007.
      All rights reserved. [and See Comments below supplied by
      M-P System Services, Inc., the administrator of this
      repository.]
      ----------------------------------------------------------

      README -  Table of Contents for CodeBase 6.5

      I.    Introduction

      II.   CodeBase Electronic Documentation Files

      III.  What's New

      IV.   Important CodeBase 6.x Information

      V.    CodeBase Documentation Errata

      VI.   Upgrading From Previous Versions of CodeBase Products

      VII.  Contacting CodeBase Software

      VIII. Known Problems with CodeBase

      IX.   Using Delphi 2 with CodeBase 6

      X.    Using Visual Basic 3 with CodeBase 6

      XI.   Change to CodeBase C++ API

      ----------------------------------------------------------

      I.    Introduction

         CodeBase 6 serial and version numbers can be found in the
         product download e-mail that includes your userid and password.

         This document is intended for users of CodeBase with any of the
         programming languages supported by CodeBase 6.5 ( 'C', 'C++', Basic
         and Pascal). However, some sections are specific to an individual
         programming language. Any such section will be preceded with one of
         the following symbols:

            Symbol         Specific To
            ------         -----------

            C/C++          The C/C++ language

            BAS            Basic/Visual Basic

            PAS            Pascal/Delphi

      ----------------------------------------------------------

      II.    CodeBase Electronic Documentation Files

         IMPORTANT NOTE FOR FIRST TIME USERS OF CODEBASE:

         It is highly recommended that any installed documentation files be
         read.  In particular the Getting Started and users guide manuals
         should be read.

         Please note that only documentation files relevant to the options
         installed will be present on your drive after installation.

         All electronic .txt files have been suitably formatted for
         hard-copy printing. To print any document, simply direct that file
         to your printer, using the 'print' command.

         The complete list of documentation files that are present in the
         CodeBase install is as follows:

            README.TXT     This file contains general user information,
                           changes to CodeBase, and installation
                           information. Always installed in the root
                           CodeBase directory (default \CODEBASE).

            GETTING_STARTED_MANUAL.CHM  Essential information to begin
                           using CodeBase

            C_USERS_GUIDE.CHM   Information on using CodeBase With C

            C_REFERENCE_GUIDE.CHM  CodeBase Reference Guide.  Documents
                           All CodeBase C Functions

            CPP_USERS_GUIDE.CHM   Information on using CodeBase With C++

            CPP_REFERENCE_GUIDE.CHM  CodeBase Reference Guide.  Documents
                           All CodeBase C++ Functions

            CBEXTENDED.CHM Describes New CodeBase Features that have been
                           added to CodeBase since the last release.


      ----------------------------------------------------------

      III.  What's New

         CodeBase 6.5

          - Support for the following compilers:

               Microsoft Visual C++ 6
               Microsoft Visual Basic 6
               Borland C++ Builder 4
               Borland Delphi 4
               Borland Delphi 5

          - Visual FoxPro Support

               Support for new field types introduced with Visual FoxPro as
               been added. See the on-line Reference Guide (REF.PDF) for
               documentation on the areas that are new.

               CodeBase supports the following Visual FoxPro field types.
               See Appendix E: Field Types for details on these field types.

                  Binary Character
                  Binary Memo
                  Currency
                  DateTime
                  Double
                  Integer

               CodeBase supports the ability to mark a field as null. The
               FIELD4INFO structure has a additional member: nulls.

               The "compatibility" member has been added to the CODE4
               structure (Code4 class in C++). This member allows database
               files created by CodeBase to be in the Visual FoxPro format.

               The following functions have been added.

                  f4assignCurrency (Field4::assignCurrency in C++)
                     Convert a string and assign it to a Currency field.
                  f4assignDateTime (Field4::assignDateTime in C++)
                     Convert a string in the form CCYYMMDDhh:mm:ss and
                     assign it to a DateTime field.
                  f4assignNull (Field4::assignNull in C++)
                     Mark the field as null.
                  f4currency (Field4::currency in C++)
                     Extract a value in a Currency field to a string.
                  f4dateTime (Field4::dateTime in C++)
                     Extract a value in a DateTime field to a string.
                  f4null (Field4::isNull in C++)
                     Is the field marked null?
                  expr4null (Expr4::isNull in C++)
                     Does the expression contain null?

          - The location of the DLL16 and DLL32 directory has changed.
            It is no longer in the programming language directory
            (e.g. \CODEBASE\CPP\DLL32); it is now in the root CodeBase
            directory (e.g. \CODEBASE\DLL32).

          - Large File Support

               CodeBase can be configured to exceed the file size limit
               commonly associated with abase database files.

               Libraries built with Microsoft Visual C++ 4.x or earlier or
               Borland C++ 4.5 (32-bit only) will not be large file enabled.
               However, DLLs built with other compilers will have this
               feature enabled and Visual C++ 4 and Borland C++ 4.5 will be
               able to use these DLLs. All prebuilt 32-bit DLLs in this
               release have been built with Visual C++ 6.

               The following functions have been added to CodeBase. Not all
               functions are in the printed Reference Guide. See the on-line
               Reference Guide (REF.PDF) for documentation on these
               functions.

                  code4largeOn (Code4::largeOn in C++)
                     Turn on the large file support by implementing a
                     different locking algorithm.
                  code4indexBlockSizeSet (Code4::indexBlockSizeSet in C++)
                     Sets the block size of the index files that get created
                     and change the index file structure  so that their size
                     can exceed traditional file size limits.

               LARGESET.EXE is an additional utility which can be found in
               the CodeBase BIN directory. It will modify the behavior of
               code4init (Code4::init or Code4 constructor in C++) by
               calling code4largeOn.

          - Unicode

               In C, C++ and Delphi, CodeBase has better support for Unicode
               data. The field type r4unicode has been added. The following
               functions have been added. See the on-line Reference Guide
               (REF.PDF) for documentation on these functions.

                  f4assignUnicode (Field4::assignUnicode in C++)
                     Assign a Unicode string to a field.
                  f4memoAssignUnicode (Field4memo::assignUnicode in C++)
                     Assign a Unicode string to a memo field.

               The functions f4str and f4memoStr (Field4::str and
               Field4memo::str in C++) return strings terminated by two null
               bytes. Therefore, the string can contain Unicode or ANSI
               characters.

          - The CodeBase Administrator, formerly called the CodeBase Server
            Administrator, has been enhanced to function in stand-alone. It
            can now be used to run maintenance operations (reindex, pack,
            etc.) in client/server or stand-alone.

          - The dBASE function RIGHT() has been added. See Appendix C: dBASE
            Expressions in the on-line Reference Guide (REF.PDF) for
            documentation.

         CodeBase 6.4

          - Additional multithreading support has been added to the
            client/server configuration, significantly improving performance
            under most configurations. As a result, the construction of
            client applications is no longer supported with Borland C++ 4.5,
            Microsoft Visual C++ 2.x, or any 16-bit compiler. The server is
            now multihomed, which means that the server will accept
            connections addressed to any IP address assigned to the machine
            (i.e. modem and network cards).

          - There is no longer a separate communication library used with
            client or server applications (i.e. C4SOCK.DLL or S4SOCK.DLL).
            All communication is built into the main CodeBase library.

          - Advanced security features have been built into the
            client/server configuration. The CodeBase Administrator utility,
            found in the CBAdmin directory, can be used to administer these
            features.

      ----------------------------------------------------------

      IV.  Important CodeBase 6.x Information

         This section documents some of the internal changes made to
         CodeBase 6 and later products that are not found in the printed or
         electronic documentation.

         Speed Improvements:
         -------------------

         Internal enhancements have been made to improve the overall
         speed of CodeBase 6. Although changes have been made for
         both 16 and 32-bit applications, the majority of the speed
         improvements will be gained for 32-bit applications running
         under Windows 95 and NT.

         Miscellaneous:
         --------------

         - All prebuilt DLLs are built with Microsoft Visual C++ 6.0.
           If you have problems using any of the prebuilt DLLs with another
           C/C++ compiler, you should rebuild the DLL with your compiler.

         - Be aware that when an error sets the errorCode member variable of
           the CODE4 structure to a negative value, this member must be
           reset to zero before subsequent functions will execute.

         - Due to changes in the ISO/ANSI standard for C++, the CodeBase
           C++ API may be slightly modified. Refer to section X of
           this document for further information.

      ----------------------------------------------------------

      V.  CodeBase Documentation Errata

         The corrections to the on-line and printed documentation are
         located in the ERRATA.TXT file. Refer to this file for the latest
         changes in our printed and on-line documentation.

      ----------------------------------------------------------

      VI.   Upgrading From Previous Versions of CodeBase Products

         The CodeBase 6 API is different from any previous CodeBase API
         product ( i.e. CodeBase, CodeBase++, CodeBasic, etc. ). Therefore
         existing applications cannot be compiled with the CodeBase 6
         libraries.

      ----------------------------------------------------------

      VII.  Contacting CodeBase Software

      Web Site
      --------

      The main mechanism for non-telephone technical support is through
      CodeBase's web site. Our web site includes items such
      as the latest source code patches, FAQs, and other support files.
      In addition, you can submit technical support questions via the
      provided electronic forms.

      If you wish to contact CodeBase electronically, the web site
      should be your first method of contact. If you do not have access
      to a web browser, other secondary options are listed later on in
      this section.

      The CodeBase Software web site can be found at:
              http://www.codebase.com
[COMMENT BY REPOSITORY ADMINISTRATOR]
Starting in 2018, another, completely different company grabbed the CodeBase name
and the www.codebase.com domain.  There is no longer any on-line presence for
the original CodeBase development team.  Sequiter still exists, but has moved
into entirely different businesses and has no resources to support or modify
the application CodeBase released to Open Source in this repository.
[ENDCOMMENT]
      Usenet Newsgroup
      ----------------

      A Usenet newsgroup is available for CodeBase users to interact
      with each other and exchange information.
      The name of the newsgroup is: comp.databases.xbase.codebase

      CodeBase technical support does not check posts to this newsgroup.
      Questions addressed to technical support
      should be sent in via the web site.

      ----------------------------------------------------------

      VIII.  Known Problems with CodeBase

      The following sections lists known problems with CodeBase and
      CodeReporter for this release. Patch files may be posted as they
      become available (See section IV).

      CodeBase
      --------

         - Query Optimization is not enabled when querying on a FoxPro
           Integer (r4int), Currency (r4currency), or DateTime (r4dateTime)
           field.

         - When a relation is constructed in client/server, a separate
           DATA4 structure is created and maintained by the relation. In
           order to close all handles to a database, the database itself
           will need to be closed as well as each relation the database was
           a part of.

         - The TRIM(), LTRIM() and ALLTRIM() dBASE expression functions
           return unpredictable lengths. The result of these functions
           should not be used by any operation that requires the length
           to be accurate, including comparison operations.

                      e.g. TRIM(L_NAME) + TRIM(F_NAME)      is OK
                           SUBSTR( TRIM(L_NAME), 3, 2 )     is not OK
                           TRIM( 'BOB ' ) $ 'BOBJ'          is not OK
                           ALLTRIM( ' george ' ) > 'george' is not OK

         - String comparison of varying length works different than
           under FoxPro.  For example, expr "AAAA" = "AAA" and expr
           "AAA" = "AAAA" both return 'TRUE' in CodeBase, but only
           the second expression returns 'TRUE' in FoxPro.

      CodeReporter
      ------------

         - When creating a report with calculation objects, the objects
           cannot be used as part of a query, a sort, or when setting a
           relation between a master and slave. For example, if you create a
           calculation object called MyCalc, you cannot use 'MyCalc' as part
           of a query expression, etc.

      ----------------------------------------------------------

      IX.  Using Delphi with CodeBase 6


         - Some references to the Delphi 1.0 menu options may be not apply,
           due to the re-design of the newer Delphi interface. Refer to your
           Delphi documentation for more information on menu items.

           In particular, the File | Open Project menu item has been merged
           into the generic File | Open menu option. When using File | Open
           to open any of the examples that contain program units (e.g.
           APPEND.PAS), you must select 'Pascal Project (*.pas)' as the File
           Type in the Open File dialog. If you leave the File Type as the
           default 'Delphi File (*.pas *.dpr), you will not be able to
           compile the file from within the Delphi environment.

      ----------------------------------------------------------

      X.   Using Visual Basic 3 with CodeBase 6

         In order to build CodeBase applications with Visual Basic 3, you
         must first make some minor changes to the CodeBase module,
         CODEBASE.BAS. The module uses #If directives to examine conditional
         compiler constants, neither of which are supported by Visual Basic
         3. The change you need to make is to delete the unused parts of the
         #If directives, leaving only those lines which apply to Visual
         Basic 3.

         For example, change this:
            Function report4parent%(ByVal r4&, ByVal hWnd&)
               #If Win16 Then
                  report4parent = report4parent16(r4, hWnd)
               #End If
               #If Win32 Then
                  report4parent = report4parent32(r4, hWnd)
               #End If
            End Function

         to this:
            Function report4parent%(ByVal r4&, ByVal hWnd&)
               report4parent = report4parent16(r4, hWnd)
            End Function

         This change is not necessary in other versions of Visual Basic;
         Visual Basic 4 and later supports #If directives and conditional
         compiler constants.

      ----------------------------------------------------------

      XI.  Change to CodeBase C++ API

         Due to changes in the ISO/ANSI standard for C++, the term 'true' is
         now a keyword. Because the CodeBase C++ API contains methods that
         use this name, compiler errors can occur with Borland 5 and Visual
         C++ 4.1 and later versions.

         To resolve this issue, any CodeBase methods named 'true' have been
         changed to 'isTrue' if you are compiling with Borland 5.0 or Visual
         C++ 4.1. If you are using older compilers, this modification does
         not occur.

         If you use Borland C++ 5 or Visual C++ 4.1 and you still wish to
         use this keyword in your application, you can override this default
         behavior by defining S4USE_TRUE in both the build of the CodeBase
         library, and in your own application.

         The two methods affected by this change are Str4::true() and
         Expr4::true().
