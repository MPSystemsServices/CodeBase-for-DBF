// err5.hpp

#ifndef ERR5_INC

#define ERR5_INC

#define e5oleTable       -30010
#define e5oleMemory      -30020
#define e5oleRange       -30030
#define e5oleArg         -30040
//#define e5oleBadStartPos -30050
#define e5oleFound       -30060
#define e5oleOpen        -30070
#define e5oleConvert     -30080

#define e5bof            -32000
#define e5eof            -32010
#define e5after          -32020
#define e5index          -32030

#include <stdio.h>

/******************************************************************************
   DESIGN of the Error handling for the CodeBase provider.

   Once CodeBase discovers an error the provider will either return from the
   function or throw an error. However, functions exposed to the Consumer can
   not have an error thrown as the Consumer may not catch the error. We should not
   have Consumer level functions call other consumer level functions as the called
   function can create an error and the calling funciton will clear the error.  This
   is a mistake as valuable information can be lost that the consumer may want.  The
   consumer level functions must catch all errors throw by lower level functions.

   When an Err5oledb object is thrown it constructs an object. As part of the
   constructor of this error object the CodeBase provider will fill in an ERRORINFO
   structure.  The ERRORINFO structure will contain all the necessary information
   about the error. Lower level function that catch these errors need to pass this
   information to the highlevel funcitons until the exposed funciton creates an object
   in the error dll for the Consumer to use.

   If an Err5oledb object is not created then the ERRORINFO object needs to be
   created so that the provider can write an error into the error DLL. There are
   four functions that write an error in the error DLL, they are return5oledb(),
   return5oledb2(), return5oledbIID(), and retrun5oledbIID2().  The first function
   is used when the when you need to create an ERRORINFO structure and you do not
   know what IID the called function is. The second function is used when an ERRORINFO
   strucutre is allready created and you want to change the IID. The third function is
   used the same as the first but you do know what the IID the consumer is using and the
   fourth function is the same as the second function but you need add more info.
   If you have caught an error an ERRORINFO structure has already been created. There
   are many different types of Error objects but all are derived from the Err5oledb object.
   These other object are used when we know what error is needed to be returned to
   the consumer.  For example there is a Err5internal object. This object will always
   return an E_FAIL to the consumer.

   All exposed API functions must use these return5...() functions to exit.  They are
   needed as all function need to clear the error DLL before writing a new error object
   to the error DLL. Also we must clear all previous errors before writing new errors.

   Below is an example of how this works
   Exposed_function()
   {
      //failed due to improper inputs
      return5oledb( , hr )
      try
      {
         hr = internal_function()
      }catch Err5oledb&
      {
         //inside a catch you must clear the CODE4.errorCode if necessary
         error4set(c4, 0 ) ;
         return5oledb2( c4 , (ERRORINFO from Err5oledb object))
      }
      return5oledb( , hr)
   }

   internal_function()
   {
      //failed due to improper values
      return hr;
      try
      {
         anotherinternal_function() //throws an Err5oledb object
      }catch Err5oledb&
      {
         throw Err5oledb( , (ERRORINFO from Err5oledb object) )
      }
      throw Err5oledb
      return hr
   }

   As a general rule no function that should call return5...() if the function
   was called by the provider.
  *****************************************************************************/

#ifdef S4WIN32
   DEFINE_GUID(CLSID_CodeBase, 0x33E17311L,0x1716,0x11d0,0x89,0x4E,0x00,0x20,0xAF,0x31,0x2D,0xC7);
#endif

HRESULT globalInterfaceSupportsErrorInfo(REFIID riid);

//#ifdef S4CBPP
//   class S4CLASS CODE4 ;
//   #define CODE4localImplementation CODE4 ;
//#else
//   struct CODE4St ;
//   #ifdef S4CBPP
//      class CODE4 ;
//   #else
//      struct CODE4 ;
//   #endif
//   #define CODE4localImplementation CODE4
//#endif

class Err5
{
public:
   int errorCode ;

   Err5() { errorCode = -1 ; }
   Err5( int ecode ) { errorCode = ecode ; }
   int virtual errCode() { return errorCode ; }
//   Err5(int i ) ;
//   Err5(int i, char *e1 ) ;
//   Err5(int i, char *e1, char *e2 ) ;
//   Err5(int i, char *e1, char *e2, char *e3 ) ;
//   int errCode ;
//   char *e1 ;
//   char *e2 ;
//   char *e3 ;
} ;



class Err5oledb : public Err5
{
public:
//   Err5oledb( HRESULT err, struct CODE4St S4PTR *c4 ) ;
   Err5oledb( HRESULT err, CODE4 S4PTR *c4 ) ;
   Err5oledb( HRESULT err, ERRORINFO transfer) ;
   Err5oledb( HRESULT err, REFIID failInInterface, DWORD provCode ) ;
   virtual HRESULT oledbCode() { return info.hrError ; }
   ERRORINFO oledbErr() { return ( info ) ; }

   ERRORINFO info ;
} ;



class Err5provider : public Err5oledb
{
public:
   Err5provider( int rcCodeBaseParm, CODE4 *c4 ) : Err5oledb(E_FAIL, c4) { errorCode = rcCodeBaseParm ; }
//   HRESULT oledbCode() { return rcCodeBase ; }     // oleCode will not have been initialized
//   int rcCodeBase ;
} ;



class Err5internal : public Err5oledb
{
public:
     Err5internal(CODE4 *c4) : Err5oledb(E_FAIL, c4) { errorCode = e4info ; }
     Err5internal(DWORD lookup, REFIID failInInterface) : Err5oledb(E_FAIL, failInInterface, lookup) { errorCode = e4info ; }
} ;



class Err5conversion : public Err5oledb
{
public:
   Err5conversion(HRESULT h, DBSTATUS s, CODE4 *c4=0, int isWarningParm = 0 ) : Err5oledb(h, c4) { status = s ; isWarning = isWarningParm ; }
   Err5conversion(DWORD lookup, REFIID failInInterface, HRESULT h, DBSTATUS s, int isWarningParm = 0 ) : Err5oledb(h, failInInterface, lookup) { status = s ; isWarning = isWarningParm ; }
   DBSTATUS statusCode() {return status ;}
   DBSTATUS status ;
   int isWarning ;
} ;



class Err5truncate : public Err5conversion
{
public:
   Err5truncate(CODE4 *c4) : Err5conversion(DB_S_ERRORSOCCURRED, DBSTATUS_S_TRUNCATED, c4, 1 ) {}
   Err5truncate( DWORD lookup, REFIID failInInterface ) : Err5conversion(lookup, failInInterface, DB_S_ERRORSOCCURRED, DBSTATUS_S_TRUNCATED, 1 ) {}
} ;



class Err5notIntegrated : public Err5oledb
{
public:
   Err5notIntegrated( CODE4 *c4 ) : Err5oledb( E_FAIL, c4 ) { errorCode = e5oleOpen ; }
} ;



class Err5signMismatch : public Err5conversion
{
public:
   Err5signMismatch(CODE4 *c4) : Err5conversion(DB_S_ERRORSOCCURRED, DBSTATUS_E_SIGNMISMATCH, c4 ) {}
   Err5signMismatch( DWORD lookup, REFIID failInInterface ) : Err5conversion( lookup, failInInterface, DB_S_ERRORSOCCURRED, DBSTATUS_E_SIGNMISMATCH, 0 ) {}
} ;



class Err5cantconvertvalue : public Err5conversion
{
public:
   Err5cantconvertvalue(CODE4 *c4) : Err5conversion(DB_S_ERRORSOCCURRED, DBSTATUS_E_CANTCONVERTVALUE, c4) {}
   Err5cantconvertvalue( DWORD lookup, REFIID failInInterface ) : Err5conversion( lookup, failInInterface, DB_S_ERRORSOCCURRED, DBSTATUS_E_CANTCONVERTVALUE, 0 ) {}
} ;



class Err5dataoverflow : public Err5conversion
{
public:
   Err5dataoverflow(CODE4 *c4) : Err5conversion(DB_S_ERRORSOCCURRED, DBSTATUS_E_DATAOVERFLOW, c4) {}
   Err5dataoverflow( DWORD lookup, REFIID failInInterface ) : Err5conversion( lookup, failInInterface, DB_S_ERRORSOCCURRED, DBSTATUS_E_DATAOVERFLOW, 0 ) {}
} ;



// special non-fox ability to get null from schema tables only
//class Err5null : public Err5conversion
//{
//public:
   // use CANTCONVERT in case attempt to set data is made (invalid becuase
   //  cannot assign to schemas)
//   Err5null() : Err5conversion(DB_S_ERRORSOCCURRED, DBSTATUS_E_CANTCONVERTVALUE) {}
//} ;



class Err5table : public Err5oledb
{
public:
   Err5table(CODE4 *c4) : Err5oledb( E_FAIL, c4 ) { errorCode = e5oleTable ; }
   Err5table( DWORD lookup, REFIID failInInterface ) : Err5oledb(E_FAIL, failInInterface, lookup) { errorCode = e5oleTable ; }
} ;



class Err5convert : public Err5oledb
{
public:
   Err5convert(CODE4 *c4) : Err5oledb( E_FAIL, c4 ) { errorCode = e5oleConvert ; }
   Err5convert(DWORD lookup, REFIID failInInterface) : Err5oledb(E_FAIL, failInInterface, lookup) { errorCode = e5oleConvert ; }
} ;



class Err5memory : public Err5oledb
{
public:
   Err5memory(CODE4 *c4=0) : Err5oledb( E_OUTOFMEMORY, c4 ) { errorCode = e5oleMemory ; }
   Err5memory(DWORD lookup, REFIID failInInterface) : Err5oledb(E_OUTOFMEMORY, failInInterface, lookup) { errorCode = e5oleMemory ; }
} ;



class Err5range : public Err5oledb
{
public:
   Err5range(CODE4 *c4) : Err5oledb( E_FAIL, c4 ) { errorCode = e5oleRange ; }
   Err5range(DWORD lookup, REFIID failInInterface) : Err5oledb(E_FAIL, failInInterface, lookup) { errorCode = e5oleRange ; }
} ;



class Err5bof : public Err5
{
public:
   Err5bof() : Err5( e5bof ) { ; }
} ;



class Err5eof : public Err5
{
public:
   Err5eof() : Err5( e5eof ) { ; }
} ;



class Err5after : public Err5
{
public:
   Err5after() : Err5( e5after ) { ; }
} ;



class Err5index : public Err5
{
public:
   Err5index() : Err5( e5index ) { ; }
} ;



class Err5invalidArg : public Err5oledb
{
public:
   Err5invalidArg(CODE4 *c4) : Err5oledb( E_INVALIDARG, c4 ) { errorCode = e5oleArg ; }
   Err5invalidArg(DWORD lookup, REFIID failInInterface) : Err5oledb(E_INVALIDARG, failInInterface, lookup) { errorCode = e5oleArg ; }
} ;



class Err5found : public Err5oledb
{
public:
   Err5found(CODE4 *c4) : Err5oledb( E_FAIL, c4 ) { errorCode = e5oleFound ; }
   Err5found(DWORD lookup, REFIID failInInterface) : Err5oledb(E_FAIL, failInInterface, lookup) { errorCode = e5oleFound ; }
} ;



class Err5open : public Err5oledb
{
public:
   Err5open(CODE4 *c4) : Err5oledb( E_FAIL, c4 ) {;}
   Err5open(DWORD lookup, REFIID failInInterface) : Err5oledb(E_FAIL, failInInterface, lookup) { errorCode = e5oleOpen; }
} ;

class Err5noTable : public Err5oledb
{
public:
   Err5noTable(CODE4 *c4) : Err5oledb( DB_E_NOTABLE , c4 ) {;}
   Err5noTable(DWORD lookup, REFIID failInInterface) : Err5oledb(E_FAIL, failInInterface, lookup) { ; }
} ;

class Err5noIndex : public Err5oledb
{
public:
   Err5noIndex(CODE4 *c4) : Err5oledb( DB_E_NOINDEX , c4 ) {;}
   Err5noIndex(DWORD lookup, REFIID failInInterface) : Err5oledb( DB_E_NOINDEX, failInInterface, lookup) { errorCode = e5oleOpen ; }
} ;

#endif /* ERR5_INC */
