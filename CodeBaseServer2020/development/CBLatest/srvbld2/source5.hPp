/* FILE source5.hpp
(c)Copyright Sequiter Software Inc., 1996-7.  All rights reserved. */

#ifdef OLEDB5BUILD

#ifndef S4CLIENT
class Table5tables ;
class Table5indexes ;
class Table5columns ;
#endif
class Session5 ;


#ifndef S4SERVER
class S4CLASS Source5interface : private IUnknown
{
public:

   Source5interface() { pSource=NULL; notSetInterface=0;}
   Source5interface( Source5 *source ) {pSource=source; notSetInterface=0; }
   DECLARE5_IUnknown

   Source5 *pSource ;
   short notSetInterface ;
};
#endif

// class used for both S4SERVER and STAND/ALONE and S4CLIENT
class S5CLASS Source5config
{
// contains the config information as to column names
public:
   Source5config()
      {
         strcpy( charDeletedFieldName, "DEL5" ) ;
         strcpy( charRecnoFieldName, "RECNO5" ) ;
// AS 04/30/98 conformance tests, bookmark field should not have a name (should be null)
//         strcpy( charBookmarkName, "BOOKMARK5" ) ;
         wcscpy( deletedFieldName, L"DEL5" ) ;
         wcscpy( recnoFieldName, L"RECNO5" ) ;
// AS 04/30/98 conformance tests, bookmark field should not have a name (should be null)
//         wcscpy( bookmarkName, L"BOOKMARK5" ) ;
      }

   char charDeletedFieldName[5] ;
   char charRecnoFieldName[7] ;
// AS 04/30/98 conformance tests, bookmark field should not have a name (should be null)
//   char charBookmarkName[10] ;

   WSTR5 deletedFieldName[LEN5COLUMN_NAME] ;
   WSTR5 recnoFieldName[LEN5COLUMN_NAME] ;
// AS 04/30/98 conformance tests, bookmark field should not have a name (should be null)
//   WSTR5 bookmarkName[LEN5COLUMN_NAME] ;
} ;

class S5CLASS Source5all : public Source5config
{
public:
   CODE4 *c4 ;
   #ifdef S4SERVER
      void initUndo() ;
      char isUnicode ;
      char *systemPath() ; // { return c4systemPath( c4 ) ; }
   #else
      // server has a different session for each client, but one source for the whole server...
      Session5 *session ;
      char *catalog ;    // catalog directory in use - based on property
   #endif
   Source5all() ;
   #ifndef S4CLIENT
      Table5tables *tablesSchema ;    // reference needed by indexes schema
      Table5indexes *indexesSchema ;    // reference needed by indexes schema
      Table5columns *columnsSchema ;    // reference needed by columns schema
   #endif
   #ifdef S4STAND_ALONE
      // AS 05/26/99 --> modified - need to include path stuff
      char logName[LEN4PATH] ;    // CJ-03/12/98-transaction processing log file name - based on property
      char *systemPath() { return catalog ; }
   #endif
} ;

#ifndef S4SERVER

// Source5 base object and contained interface definitions

//extern LONG g_cObj ;
DEFINE_GUID(CLSID_CodeBase, 0x33E17311L,0x1716,0x11d0,0x89,0x4E,0x00,0x20,0xAF,0x31,0x2D,0xC7) ;

// Classes -------------------------------------------------------------------

//----------------------------------------------------------------------------
// Source5 | Containing class for all interfaces on the Datasource
// CoType Object
//
// AS 08/26/99 -- ISupportErrorInfo class not derived as expected...
class Source5 : public Source5all, private IDBCreateSession, private IDBInitialize, private IDBProperties, private IPersist, private IDBInfo, private Unknown5, private ISupportErrorInfo
{
public:
   void * operator new( size_t s ) { mem4init() ; void *ptr = alloc5(s) ; memset( ptr, 0, s); return ptr ; }
   void operator delete( void *p ) ;
   Source5(LPUNKNOWN) ;
   ~Source5() ;

   void init() ;

   DECLARE5_IUnknown
   DECLARE5_IDBCreateSession
   DECLARE5_IDBInitialize
   DECLARE5_IDBProperties
   DECLARE5_IDBInfo
   DECLARE5_ISupportErrorInfo

   STDMETHODIMP GetClassID(CLSID *pClsID) ;
   Source5properties properties ; // Property object to manage properties
   #ifndef S4SERVER
      IUnknown *pUnkOuter;
      Source5interface *sourceAgg ;
      HRESULT verifySetIUnKnown(REFIID riid, IUnknown *punkOuter);
   #endif

protected:
   //flag == TRUE if this Data Source object is an an initialized state
   BOOL initialized ;
private:
   void setCatalogFromProperties( Bool5 useCatalogPropertyOnly ) ;
} ;

#endif /* S4SERVER */

#endif /* OLEDB5BUILD */
