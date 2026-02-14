// Unknown5.hpp

#if defined( OLEDB5BUILD ) && !defined( S4SERVER )

   #ifdef S4JOINT_OLEDB_DLL
      // can't be #define because if c4 is 0 (0)->errorCode does not compile...
      // #define reset5errorCode( c4 ) ( (c4) ? ((c4)->errorCode = 0 ) : 0 )
      inline void reset5errorCode( CODE4 *c4 ) { (c4 ? ( (c4)->errorCode = 0 ) : 0 ) ; }
   #else
      // #define reset5errorCode( c4 ) ( (c4) ? error4set( c4, 0 ) : 0 )
      inline void reset5errorCode( CODE4 *c4 ) { (c4 ? error4set( c4, 0 ) : 0 ) ; }
   #endif


   #define return5resetErrorCode( c4, hr ) ( reset5errorCode( c4 ), (hr) )
   /*
      inline HRESULT return5resetErrorCode( CODE4 *c4, HRESULT hr )
      {
            CodeBase should never be left in an error state when departing a user-based function.
            Therefore, this function resets CodeBase to a safe state before returning to the user.

            This is basically a small safety feature to reset the error code to avoid a potential
            incorrect error state, or to just reset it before return when it is suspected that
            we are in an error state (after a catch() statement).

         reset5errorCode( c4 ) ;
         return hr ;
      }
   */

   #define return5ulongResetErrorCode( c4, hr ) ( reset5errorCode( c4 ), (hr) )

   HRESULT return5verifyCorrectErrorCode( CODE4 *c4, HRESULT hr ) ;
   ULONG return5ulongVerifyCorrectErrorCode( CODE4 *c4, ULONG hr ) ;
   #ifdef S4CLIENT
      void verify5connectionClear( CODE4 *c4 ) ;
      HRESULT return5cleanConnection( CODE4 *c4, HRESULT hr ) ;
      ULONG return5ulongCleanConnection( CODE4 *c4, ULONG hr ) ;
      HRESULT return5verifyConnectionClear( CODE4 *c4, HRESULT hr ) ;
      ULONG return5ulongVerifyConnectionClear( CODE4 *c4, ULONG hr ) ;
   #endif

   #ifdef E4DEBUG
      #ifdef S4CLIENT
         #define return5verifyOledb( c4, hr ) ( return5verifyConnectionClear( (c4), return5verifyCorrectErrorCode( (c4), (hr) ) ) )
         #define return5verifyOledbUlong( c4, hr ) ( return5ulongVerifyConnectionClear( (c4), return5ulongVerifyCorrectErrorCode( (c4), (hr) ) ) )
      #else
         #define return5verifyOledb( c4, hr ) ( return5verifyCorrectErrorCode( (c4), (hr) ) )
         #define return5verifyOledbUlong( c4, hr ) ( return5ulongVerifyCorrectErrorCode( (c4), (hr) ) )
      #endif
   #else
      #ifdef S4CLIENT
         #ifdef OLEDB5SAFE
            #define return5verifyOledb( c4, hr ) ( return5resetErrorCode( (c4), return5cleanConnection( (c4), (hr) ) ) )
            #define return5verifyOledbUlong( c4, hr ) ( return5ulongResetErrorCode( (c4), return5ulongCleanConnection( (c4), (hr) ) ) )
         #else
            #define return5verifyOledb( c4, hr ) (hr)
            #define return5verifyOledbUlong( c4, hr ) (hr)
         #endif
      #else
         #ifdef OLEDB5SAFE
            #define return5verifyOledb( c4, hr ) ( return5resetErrorCode( (c4), (hr) ) )
            #define return5verifyOledbUlong( c4, hr ) ( return5ulongResetErrorCode( (c4), (hr) ) )
         #else
            #define return5verifyOledb( c4, hr ) (hr)
            #define return5verifyOledbUlong( c4, hr ) (hr)
         #endif
      #endif
   #endif

   // cam to replace return5iErrorInfo with a real function for IErrorInfo
   HRESULT return5iErrorInfo( HRESULT hr, CODE4 *c4, REFIID iid ) ;
   HRESULT return5iErr5Info( ERRORINFO err, REFIID iid ) ;
   #ifdef S4OLEDEBUG_PRINT
      HRESULT return5printErr( HRESULT hr ) ;
   #endif

   #define return5oledb( c4, hr )   ( return5verifyOledb( (c4), (return5iErrorInfo( hr , c4, IID_IUnknown )) ) )
   #define return5oledb2( c4, err ) ( return5verifyOledb( (c4), (return5iErr5Info( err, IID_IUnknown )) ) )
   #define return5oledbIID( c4, hr, iid )  ( return5verifyOledb( (c4), (return5iErrorInfo( hr , c4, iid )) ) )
   #define return5oledbIID2( c4, err, iid ) ( return5verifyOledb( (c4), (return5iErr5Info( err, iid )) ) )

   #ifdef S4OLEDEBUG_PRINT
      #define return5noErr(c4, hr) ( return5verifyOledb ( (c4) , (return5printErr(hr)) ) )
   #else
      #define return5noErr(c4, hr)     ( return5verifyOledb( (c4), (hr) ) )
   #endif
   #define return5oledbUlong( c4, hr ) ( return5verifyOledbUlong( (c4), ( hr ) ) )

   class Unknown5 : private IUnknown
   {
   public:
      STDMETHODIMP QueryInterface(REFIID, LPVOID *);
      STDMETHODIMP_(ULONG) AddRef(void)  { return ++ref ; }
      virtual STDMETHODIMP_(ULONG) Release(void) = 0 ;

      Unknown5() { ref = 0 ; }

   public:
      ULONG ref ;   // reference count for iunknown
   } ;
#else
   #define return5oledb( c4, hr )   (hr)
   #define return5oledb2( c4, err ) (hr)
   #define return5noErr(c4, hr)     (hr)
   #define return5oledbUlong( c4, hr ) (hr)
#endif /* #if defined( OLEDB5BUILD ) && !defined( S4SERVER ) */





