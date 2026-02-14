/* pop the stack back to previous alignment */

#if defined(_MSC_VER) && !defined(S4WIN64)
   #if _MSC_VER >= 900
      #pragma pack(pop)
   #else
      #pragma pack()
   #endif
#else
   #ifdef __BORLANDC__
      #pragma pack()
   #endif
#endif

#ifdef __BORLANDC__
   #pragma nopackwarning
#endif
