/* push the stack to one-byte alignment */

#if defined(_MSC_VER) && !defined(S4WIN64)
   #if _MSC_VER >= 900
      #pragma pack(push,1)
   #else
      #pragma pack(1)
   #endif
#else
   #ifdef __BORLANDC__
      #pragma pack(1)
   #endif
#endif

#ifdef __BORLANDC__
   #pragma nopackwarning
#endif
