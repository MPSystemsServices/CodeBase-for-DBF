/* r4bit.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"

#ifndef S4OFF_REPORT
#ifdef S4WINDOWS

#define DIB_HEADER_MARKER   ((WORD) ('M' << 8) | 'B')
#define BYTES_PER_READ  32767


#define WIDTHBYTES(bits)      (((bits) + 31) / 32 * 4)
#define IS_WIN30_DIB(lpbi)  ((*(LPDWORD) (lpbi)) == sizeof (BITMAPINFOHEADER))


DWORD PASCAL kwrite ( int, VOID FAR *, DWORD );

WORD S4FUNCTION bmp4DIBNumColors (LPSTR lpbi)
{
   WORD wBitCount;


      /* If this is a Windows style DIB, the number of colors in the  */
      /*  color table can be less than the number of bits per pixel   */
      /*  allows for (i.e. lpbi->biClrUsed can be set to some value). */
      /*  If this is the case, return the appropriate value.          */

   if (IS_WIN30_DIB (lpbi))
      {
      DWORD dwClrUsed;

      dwClrUsed = ((LPBITMAPINFOHEADER) lpbi)->biClrUsed;

      if (dwClrUsed)
         return (WORD) dwClrUsed;
      }


      /* Calculate the number of colors in the color table based on */
      /*  the number of bits per pixel for the DIB.                 */

   if (IS_WIN30_DIB (lpbi))
      wBitCount = ((LPBITMAPINFOHEADER) lpbi)->biBitCount;
   else
      wBitCount = ((LPBITMAPCOREHEADER) lpbi)->bcBitCount;

   switch (wBitCount)
      {
      case 1:
         return 2;

      case 4:
         return 16;

      case 8:
         return 256;

      default:
         return 0;
      }
}


WORD S4FUNCTION bmp4PaletteSize (LPSTR lpbi)
{
   if (IS_WIN30_DIB (lpbi))
      return (bmp4DIBNumColors (lpbi) * sizeof (RGBQUAD));
   else
      return (bmp4DIBNumColors (lpbi) * sizeof (RGBTRIPLE));
}

LPSTR S4FUNCTION bmp4FindDIBBits (LPSTR lpbi)
{
   return (lpbi + *(LPDWORD)lpbi + bmp4PaletteSize (lpbi));
}



HANDLE S4FUNCTION bmp4GetDIB ( LPSTR fname, CODE4 *codeBase )
{

   int      hFile, errOpen;
   OFSTRUCT ofs;
   HANDLE   hDIB;
   char     fname2[14];
   FILE4    file;

   u4namePiece( fname2, sizeof(fname2), fname, 0, 1 );
   SetCursor(LoadCursor((HINSTANCE)NULL, IDC_WAIT));

   errOpen = codeBase->errOpen;
   codeBase->errOpen = 0;

   if( file4open( &file, codeBase, fname2, 1 ) == 0 )
   {
      file4close( &file );
      codeBase->errOpen = errOpen;

      if ((hFile = OpenFile (fname2, &ofs, OF_READ)) != -1)
      {
         hDIB = bmp4ReadDIBFile (hFile);
         _lclose (hFile);
         SetCursor(LoadCursor((HINSTANCE)NULL, IDC_ARROW));
         return hDIB;
      }
   }
   else
      error4set( codeBase, 0 );

   codeBase->errOpen = errOpen;
   if ((hFile = OpenFile (fname, &ofs, OF_READ)) != -1)
   {
      hDIB = bmp4ReadDIBFile (hFile);
      _lclose (hFile);
      SetCursor(LoadCursor((HINSTANCE)NULL, IDC_ARROW));
      return hDIB;
   }

   SetCursor(LoadCursor((HINSTANCE)NULL, IDC_ARROW));
   return (HANDLE)NULL;
}

/*************************************************************************

  Function:  MyRead (int, LPSTR, DWORD)

   Purpose:  Routine to read files greater than 64K in size.

   Returns:  TRUE if successful.
             FALSE if an error occurs.

  Comments:

   History:   Date     Reason

             6/1/91    Created

*************************************************************************/

BOOL MyRead (int hFile, LPSTR lpBuffer, DWORD dwSize)
{
   #ifndef S4WIN32
      char huge *lpInBuf = (char huge *) lpBuffer;
   #else
      char *lpInBuf = (char *) lpBuffer;
   #endif
   int       nBytes;


   while (dwSize)
      {
      nBytes = (int) (dwSize > (DWORD) BYTES_PER_READ ? BYTES_PER_READ :
                                                        LOWORD (dwSize));

      if (_lread (hFile, (LPSTR) lpInBuf, nBytes) != (WORD) nBytes)
         return FALSE;

      dwSize  -= nBytes;
      lpInBuf += nBytes;
      }

   return TRUE;
}


/*************************************************************************

  Function:  ReadDIBFile (int)

   Purpose:  Reads in the specified DIB file into a global chunk of
             memory.

   Returns:  A handle to a dib (hDIB) if successful.
             NULL if an error occurs.

  Comments:  BITMAPFILEHEADER is stripped off of the DIB.  Everything
             from the end of the BITMAPFILEHEADER structure on is
             returned in the global memory handle.

   History:   Date      Author      Reason

             6/1/91    Created
             6/27/91   Removed PM bitmap conversion routines.
             6/31/91   Removed logic which overallocated memory
                       (to account for bad display drivers).
            11/08/91   Again removed logic which overallocated
                       memory (it had creeped back in!)

*************************************************************************/

HANDLE S4FUNCTION bmp4ReadDIBFile (int hFile)
{
   BITMAPFILEHEADER   bmfHeader;
   DWORD              dwBitsSize;
   HANDLE             hDIB;
   LPSTR              pDIB;


   /* get length of DIB in bytes for use when reading */
   #ifdef S4WIN32
     dwBitsSize = file4longGetLo( u4filelength ((HANDLE)hFile) ) ;
   #else
     dwBitsSize = u4filelength (hFile);
   #endif
   /* Go read the DIB file header and check if it's valid. */

   if ((_lread (hFile, (LPSTR) &bmfHeader, sizeof (bmfHeader)) != sizeof (bmfHeader)) ||
        (bmfHeader.bfType != DIB_HEADER_MARKER))
      {
/*      DIBError (ERR_NOT_DIB); */
      return (HANDLE)NULL;
      }

   /* Allocate memory for DIB */

   hDIB = GlobalAlloc (GMEM_MOVEABLE | GMEM_ZEROINIT, dwBitsSize - sizeof(BITMAPFILEHEADER));

   if (hDIB == 0)
     {
/*     DIBError (ERR_MEMORY); */
     return (HANDLE)NULL;
     }

   pDIB = (LPSTR)GlobalLock (hDIB);

   /* Go read the bits. */

   if (!MyRead (hFile, pDIB, dwBitsSize - sizeof(BITMAPFILEHEADER)))
      {
      GlobalUnlock (hDIB);
      GlobalFree   (hDIB);
      return (HANDLE)NULL;
      }


   GlobalUnlock (hDIB);
   return hDIB;
}

/****************************************************************************

 FUNCTION   : lwrite(int fh, VOID FAR *pv, DWORD ul)

 PURPOSE    : Writes data in steps of 32k till all the data is written.

 RETURNS    : 0 - If write did not proceed correctly.
                 number of bytes written otherwise.

 ****************************************************************************/
DWORD PASCAL kwrite (int fh, VOID FAR *pv, DWORD ul)
{
   DWORD     ulT = ul;
   #ifndef S4WIN32
      BYTE huge *hp = (unsigned char huge *)pv;
   #else
      BYTE *hp = (BYTE*)pv;
   #endif
   while (ul > BYTES_PER_READ)
      {
      if (_lwrite(fh, (LPSTR)hp, (WORD)BYTES_PER_READ) != BYTES_PER_READ)
                   return 0;

      ul -= BYTES_PER_READ;
      hp += BYTES_PER_READ;
      }

   if (_lwrite(fh, (LPSTR)hp, (WORD)ul) != (WORD)ul)
      return 0;

   return ulT;
}


/****************************************************************************

 FUNCTION   : WriteDIB(LPSTR szFile,HANDLE hdib)

 PURPOSE    : Write a global handle in CF_DIB format to a file.

 RETURNS    : TRUE  - if successful.
              FALSE - otherwise

 ****************************************************************************/
BOOL S4FUNCTION bmp4WriteDIB (LPSTR szFile, HANDLE hdib)
{
    BITMAPFILEHEADER    hdr;
    LPBITMAPINFOHEADER  lpbi;
    int                 fh;
    OFSTRUCT            of;

    if (!hdib)
        return FALSE;

    fh = OpenFile (szFile, &of, OF_CREATE|OF_READWRITE);
    if (fh == -1)
        return FALSE;

    lpbi = (LPBITMAPINFOHEADER)GlobalLock (hdib);

    /* Fill in the fields of the file header */
    hdr.bfType          = DIB_HEADER_MARKER;
    hdr.bfSize          = GlobalSize (hdib) + sizeof (BITMAPFILEHEADER);
    hdr.bfReserved1     = 0;
    hdr.bfReserved2     = 0;
    hdr.bfOffBits       = (DWORD)sizeof(BITMAPFILEHEADER) + lpbi->biSize +
                          bmp4PaletteSize((LPSTR)lpbi);

    /* Write the file header */
    if (!_lwrite (fh, (LPSTR)&hdr, sizeof (BITMAPFILEHEADER)))
      {
      GlobalUnlock (hdib);
      _lclose (fh);
      return FALSE;
      }

    /* Write the DIB header and the bits */
    if (!kwrite (fh, (LPSTR)lpbi, GlobalSize (hdib)))
      {
      GlobalUnlock (hdib);
      _lclose (fh);
      return FALSE;
      }

    GlobalUnlock (hdib);
    _lclose (fh);
    return TRUE;
}
#endif
#endif   /* S4OFF_REPORT */
