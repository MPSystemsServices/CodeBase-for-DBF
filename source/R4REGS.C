/* *********************************************************************************************** */
/* Copyright (C) 1999-2015 by Sequiter, Inc., 9644-54 Ave, NW, Suite 209, Edmonton, Alberta Canada.*/
/* This program is free software: you can redistribute it and/or modify it under the terms of      */
/* the GNU Lesser General Public License as published by the Free Software Foundation, version     */
/* 3 of the License.                                                                               */
/*                                                                                                 */
/* This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;       */
/* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.       */
/* See the GNU Lesser General Public License for more details.                                     */
/*                                                                                                 */
/* You should have received a copy of the GNU Lesser General Public License along with this        */
/* program. If not, see <https://www.gnu.org/licenses/>.                                           */
/* *********************************************************************************************** */

/* r4regs.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif  /* __TUROBC__ */
#endif  /* S4UNIX */

#ifdef S4REGISTERS
#ifndef S4STAND_ALONE
#ifndef S4COMFILE
#ifdef S4WINDOWS

/* DPMI 0x300 - simulate real mode interrupt handler */
int int4sim( unsigned int inter, DPMI4REGS *in, DPMI4REGS *out)
{
   DPMI4REGS regs ;

   memcpy( &regs, in, sizeof( DPMI4REGS ) ) ;
   regs.ss = regs.sp = 0 ;
   _asm
   {
      PUSHF
      POP word ptr[regs.asm4flags]
      mov ax, word ptr[regs]
      push DI
      push ES
      mov AX, 0x0300
      mov BX, inter
      xor CX, CX
      push DS
      pop ES
      lea DI, regs
      int 0x31
      pop ES
      pop DI
   }

   memcpy( out, &regs, sizeof( DPMI4REGS ) ) ;
   return regs.un.x.ax ;
}

int int4realFar( unsigned int inter, DPMI4REGS *in, DPMI4REGS *out)
{
   DPMI4REGS regs ;

   memcpy( &regs, in, sizeof( DPMI4REGS ) ) ;
   regs.ss = regs.sp = 0 ;
   _asm
   {
      mov AX, 0x0301
      mov BX, inter
      xor CX, CX
      push DS
      pop ES
      lea DI, regs
      int 0x31
      pop ES
      pop DI
   }

   memcpy( out, &regs, sizeof( DPMI4REGS ) ) ;
   return regs.un.x.ax ;
}

int int4x( unsigned int inter, DPMI4REGS *in, DPMI4REGS *out, DPMI4REGS *seg)
{
   DPMI4REGS regs ;

   memcpy( &regs, in, sizeof( DPMI4REGS ) ) ;
   regs.ss = regs.sp = 0 ;
   _asm
   {
      PUSHF
      POP word ptr[regs.asm4flags]
      push DI
      push ES
      mov AX, 0x0300
      mov BX, inter
      xor CX, CX
      push DS
      pop ES
      lea DI, regs
      int 0x31
      pop ES
      pop DI
   }

   memcpy( out, &regs, sizeof( DPMI4REGS ) ) ;
   return regs.un.x.ax ;
}

int int4allocCallback( void (far *protectedAddress)(), void (far* *retAddress)() )
{
   DPMI4REGS regs ;

   *retAddress = 0 ;
   memset( &regs, 0, sizeof( DPMI4REGS ) ) ;
   _asm
   {
      push DI
      push ES
      push DS
      push SI
      mov AX, 0x0303
      xor CX, CX
      push DS
      pop ES
      lea DI, regs
      mov si, word ptr protectedAddress
      mov ds, word ptr protectedAddress.2
      int 0x31
      jc dpmi4error
      lds bx, retAddress
      mov word ptr [bx], dx
      mov word ptr [bx].2, cx
      xor ax, ax
   dpmi4error:
      pop SI
      pop DS
      pop ES
      pop DI
   }

   if ( *retAddress == 0 )
      return -1 ;
   else
      return 0 ;
}

/*
void int4freeCallback( void *realAddress )
{
   DPMI4REGS regs ;

   memset( &regs, 0, sizeof( DPMI4REGS ) ) ;
   _asm
   {
      PUSHF
      POP regs.asm4flags
      push DI
      push ES
      mov AX, 0x0300
      mov BX, inter
      xor CX, CX
      push DS
      pop ES
      lea DI, regs
      int 0x31
      pop ES
      pop DI
   }
}
*/

#endif /* S4WINDOWS */
#endif /* S4COMFILE */
#endif /* S4STAND_ALONE */
#endif /* S4REGISTERS */
