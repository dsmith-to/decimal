/**
 ** author David B. Smith <dave.smith.to@gmail.com>
 ** copyright (C) 2009, David Smith
 ** doc
 **
 ** Created : 25 Oct 2008
 **
 ** Copyright (c) 2009 David B. Smith
 ** All rights reserved.
 **
 ** Redistribution and use in source and binary forms, with or without
 ** modification, are permitted provided that the following conditions
 ** are met:
 ** 1. Redistributions of source code must retain the above copyright
 **    notice, this list of conditions and the following disclaimer.
 ** 2. Redistributions in binary form must reproduce the above copyright
 **    notice, this list of conditions and the following disclaimer in the
 **    documentation and/or other materials provided with the distribution.
 ** 3. Neither the name of the copyright holder nor the names of contributors
 **    may be used to endorse or promote products derived from this software
 **    without specific prior written permission.
 ** 
 ** THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTOR(S) ``AS IS'' AND
 ** ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 ** IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 ** ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTOR(S) BE LIABLE
 ** FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 ** DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 ** OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 ** HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 ** LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 ** OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 ** SUCH DAMAGE.
 **/

/* decimal_drv.c */

#define DECNUMDIGITS 34

#include <stdio.h>
#include <string.h>
#include "ei.h"
#include "erl_driver.h"
#include "decNumber.h"
//#include "decimal128.h"
//#include "decQuad.h"
//#include "decPacked.h"

#define STRSZ  DECNUMDIGITS+14
//#define DIGITS DECQUAD_Pmax
//#define BCD_OFF 0

// Functions are provided by this driver
#define DRV_INFO_LIB    0
//conversions
#define DRV_FROM_LIST   1
#define DRV_TO_LIST     2 
#define DRV_FROM_FLOAT  3
#define DRV_TO_FLOAT    4 
#define DRV_FROM_PACKED 5
#define DRV_TO_PACKED   6
#define DRV_FROM_BCD    7
#define DRV_TO_BCD      8
//uniary ops
#define DRV_ABS         9
#define DRV_MINUS      10
#define DRV_EXP        11
#define DRV_LOG10      12
#define DRV_LN         13
#define DRV_SQRT       14
//bianry ops
#define DRV_ADD        15
#define DRV_SUBTRACT   16
#define DRV_MULTIPLY   17
#define DRV_DIVIDE     18
#define DRV_POWER      19
#define DRV_MIN        20
#define DRV_MAX        21
#define DRV_MINMAG     22
#define DRV_MAXMAG     23
 
static int init(void);
static ErlDrvData start(ErlDrvPort port, char *command);
static void stop(ErlDrvData drv_data);
static int control(ErlDrvData drv_data, unsigned int command, char *buf, 
                   int len, char **rbuf, int rlen); 

static ErlDrvEntry decimal_driver_entry = {
    init,
    start, 
    stop, 
    NULL,                       /* output */
    NULL,                       /* ready_input */
    NULL,                       /* ready_output */ 
    "decimal_drv", 
    NULL,                       /* finish */
    NULL,                       /* handle */
    control, 
    NULL,                       /* timeout */
    NULL,                       /* outputv */

    NULL,                       /* ready_async */
    NULL,                       /* flush */
    NULL,                       /* call */
    NULL,                       /* event */
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    0,
    NULL,                       /* handle2 */
    NULL                        /* process_exit */
};

DRIVER_INIT(decimal_drv)
{
    return &decimal_driver_entry;
}

static decContext decctxt;

static int init(void)
{
    decContextDefault(&decctxt, DEC_INIT_DECQUAD); // initialize 
    decctxt.traps=0; // no traps, thank you 
    decctxt.digits=DECNUMDIGITS; // set precision
    return 0; 
}

static ErlDrvData start(ErlDrvPort port, char *command)
{ 
    set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
    return 0; /* not used */
}

static void stop(ErlDrvData drv_data)
{
    return;
}

/* Since we are operating in binary mode, the return value from control
 * is irrelevant, as long as it is not negative.
 */
static int control(ErlDrvData drv_data, unsigned int command, char *buf, 
                   int len, char **rbuf, int rlen)
{
  char cbuf[STRSZ];
  const char* version;
  ErlDrvBinary *bin;
  int sz;
  int ret = -1;
  decNumber *pd; 
  decNumber *pdScale;
  decNumber d;

  //    printf("contol command: %d\n", command);
  switch(command) {
    
    case DRV_INFO_LIB:
      version = decNumberVersion();
      sz = strlen(version);
      *rbuf = (char*)(bin = driver_alloc_binary(sz));
      strncpy(bin->orig_bytes, version, sz);
      return sz;

    case DRV_FROM_LIST:
      *rbuf = (char*)(bin = driver_alloc_binary(sizeof(decNumber)));
      memset(bin->orig_bytes, 0, sizeof(decNumber));
      if (len >= STRSZ) {
        decNumberFromString((decNumber*)(bin->orig_bytes), "NaN", &decctxt);
      } else {
        strncpy(cbuf, buf, len);
        cbuf[len] = '\0';
        decNumberFromString((decNumber*)(bin->orig_bytes), cbuf, &decctxt);
      }
      return sizeof(decNumber);
    
    case DRV_TO_LIST:
      pd = (decNumber*)buf;
      if (len>sizeof(decNumber)) {
        pdScale = (decNumber*)(buf+sizeof(decNumber));
        decNumberRescale(pd, pd, pdScale, &decctxt);
      }
      decNumberToString(pd, cbuf);
      sz = strlen(cbuf);
      *rbuf = (char*)(bin = driver_alloc_binary(sz));
      strncpy(bin->orig_bytes, cbuf, sz);
      return sz;
      /*
    case DRV_TO_PACKED:
      *rbuf = (char*)(bin = driver_alloc_binary(DECQUAD_Pmax+2*sizeof(int32_t)));
      *(int32_t*)(bin->orig_bytes+sizeof(int32_t)) = decNumberToPacked((decNumber*)buf,
                          (int32_t*)(bin->orig_bytes), 
                          (uint8_t*)(bin->orig_bytes+2*sizeof(int32_t)));
      //printf("Exp: %d\n", *(int32_t*)(bin->orig_bytes));
      //printf("Sign: %d\n", *(int32_t*)(bin->orig_bytes+sizeof(int32_t)));
      return DECQUAD_Pmax+2*sizeof(int32_t);

    case DRV_TO_PACKED:
      decNumberToNumber((decNumber*)buf, &dn1);
      decNumberToNumber((decNumber*)(buf+sizeof(decNumber)), &dnScale);
      decNumberRescale(&dn1, &dn1, &dnScale, &decctxt);
      *rbuf = (char*)(bin = driver_alloc_binary(16+sizeof(int32_t)));
      decPackedFromNumber((uint8_t*)(bin->orig_bytes+sizeof(int32_t)), 16, 
                          (int32_t*)(bin->orig_bytes), &dn1);
      for (i=16; i>0; i--) {
      }
      return 16+sizeof(int32_t);
      	      
    case DRV_FROM_PACKED:
      *rbuf = (char*)(bin = driver_alloc_binary(sizeof(decNumber)));
      decPackedToNumber((decNumber*)(bin->orig_bytes), (decNumber*)buf, &decctxt);
      return sizeof(decNumber);
      */

    case DRV_FROM_BCD:
      *rbuf = (char*)(bin = driver_alloc_binary(sizeof(decNumber)));
      pd = (decNumber*)(bin->orig_bytes);
      memmove(&(pd->exponent), buf, 5);
      pd->digits=len-5;
      decNumberSetBCD(pd, ((uint8_t*)buf)+5, len-5);
      return sizeof(decNumber);

    case DRV_TO_BCD:
      pd = decNumberReduce(&d, (decNumber*)buf, &decctxt);
      sz = (pd->digits)+5;
      *rbuf = (char*)(bin = driver_alloc_binary(sz));
      memset(bin->orig_bytes, 0, sz);
      memmove(bin->orig_bytes, &(pd->exponent), 5);
      decNumberGetBCD(pd, (uint8_t*)(bin->orig_bytes+5));
      return sz;

    case DRV_ABS:
      *rbuf = (char*)(bin = driver_alloc_binary(sizeof(decNumber)));
      decNumberAbs((decNumber*)(bin->orig_bytes), (decNumber*)buf, &decctxt);
      return sizeof(decNumber);

    case DRV_MINUS:
      *rbuf = (char*)(bin = driver_alloc_binary(sizeof(decNumber)));
      decNumberMinus((decNumber*)(bin->orig_bytes), (decNumber*)buf, &decctxt);
      return sizeof(decNumber);

    case DRV_EXP:
      *rbuf = (char*)(bin = driver_alloc_binary(sizeof(decNumber)));
      decNumberExp((decNumber*)(bin->orig_bytes), (decNumber*)buf, &decctxt);
      return sizeof(decNumber);

    case DRV_LOG10:
      *rbuf = (char*)(bin = driver_alloc_binary(sizeof(decNumber)));
      decNumberLog10((decNumber*)(bin->orig_bytes), (decNumber*)buf, &decctxt);
      return sizeof(decNumber);

    case DRV_LN:
      *rbuf = (char*)(bin = driver_alloc_binary(sizeof(decNumber)));
      decNumberLn((decNumber*)(bin->orig_bytes), (decNumber*)buf, &decctxt);
      return sizeof(decNumber);

    case DRV_SQRT:
      *rbuf = (char*)(bin = driver_alloc_binary(sizeof(decNumber)));
      decNumberSquareRoot((decNumber*)(bin->orig_bytes), (decNumber*)buf, &decctxt);
      return sizeof(decNumber);

    case DRV_ADD:
      *rbuf = (char*)(bin = driver_alloc_binary(sizeof(decNumber)));
      decNumberAdd((decNumber*)(bin->orig_bytes), (decNumber*)buf, 
                   (decNumber*)(buf+sizeof(decNumber)), &decctxt);
      return sizeof(decNumber);

    case DRV_SUBTRACT:
      *rbuf = (char*)(bin = driver_alloc_binary(sizeof(decNumber)));
      decNumberSubtract((decNumber*)(bin->orig_bytes),(decNumber*)buf, 
                        (decNumber*)(buf+sizeof(decNumber)), &decctxt);
      return sizeof(decNumber);

    case DRV_MULTIPLY:
      *rbuf = (char*)(bin = driver_alloc_binary(sizeof(decNumber)));
      decNumberMultiply((decNumber*)(bin->orig_bytes), (decNumber*)buf, 
                        (decNumber*)(buf+sizeof(decNumber)), &decctxt);
      return sizeof(decNumber);

    case DRV_DIVIDE:
      *rbuf = (char*)(bin = driver_alloc_binary(sizeof(decNumber)));
      decNumberDivide((decNumber*)(bin->orig_bytes), (decNumber*)buf, 
                      (decNumber*)(buf+sizeof(decNumber)), &decctxt);
      return sizeof(decNumber);

    case DRV_POWER:
      *rbuf = (char*)(bin = driver_alloc_binary(sizeof(decNumber)));
      decNumberPower((decNumber*)(bin->orig_bytes), (decNumber*)buf, 
                      (decNumber*)(buf+sizeof(decNumber)), &decctxt);
      return sizeof(decNumber);

    case DRV_MIN:
      *rbuf = (char*)(bin = driver_alloc_binary(sizeof(decNumber)));
      decNumberMin((decNumber*)(bin->orig_bytes), (decNumber*)buf, 
                      (decNumber*)(buf+sizeof(decNumber)), &decctxt);
      return sizeof(decNumber);

    case DRV_MAX:
      *rbuf = (char*)(bin = driver_alloc_binary(sizeof(decNumber)));
      decNumberMax((decNumber*)(bin->orig_bytes), (decNumber*)buf, 
                      (decNumber*)(buf+sizeof(decNumber)), &decctxt);
      return sizeof(decNumber);

    case DRV_MINMAG:
      *rbuf = (char*)(bin = driver_alloc_binary(sizeof(decNumber)));
      decNumberMinMag((decNumber*)(bin->orig_bytes), (decNumber*)buf, 
                      (decNumber*)(buf+sizeof(decNumber)), &decctxt);
      return sizeof(decNumber);

    case DRV_MAXMAG:
      *rbuf = (char*)(bin = driver_alloc_binary(sizeof(decNumber)));
      decNumberMaxMag((decNumber*)(bin->orig_bytes), (decNumber*)buf, 
                      (decNumber*)(buf+sizeof(decNumber)), &decctxt);
      return sizeof(decNumber);

    }


    return ret;
}

