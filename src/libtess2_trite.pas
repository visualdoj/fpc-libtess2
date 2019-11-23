unit libtess2_trite;

{*
** SGI FREE SOFTWARE LICENSE B (Version 2.0, Sept. 18, 2008)
** Copyright (C) [dates of first publication] Silicon Graphics, Inc.
** All Rights Reserved.
**
** Permission is hereby granted, free of charge, to any person obtaining a copy
** of this software and associated documentation files (the "Software"), to deal
** in the Software without restriction, including without limitation the rights
** to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
** of the Software, and to permit persons to whom the Software is furnished to do so,
** subject to the following conditions:
**
** The above copyright notice including the dates of first publication and either this
** permission notice or a reference to http://oss.sgi.com/projects/FreeB/ shall be
** included in all copies or substantial portions of the Software.
**
** THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
** INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
** PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL SILICON GRAPHICS, INC.
** BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
** TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE
** OR OTHER DEALINGS IN THE SOFTWARE.
**
** Except as contained in this notice, the name of Silicon Graphics, Inc. shall not
** be used in advertising or otherwise to promote the sale, use or other dealings in
** this Software without prior written authorization from Silicon Graphics, Inc.
*}
{*
** Author: Doj, 2019
*}

{$MODE FPC}
{$MODESWITCH RESULT}
{$MODESWITCH OUT}

interface

uses
  libtess2_structs;

const
  drand48_m = 1 shl 48;

type
  TDebugEvent = procedure (tess: PTESStesselator);

procedure DebugEventNOP(tess: PTESStesselator);

var
  RandomSweep: Boolean = False;
  RandomInterpolate: Boolean = False;

  drand48_a: UInt64 = $5DEECE66D;
  drand48_c: UInt64 = $B;
  drand48_seed: UInt64 = $330E;

  DebugEvent: TDebugEvent = @DebugEventNOP;

function drand48: Double;
procedure srand48(randval: UInt32);

implementation

procedure DebugEventNOP(tess: PTESStesselator);
begin
end;

function drand48: Double;
begin
  drand48_seed := (drand48_a * drand48_seed) mod drand48_m;
  Exit(drand48_seed / drand48_m);
end;

procedure srand48(randval: UInt32);
begin
  drand48_seed := (randval shl 16) or $330E;
end;

end.
