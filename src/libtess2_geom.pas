unit libtess2_geom;

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
** Author: Eric Veach, July 1994.
** Pascal port: Doj, 2019
*}

{$MODE FPC}
{$MODESWITCH RESULT}
{$MODESWITCH OUT}

interface

uses
  libtess2_structs;

function VertEq(u, v: PTESSvertex): Boolean; inline;
function VertLeq(u, v: PTESSvertex): Boolean; inline;

function EdgeEval(u, v, w: PTESSvertex): TESSreal; inline;
function EdgeSign(u, v, w: PTESSvertex): TESSreal; inline;

// Versions of VertLeq, EdgeSign, EdgeEval with s and t transposed.
function TransLeq(u, v: PTESSvertex): Boolean; inline;
function TransEval(u, v, w: PTESSvertex): TESSreal; inline;
function TransSign(u, v, w: PTESSvertex): TESSreal; inline;

function EdgeGoesLeft(e: PTESShalfEdge): Boolean; inline;
function EdgeGoesRight(e: PTESShalfEdge): Boolean; inline;
function EdgeIsInternal(e: PTESShalfEdge): Boolean; inline;

function VertL1dist(u, v: PTESSvertex): TESSreal; inline;

function VertCCW(u, v, w: PTESSvertex): Boolean; inline;

function tesvertLeq(u, v: PTESSvertex): Boolean; inline;
function tesedgeEval(u, v, w: PTESSvertex): TESSreal;
function tesedgeSign(u, v, w: PTESSvertex): TESSreal;
function testransEval(u, v, w: PTESSvertex): TESSreal;
function testransSign(u, v, w: PTESSvertex): TESSreal;
function tesvertCCW(u, v, w: PTESSvertex): Boolean;
procedure tesedgeIntersect(o1, d1, o2, d2, v: PTESSvertex);
function tesedgeIsLocallyDelaunay(e: PTESShalfEdge): Boolean;

implementation

{$IF Defined(FOR_TRITE_TEST_PROGRAM)}
uses
  libtess2_trite;
{$ENDIF}

{$IF Defined(NO_BRANCH_CONDITIONS)}
//  MIPS architecture has special instructions to evaluate boolean
// conditions -- more efficient than branching, IF you can get the
// compiler to generate the right instructions (SGI compiler doesn't)
//
function VertEq(u, v: PTESSvertex): Boolean;
begin
  Exit(Boolean(Ord(u^.s = v^.s) and Ord(u^.t == v^.t)));
end;

function VertLeq(u, v: PTESSvertex): Boolean;
begin
  Exit(Boolean(Ord(u^.s < v^.s) or (Ord(u^.s = v^.s) and Ord(u^.t <= v^.t)));
end;
{$ELSE}
function VertEq(u, v: PTESSvertex): Boolean;
begin
  Exit((u^.s = v^.s) and (u^.t = v^.t));
end;

function VertLeq(u, v: PTESSvertex): Boolean;
begin
  Exit((u^.s < v^.s) or ((u^.s = v^.s) and (u^.t <= v^.t)));
end;
{$ENDIF}

function EdgeEval(u, v, w: PTESSvertex): TESSreal;
begin
  Exit(tesedgeEval(u,v,w));
end;

function EdgeSign(u, v, w: PTESSvertex): TESSreal;
begin
  Exit(tesedgeSign(u,v,w));
end;

function TransLeq(u, v: PTESSvertex): Boolean;
begin
  Exit((u^.t < v^.t) or ((u^.t = v^.t) and (u^.s <= v^.s)));
end;

function TransEval(u, v, w: PTESSvertex): TESSreal;
begin
  Exit(testransEval(u, v, w));
end;

function TransSign(u, v, w: PTESSvertex): TESSreal;
begin
  Exit(testransSign(u, v, w));
end;

function EdgeGoesLeft(e: PTESShalfEdge): Boolean;
begin
  Exit(VertLeq(e^.Dst, e^.Org));
end;

function EdgeGoesRight(e: PTESShalfEdge): Boolean;
begin
  Exit(VertLeq(e^.Org, e^.Dst));
end;

function EdgeIsInternal(e: PTESShalfEdge): Boolean;
begin
  Exit((e^.Rface <> nil) and e^.Rface^.inside);
end;

function VertL1dist(u, v: PTESSvertex): TESSreal;
begin
  Exit(ABS(u^.s - v^.s) + ABS(u^.t - v^.t));
end;

function VertCCW(u, v, w: PTESSvertex): Boolean; inline;
begin
  Exit(tesvertCCW(u, v, w));
end;

function tesvertLeq(u, v: PTESSvertex): Boolean;
begin
  // Returns TRUE if u is lexicographically <= v.
  Exit(VertLeq(u, v));
end;

function tesedgeEval(u, v, w: PTESSvertex): TESSreal;
var
  gapL, gapR: TESSreal;
begin
  //  Given three vertices u,v,w such that VertLeq(u,v) && VertLeq(v,w),
  // evaluates the t-coord of the edge uw at the s-coord of the vertex v.
  // Returns v->t - (uw)(v->s), ie. the signed distance from uw to v.
  // If uw is vertical (and thus passes thru v), the result is zero.
  //
  // The calculation is extremely accurate and stable, even when v
  // is very close to u or w.  In particular if we set v->t = 0 and
  // let r be the negated result (this evaluates (uw)(v->s)), then
  // r is guaranteed to satisfy MIN(u->t,w->t) <= r <= MAX(u->t,w->t).
  //
  Assert(VertLeq(u, v) and VertLeq(v, w));

  gapL := v^.s - u^.s;
  gapR := w^.s - v^.s;

  if gapL + gapR > 0 then begin
    if gapL < gapR then begin
      Exit((v^.t - u^.t) + (u^.t - w^.t) * (gapL / (gapL + gapR)));
    end else begin
      Exit((v^.t - w^.t) + (w^.t - u^.t) * (gapR / (gapL + gapR)));
    end;
  end;
  // vertical line
  Exit(0);
end;

function tesedgeSign(u, v, w: PTESSvertex): TESSreal;
var
  gapL, gapR: TESSreal;
begin
  //  Returns a number whose sign matches EdgeEval(u,v,w) but which
  // is cheaper to evaluate.  Returns > 0, == 0 , or < 0
  // as v is above, on, or below the edge uw.
  //
  Assert(VertLeq(u, v) and VertLeq(v, w));

  gapL := v^.s - u^.s;
  gapR := w^.s - v^.s;

  if gapL + gapR > 0 then begin
    Exit((v^.t - w^.t) * gapL + (v^.t - u^.t) * gapR);
  end;
  // vertical line
  Exit(0);
end;

// **********************************************************************
// Define versions of EdgeSign, EdgeEval with s and t transposed.
//

function testransEval(u, v, w: PTESSvertex): TESSreal;
var
  gapL, gapR: TESSreal;
begin
  //  Given three vertices u,v,w such that TransLeq(u,v) && TransLeq(v,w),
  // evaluates the t-coord of the edge uw at the s-coord of the vertex v.
  // Returns v->s - (uw)(v->t), ie. the signed distance from uw to v.
  // If uw is vertical (and thus passes thru v), the result is zero.
  //
  // The calculation is extremely accurate and stable, even when v
  // is very close to u or w.  In particular if we set v->s = 0 and
  // let r be the negated result (this evaluates (uw)(v->t)), then
  // r is guaranteed to satisfy MIN(u->s,w->s) <= r <= MAX(u->s,w->s).
  //
  Assert(TransLeq(u, v) and TransLeq(v, w));

  gapL := v^.t - u^.t;
  gapR := w^.t - v^.t;

  if gapL + gapR > 0 then begin
    if gapL < gapR then begin
      Exit((v^.s - u^.s) + (u^.s - w^.s) * (gapL / (gapL + gapR)));
    end else begin
      Exit((v^.s - w^.s) + (w^.s - u^.s) * (gapR / (gapL + gapR)));
    end;
  end;
  // vertical line
  Exit(0);
end;

function testransSign(u, v, w: PTESSvertex): TESSreal;
var
  gapL, gapR: TESSreal;
begin
  //  Returns a number whose sign matches TransEval(u,v,w) but which
  // is cheaper to evaluate.  Returns > 0, == 0 , or < 0
  // as v is above, on, or below the edge uw.
  //
  Assert(TransLeq(u, v) and TransLeq(v, w));

  gapL := v^.t - u^.t;
  gapR := w^.t - v^.t;

  if gapL + gapR > 0 then begin
    Exit((v^.s - w^.s) * gapL + (v^.s - u^.s) * gapR);
  end;
  // vertical line
  Exit(0);
end;

function tesvertCCW(u, v, w: PTESSvertex): Boolean;
begin
  //  For almost-degenerate situations, the results are not reliable.
  // Unless the floating-point arithmetic can be performed without
  // rounding errors, *any* implementation will give incorrect results
  // on some degenerate inputs, so the client must have some way to
  // handle this situation.
  //
  Exit((u^.s*(v^.t - w^.t) + v^.s*(w^.t - u^.t) + w^.s*(u^.t - v^.t)) >= 0);
end;

//  Given parameters a,x,b,y returns the value (b*x+a*y)/(a+b),
// or (x+y)/2 if a==b==0.  It requires that a,b >= 0, and enforces
// this in the rare case that one argument is slightly negative.
// The implementation is extremely stable numerically.
// In particular it guarantees that the result r satisfies
// MIN(x,y) <= r <= MAX(x,y), and the results are very accurate
// even when a and b differ greatly in magnitude.
//
function RealInterpolate(a, x, b, y: Double): Double; inline;
begin
  if a < 0 then
    a := 0;
  if b < 0 then
    b := 0;
  if a <= b then begin
    if b = 0 then begin
      Exit((x + y) / 2);
    end else begin
      Exit(x + (y - x) * (a / (a + b)));
    end;
  end else begin
    Exit(y + (x - y) * (b / (a + b)));
  end;
end;

{$IF not Defined(FOR_TRITE_TEST_PROGRAM)}
function Interpolate(a, x, b, y: Double): Double; inline;
begin
  Exit(RealInterpolate(a, x, b, y));
end;
{$ELSE}

//  Claim: the ONLY property the sweep algorithm relies on is that
// MIN(x,y) <= r <= MAX(x,y).  This is a nasty way to test that.
//

function Interpolate(a, x, b, y: Double): Double; inline;
begin
  Writeln('*********************', RandomInterpolate);
  if RandomInterpolate then begin
    a := 1.2 * drand48() - 0.1;
    if a < 0 then begin
      a := 0;
    end else if a > 1 then begin
      a := 1;
    end;
    b := 1.0 - a;
  end;
  Exit(RealInterpolate(a, x, b, y));
end;

{$ENDIF}

procedure Swap(var a, b: PTESSvertex); inline;
var
  t: PTESSvertex;
begin
  t := a;
  a := b;
  b := t;
end;

procedure tesedgeIntersect(o1, d1, o2, d2, v: PTESSvertex);
            //  Given edges (o1,d1) and (o2,d2), compute their point of intersection.
            // The computed point is guaranteed to lie in the intersection of the
            // bounding rectangles defined by each edge.
            //
var
  z1, z2: TESSreal;
begin
  //  This is certainly not the most efficient way to find the intersection
  // of two line segments, but it is very numerically stable.
  //
  // Strategy: find the two middle vertices in the VertLeq ordering,
  // and interpolate the intersection s-value from these.  Then repeat
  // using the TransLeq ordering to find the intersection t-value.
  //

  if not VertLeq(o1, d1) then begin
    Swap(o1, d1);
  end;
  if not VertLeq(o2, d2) then begin
    Swap(o2, d2);
  end;
  if not VertLeq(o1, o2) then begin
    Swap(o1, o2);
    Swap(d1, d2);
  end;

  if not VertLeq(o2, d1) then begin
    // Technically, no intersection -- do our best
    v^.s := (o2^.s + d1^.s) / 2;
  end else if( VertLeq( d1, d2 )) then begin
    // Interpolate between o2 and d1
    z1 := EdgeEval(o1, o2, d1);
    z2 := EdgeEval(o2, d1, d2);
    if z1 + z2 < 0 then begin
      z1 := -z1;
      z2 := -z2;
    end;
    v^.s := Interpolate( z1, o2^.s, z2, d1^.s );
  end else begin
    // Interpolate between o2 and d2
    z1 :=  EdgeSign(o1, o2, d1);
    z2 := -EdgeSign(o1, d2, d1);
    if z1 + z2 < 0 then begin
      z1 := -z1;
      z2 := -z2;
    end;
    v^.s := Interpolate(z1, o2^.s, z2, d2^.s);
  end;

  // Now repeat the process for t

  if not TransLeq(o1, d1) then begin
    Swap(o1, d1);
  end;
  if not TransLeq(o2, d2) then begin
    Swap(o2, d2);
  end;
  if not TransLeq(o1, o2) then begin
    Swap(o1, o2);
    Swap(d1, d2);
  end;

  if not TransLeq(o2, d1) then begin
    // Technically, no intersection -- do our best
    v^.t := (o2^.t + d1^.t) / 2;
  end else if TransLeq(d1, d2) then begin
    // Interpolate between o2 and d1
    z1 := TransEval(o1, o2, d1);
    z2 := TransEval(o2, d1, d2);
    if z1 + z2 < 0 then begin
      z1 := -z1;
      z2 := -z2;
    end;
    v^.t := Interpolate(z1, o2^.t, z2, d1^.t);
  end else begin
    // Interpolate between o2 and d2
    z1 := TransSign(o1, o2, d1);
    z2 := -TransSign(o1, d2, d1);
    if z1 + z2 < 0 then begin
      z1 := -z1;
      z2 := -z2;
    end;
    v^.t := Interpolate(z1, o2^.t, z2, d2^.t);
  end;
end;

function inCircle(v, v0, v1, v2: PTESSvertex): TESSreal;
var
  adx, ady, bdx, bdy, cdx, cdy: TESSreal;
  abdet, bcdet, cadet: TESSreal;
  alift, blift, clift: TESSreal;
begin
  adx := v0^.s - v^.s;
  ady := v0^.t - v^.t;
  bdx := v1^.s - v^.s;
  bdy := v1^.t - v^.t;
  cdx := v2^.s - v^.s;
  cdy := v2^.t - v^.t;

  abdet := adx * bdy - bdx * ady;
  bcdet := bdx * cdy - cdx * bdy;
  cadet := cdx * ady - adx * cdy;

  alift := adx * adx + ady * ady;
  blift := bdx * bdx + bdy * bdy;
  clift := cdx * cdx + cdy * cdy;

  Exit(alift * bcdet + blift * cadet + clift * abdet);
end;

//
// Returns 1 is edge is locally delaunay
//
function tesedgeIsLocallyDelaunay(e: PTESShalfEdge): Boolean;
begin
  Exit(inCircle(e^.Sym^.Lnext^.Lnext^.Org, e^.Lnext^.Org, e^.Lnext^.Lnext^.Org, e^.Org) < 0);
end;

end.
