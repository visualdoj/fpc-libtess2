unit libtess2_sweep;

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
  libtess2_structs,
  libtess2_bucketalloc,
  libtess2_dict,
  libtess2_geom,
  libtess2_mesh,
  libtess2_priorityq;

//  tessComputeInterior( tess ) computes the planar arrangement specified
// by the given contours, and further subdivides this arrangement
// into regions.  Each region is marked "inside" if it belongs
// to the polygon, according to the rule given by tess->windingRule.
// Each interior region is guaranteed be monotone.
//
function tessComputeInterior(tess: PTESStesselator): Boolean;

// The following is here *only* for access by debugging routines
//uses dict

function RegionBelow(r: PActiveRegion): PActiveRegion; inline;
function RegionAbove(r: PActiveRegion): PActiveRegion; inline;

implementation

{$IF Defined(FOR_TRITE_TEST_PROGRAM)}
uses
  libtess2_trite;
{$ENDIF}

function RegionBelow(r: PActiveRegion): PActiveRegion; inline;
begin
  Exit(PActiveRegion(dictKey(dictPred(r^.nodeUp))));
end;

function RegionAbove(r: PActiveRegion): PActiveRegion; inline;
begin
  Exit(PActiveRegion(dictKey(dictSucc(r^.nodeUp))));
end;

{$IF not Defined(FOR_TRITE_TEST_PROGRAM)}
procedure DebugEvent(tess: PTESStesselator); inline;
begin
end;
{$ENDIF}

//
// Invariants for the Edge Dictionary.
// - each pair of adjacent edges e2=Succ(e1) satisfies EdgeLeq(e1,e2)
//   at any valid location of the sweep event
// - if EdgeLeq(e2,e1) as well (at any valid sweep event), then e1 and e2
//   share a common endpoint
// - for each e, e->Dst has been processed, but not e->Org
// - each edge e satisfies VertLeq(e->Dst,event) && VertLeq(event,e->Org)
//   where "event" is the current sweep line event.
// - no edge e has zero length
//
// Invariants for the Mesh (the processed portion).
// - the portion of the mesh left of the sweep line is a planar graph,
//   ie. there is *some* way to embed it in the plane
// - no processed edge has zero length
// - no two processed vertices have identical coordinates
// - each "inside" region is monotone, ie. can be broken into two chains
//   of monotonically increasing vertices according to VertLeq(v1,v2)
//   - a non-invariant: these chains may intersect (very slightly)
//
// Invariants for the Sweep.
// - if none of the edges incident to the event vertex have an activeRegion
//   (ie. none of these edges are in the edge dictionary), then the vertex
//   has only right-going edges.
// - if an edge is marked "fixUpperEdge" (it is a temporary edge introduced
//   by ConnectRightVertex), then it is the only right-going edge from
//   its associated vertex.  (This says that these edges exist only
//   when it is necessary.)
//

function MAX(x, y: TESSreal): TESSreal; inline;
begin
  if x > y then begin
    Exit(x);
  end else
    Exit(y);
end;

function MAXInt(x, y: PtrInt): PtrInt; inline;
begin
  if x > y then begin
    Exit(x);
  end else
    Exit(y);
end;

function MIN(x, y: TESSreal): TESSreal; inline;
begin
  if x < y then begin
    Exit(x);
  end else
    Exit(y);
end;

//  When we merge two edges into one, we need to compute the combined
// winding of the new edge.
//
procedure AddWinding(eDst, eSrc: PTESShalfEdge);
begin
  Inc(eDst^.winding, eSrc^.winding);
  Inc(eDst^.Sym^.winding, eSrc^.Sym^.winding);
end;

procedure SweepEvent(tess: PTESStesselator; vEvent: PTESSvertex); forward;
procedure WalkDirtyRegions(tess: PTESStesselator; regUp: PActiveRegion); forward;
function CheckForRightSplice(tess: PTESStesselator; regUp: PActiveRegion): Boolean; forward;

function EdgeLeq(tess: Pointer; reg1, reg2: TDictKey): Boolean;
//
// Both edges must be directed from right to left (this is the canonical
// direction for the upper edge of each region).
//
// The strategy is to evaluate a "t" value for each edge at the
// current sweep line position, given by tess->event.  The calculations
// are designed to be very stable, but of course they are not perfect.
//
// Special case: if both edge destinations are at the sweep event,
// we sort the edges by slope (they would otherwise compare equally).
//
var
  event: PTESSvertex;
  e1, e2: PTESShalfEdge;
  t1, t2: TESSreal;
begin
  Result := False; // make compiler happy

  event := PTESStesselator(tess)^.event;

  e1 := PActiveRegion(reg1)^.eUp;
  e2 := PActiveRegion(reg2)^.eUp;

  if e1^.Dst = event then begin
    if e2^.Dst = event then begin
      //  Two edges right of the sweep line which meet at the sweep event.
      // Sort them by slope.
      //
      if VertLeq(e1^.Org, e2^.Org) then begin
        Exit(EdgeSign(e2^.Dst, e1^.Org, e2^.Org) <= 0);
      end;
      Exit(EdgeSign(e1^.Dst, e2^.Org, e1^.Org) >= 0);
    end;
    Exit(EdgeSign(e2^.Dst, event, e2^.Org) <= 0);
  end;
  if e2^.Dst = event then begin
    Exit(EdgeSign(e1^.Dst, event, e1^.Org) >= 0);
  end;

  // General case - compute signed distance *from* e1, e2 to event
  t1 := EdgeEval(e1^.Dst, event, e1^.Org);
  t2 := EdgeEval(e2^.Dst, event, e2^.Org);
  Exit(t1 >= t2);
end;


procedure DeleteRegion(tess: PTESStesselator; reg: PActiveRegion);
begin
  if reg^.fixUpperEdge then begin
    //  It was created with zero winding number, so it better be
    // deleted with zero winding number (ie. it better not get merged
    // with a real edge).
    //
    Assert(reg^.eUp^.winding = 0);
  end;
  reg^.eUp^.activeRegion := nil;
  dictDelete(tess^.dict, reg^.nodeUp);
  bucketFree(tess^.regionPool, reg);
end;


function FixUpperEdge(tess: PTESStesselator; reg: PActiveRegion; newEdge: PTESShalfEdge): Boolean;
//
// Replace an upper edge which needs fixing (see ConnectRightVertex).
//
begin
  Assert(reg^.fixUpperEdge);
  if not tessMeshDelete(tess^.mesh, reg^.eUp) then
    Exit(False);
  reg^.fixUpperEdge := FALSE;
  reg^.eUp := newEdge;
  newEdge^.activeRegion := reg;

  Exit(True);
end;

function TopLeftRegion(tess: PTESStesselator; reg: PActiveRegion): PActiveRegion;
var
  org: PTESSvertex;
  e: PTESShalfEdge;
begin
  org := reg^.eUp^.Org;

  // Find the region above the uppermost edge with the same origin
  repeat
    reg := RegionAbove(reg);
  until reg^.eUp^.Org <> org;

  //  If the edge above was a temporary edge introduced by ConnectRightVertex,
  // now is the time to fix it.
  //
  if reg^.fixUpperEdge then begin
    e := tessMeshConnect(tess^.mesh, RegionBelow(reg)^.eUp^.Sym, reg^.eUp^.Lnext);
    if e = nil then
      Exit(nil);
    if not FixUpperEdge(tess, reg, e) then
      Exit(nil);
    reg := RegionAbove(reg);
  end;
  Exit(reg);
end;

function TopRightRegion(reg: PActiveRegion): PActiveRegion;
var
  dst: PTESSvertex;
begin
  dst := reg^.eUp^.Dst;

  // Find the region above the uppermost edge with the same destination
  repeat
    reg := RegionAbove(reg);
  until reg^.eUp^.Dst <> dst;
  Exit(reg);
end;

function AddRegionBelow(tess: PTESStesselator;
                  regAbove: PActiveRegion;
                  eNewUp: PTESShalfEdge): PActiveRegion;
//
// Add a new active region to the sweep line, *somewhere* below "regAbove"
// (according to where the new edge belongs in the sweep-line dictionary).
// The upper edge of the new region will be "eNewUp".
// Winding number and "inside" flag are not updated.
//
var
  regNew: PActiveRegion;
begin
  regNew := PActiveRegion(bucketAlloc(tess^.regionPool));
  if regNew = nil then
    longjmp(tess^.env, 1);

  regNew^.eUp := eNewUp;
  regNew^.nodeUp := dictInsertBefore(tess^.dict, regAbove^.nodeUp, regNew);
  if regNew^.nodeUp = nil then
    longjmp(tess^.env, 1);
  regNew^.fixUpperEdge := False;
  regNew^.sentinel := False;
  regNew^.dirty := False;

  eNewUp^.activeRegion := regNew;
  Exit(regNew);
end;

function IsWindingInside(tess: PTESStesselator; n: PtrInt): Boolean;
begin
  case tess^.windingRule of
    TESS_WINDING_ODD: Exit(n and 1 <> 0);
    TESS_WINDING_NONZERO: Exit(n <> 0);
    TESS_WINDING_POSITIVE: Exit(n > 0);
    TESS_WINDING_NEGATIVE: Exit(n < 0);
    TESS_WINDING_ABS_GEQ_TWO: Exit((n >= 2) or (n <= -2));
  end;
  //LINTED
  Assert(False);
  //NOTREACHED

  Exit(False);
end;


procedure ComputeWinding(tess: PTESStesselator; reg: PActiveRegion);
begin
  reg^.windingNumber := RegionAbove(reg)^.windingNumber + reg^.eUp^.winding;
  reg^.inside := IsWindingInside(tess, reg^.windingNumber);
end;


procedure FinishRegion(tess: PTESStesselator; reg: PActiveRegion);
//
// Delete a region from the sweep line.  This happens when the upper
// and lower chains of a region meet (at a vertex on the sweep line).
// The "inside" flag is copied to the appropriate mesh face (we could
// not do this before -- since the structure of the mesh is always
// changing, this face may not have even existed until now).
//
var
  e: PTESShalfEdge;
  f: PTESSface;
begin
  e := reg^.eUp;
  f := e^.Lface;

  f^.inside := reg^.inside;
  f^.anEdge := e;   // optimization for tessMeshTessellateMonoRegion()
  DeleteRegion(tess, reg);
end;


function FinishLeftRegions(tess: PTESStesselator;
                           regFirst, regLast: PActiveRegion): PTESShalfEdge;
//
// We are given a vertex with one or more left-going edges.  All affected
// edges should be in the edge dictionary.  Starting at regFirst->eUp,
// we walk down deleting all regions where both edges have the same
// origin vOrg.  At the same time we copy the "inside" flag from the
// active region to the face, since at this point each face will belong
// to at most one region (this was not necessarily true until this point
// in the sweep).  The walk stops at the region above regLast; if regLast
// is NULL we walk as far as possible.  At the same time we relink the
// mesh if necessary, so that the ordering of edges around vOrg is the
// same as in the dictionary.
//
var
  reg, regPrev: PActiveRegion;
  e, ePrev: PTESShalfEdge;
begin
  regPrev := regFirst;
  ePrev := regFirst^.eUp;
  while regPrev <> regLast do begin
    regPrev^.fixUpperEdge := False;  // placement was OK
    reg := RegionBelow(regPrev);
    e := reg^.eUp;
    if e^.Org <> ePrev^.Org then begin
      if not reg^.fixUpperEdge then begin
        //  Remove the last left-going edge.  Even though there are no further
        // edges in the dictionary with this origin, there may be further
        // such edges in the mesh (if we are adding left edges to a vertex
        // that has already been processed).  Thus it is important to call
        // FinishRegion rather than just DeleteRegion.
        //
        FinishRegion(tess, regPrev);
        break;
      end;
      //  If the edge below was a temporary edge introduced by
      // ConnectRightVertex, now is the time to fix it.
      //
      e := tessMeshConnect(tess^.mesh, ePrev^.Lprev, e^.Sym);
      if e = nil then
        longjmp(tess^.env, 1);
      if not FixUpperEdge(tess, reg, e) then
        longjmp(tess^.env, 1);
    end;

    // Relink edges so that ePrev->Onext == e
    if ePrev^.Onext <> e then begin
      if not tessMeshSplice(tess^.mesh, e^.Oprev, e) then
        longjmp(tess^.env, 1);
      if not tessMeshSplice(tess^.mesh, ePrev, e) then
        longjmp(tess^.env, 1);
    end;
    FinishRegion(tess, regPrev);  // may change reg->eUp
    ePrev := reg^.eUp;
    regPrev := reg;
  end;
  Exit(ePrev);
end;


procedure AddRightEdges(tess: PTESStesselator; regUp: PActiveRegion;
              eFirst, eLast, eTopLeft: PTESShalfEdge;
              cleanUp: Boolean);
//
// Purpose: insert right-going edges into the edge dictionary, and update
// winding numbers and mesh connectivity appropriately.  All right-going
// edges share a common origin vOrg.  Edges are inserted CCW starting at
// eFirst; the last edge inserted is eLast->Oprev.  If vOrg has any
// left-going edges already processed, then eTopLeft must be the edge
// such that an imaginary upward vertical segment from vOrg would be
// contained between eTopLeft->Oprev and eTopLeft; otherwise eTopLeft
// should be NULL.
//
var
  reg, regPrev: PActiveRegion;
  e, ePrev: PTESShalfEdge;
  firstTime: Boolean;
begin
  firstTime := True;

  // Insert the new right-going edges in the dictionary
  e := eFirst;
  repeat
    Assert(VertLeq(e^.Org, e^.Dst));
    AddRegionBelow(tess, regUp, e^.Sym);
    e := e^.Onext;
  until e = eLast;

  //  Walk *all* right-going edges from e->Org, in the dictionary order,
  // updating the winding numbers of each region, and re-linking the mesh
  // edges to match the dictionary ordering (if necessary).
  //
  if eTopLeft = nil then begin
    eTopLeft := RegionBelow(regUp)^.eUp^.Rprev;
  end;
  regPrev := regUp;
  ePrev := eTopLeft;
  while True do begin
    reg := RegionBelow(regPrev);
    e := reg^.eUp^.Sym;
    if e^.Org <> ePrev^.Org then
      break;

    if e^.Onext <> ePrev then begin
      // Unlink e from its current position, and relink below ePrev
      if not tessMeshSplice(tess^.mesh, e^.Oprev, e) then
        longjmp(tess^.env, 1);
      if not tessMeshSplice(tess^.mesh, ePrev^.Oprev, e) then
        longjmp(tess^.env, 1);
    end;
    // Compute the winding number and "inside" flag for the new regions
    reg^.windingNumber := regPrev^.windingNumber - e^.winding;
    reg^.inside := IsWindingInside(tess, reg^.windingNumber);

    //  Check for two outgoing edges with same slope -- process these
    // before any intersection tests (see example in tessComputeInterior).
    //
    regPrev^.dirty := True;
    if (not firstTime) and CheckForRightSplice(tess, regPrev) then begin
      AddWinding(e, ePrev);
      DeleteRegion(tess, regPrev);
      if not tessMeshDelete(tess^.mesh, ePrev) then
        longjmp(tess^.env, 1);
    end;
    firstTime := False;
    regPrev := reg;
    ePrev := e;
  end;
  regPrev^.dirty := True;
  Assert(regPrev^.windingNumber - e^.winding = reg^.windingNumber);

  if cleanUp then begin
    // Check for intersections between newly adjacent edges.
    WalkDirtyRegions(tess, regPrev);
  end;
end;


procedure SpliceMergeVertices(tess: PTESStesselator; e1, e2: PTESShalfEdge);
//
// Two vertices with idential coordinates are combined into one.
// e1->Org is kept, while e2->Org is discarded.
//
begin
  if not tessMeshSplice(tess^.mesh, e1, e2) then
    longjmp(tess^.env, 1);
end;

procedure VertexWeights(isect, org, dst: PTESSvertex;
                        weights: PTESSreal);
//
// Find some weights which describe how the intersection vertex is
// a linear combination of "org" and "dest".  Each of the two edges
// which generated "isect" is allocated 50% of the weight; each edge
// splits the weight between its org and dst according to the
// relative distance to "isect".
//
var
  t1, t2: TESSreal;
begin
  t1 := VertL1dist(org, isect);
  t2 := VertL1dist(dst, isect);

  weights[0] := TESSreal(0.5 * t2 / (t1 + t2));
  weights[1] := TESSreal(0.5 * t1 / (t1 + t2));
  isect^.coords[0] := isect^.coords[0] + weights[0]*org^.coords[0] + weights[1]*dst^.coords[0];
  isect^.coords[1] := isect^.coords[1] + weights[0]*org^.coords[1] + weights[1]*dst^.coords[1];
  isect^.coords[2] := isect^.coords[2] + weights[0]*org^.coords[2] + weights[1]*dst^.coords[2];
end;


procedure GetIntersectData(tess: PTESStesselator;
               isect, orgUp, dstUp, orgLo, dstLo: PTESSvertex);
 //
 // We've computed a new intersection point, now we need a "data" pointer
 // from the user so that we can refer to this new vertex in the
 // rendering callbacks.
 //
var
  weights: array[0 .. 3] of TESSreal;
begin
  TESS_NOTUSED(tess);

  isect^.coords[0] := 0;
  isect^.coords[1] := 0;
  isect^.coords[2] := 0;
  isect^.idx := TESS_UNDEF;
  VertexWeights(isect, orgUp, dstUp, @weights[0]);
  VertexWeights(isect, orgLo, dstLo, @weights[2]);
end;

function CheckForRightSplice(tess: PTESStesselator; regUp: PActiveRegion): Boolean;
//
// Check the upper and lower edge of "regUp", to make sure that the
// eUp->Org is above eLo, or eLo->Org is below eUp (depending on which
// origin is leftmost).
//
// The main purpose is to splice right-going edges with the same
// dest vertex and nearly identical slopes (ie. we can't distinguish
// the slopes numerically).  However the splicing can also help us
// to recover from numerical errors.  For example, suppose at one
// point we checked eUp and eLo, and decided that eUp->Org is barely
// above eLo.  Then later, we split eLo into two edges (eg. from
// a splice operation like this one).  This can change the result of
// our test so that now eUp->Org is incident to eLo, or barely below it.
// We must correct this condition to maintain the dictionary invariants.
//
// One possibility is to check these edges for intersection again
// (ie. CheckForIntersect).  This is what we do if possible.  However
// CheckForIntersect requires that tess->event lies between eUp and eLo,
// so that it has something to fall back on when the intersection
// calculation gives us an unusable answer.  So, for those cases where
// we can't check for intersection, this routine fixes the problem
// by just splicing the offending vertex into the other edge.
// This is a guaranteed solution, no matter how degenerate things get.
// Basically this is a combinatorial solution to a numerical problem.
//
var
  regLo: PActiveRegion;
  eUp, eLo: PTESShalfEdge;
begin
  regLo := RegionBelow(regUp);
  eUp := regUp^.eUp;
  eLo := regLo^.eUp;

  if VertLeq(eUp^.Org, eLo^.Org) then begin
    if EdgeSign(eLo^.Dst, eUp^.Org, eLo^.Org) > 0 then
      Exit(False);

    // eUp->Org appears to be below eLo
    if not VertEq(eUp^.Org, eLo^.Org) then begin
      // Splice eUp->Org into eLo
      if tessMeshSplitEdge(tess^.mesh, eLo^.Sym) = nil then
        longjmp(tess^.env, 1);
      if not tessMeshSplice(tess^.mesh, eUp, eLo^.Oprev) then
        longjmp(tess^.env, 1);
      regLo^.dirty := True;
      regUp^.dirty := True;

    end else if eUp^.Org <> eLo^.Org then begin
      // merge the two vertices, discarding eUp->Org
      pqDelete(tess^.pq, eUp^.Org^.pqHandle);
      SpliceMergeVertices(tess, eLo^.Oprev, eUp);
    end;
  end else begin
    if EdgeSign(eUp^.Dst, eLo^.Org, eUp^.Org) <= 0 then
      Exit(False);

    // eLo->Org appears to be above eUp, so splice eLo->Org into eUp
    RegionAbove(regUp)^.dirty := True;
    regUp^.dirty := True;
    if tessMeshSplitEdge(tess^.mesh, eUp^.Sym) = nil then
      longjmp(tess^.env, 1);
    if not tessMeshSplice(tess^.mesh, eLo^.Oprev, eUp) then
      longjmp(tess^.env, 1);
  end;
  Exit(True);
end;

function CheckForLeftSplice(tess: PTESStesselator; regUp: PActiveRegion): Boolean;
//
// Check the upper and lower edge of "regUp", to make sure that the
// eUp->Dst is above eLo, or eLo->Dst is below eUp (depending on which
// destination is rightmost).
//
// Theoretically, this should always be true.  However, splitting an edge
// into two pieces can change the results of previous tests.  For example,
// suppose at one point we checked eUp and eLo, and decided that eUp->Dst
// is barely above eLo.  Then later, we split eLo into two edges (eg. from
// a splice operation like this one).  This can change the result of
// the test so that now eUp->Dst is incident to eLo, or barely below it.
// We must correct this condition to maintain the dictionary invariants
// (otherwise new edges might get inserted in the wrong place in the
// dictionary, and bad stuff will happen).
//
// We fix the problem by just splicing the offending vertex into the
// other edge.
//
var
  regLo: PActiveRegion;
  eUp: PTESShalfEdge;
  eLo: PTESShalfEdge;
  e: PTESShalfEdge;
begin
  regLo := RegionBelow(regUp);
  eUp := regUp^.eUp;
  eLo := regLo^.eUp;

  Assert(not VertEq(eUp^.Dst, eLo^.Dst));

  if VertLeq(eUp^.Dst, eLo^.Dst) then begin
    if EdgeSign(eUp^.Dst, eLo^.Dst, eUp^.Org) < 0 then
      Exit(False);

    // eLo->Dst is above eUp, so splice eLo->Dst into eUp
    RegionAbove(regUp)^.dirty := True;
    regUp^.dirty := True;
    e := tessMeshSplitEdge(tess^.mesh, eUp);
    if e = nil then
      longjmp(tess^.env, 1);
    if not tessMeshSplice(tess^.mesh, eLo^.Sym, e) then
      longjmp(tess^.env,1);
    e^.Lface^.inside := regUp^.inside;
  end else begin
    if EdgeSign(eLo^.Dst, eUp^.Dst, eLo^.Org) > 0 then
      Exit(False);

    // eUp->Dst is below eLo, so splice eUp->Dst into eLo
    regUp^.dirty := True;
    regLo^.dirty := True;
    e := tessMeshSplitEdge(tess^.mesh, eLo);
    if e = nil then
      longjmp(tess^.env, 1);
    if not tessMeshSplice(tess^.mesh, eUp^.Lnext, eLo^.Sym) then
      longjmp(tess^.env,1);
    e^.Rface^.inside := regUp^.inside;
  end;
  Exit(True);
end;


function CheckForIntersect(tess: PTESStesselator; regUp: PActiveRegion): Boolean;
//
// Check the upper and lower edges of the given region to see if
// they intersect.  If so, create the intersection and add it
// to the data structures.
//
// Returns TRUE if adding the new intersection resulted in a recursive
// call to AddRightEdges(); in this case all "dirty" regions have been
// checked for intersections, and possibly regUp has been deleted.
//
var
  regLo: PActiveRegion;
  eUp: PTESShalfEdge;
  eLo: PTESShalfEdge;
  orgUp: PTESSvertex;
  orgLo: PTESSvertex;
  dstUp: PTESSvertex;
  dstLo: PTESSvertex;
  tMinUp, tMaxLo: TESSreal;
  isect: TESSvertex;
  orgMin: PTESSvertex;
  e: PTESShalfEdge;
begin
  Result := False; // make compiler happy

  regLo := RegionBelow(regUp);
  eUp := regUp^.eUp;
  eLo := regLo^.eUp;
  orgUp := eUp^.Org;
  orgLo := eLo^.Org;
  dstUp := eUp^.Dst;
  dstLo := eLo^.Dst;

  Assert(not VertEq(dstLo, dstUp));
  Assert(EdgeSign(dstUp, tess^.event, orgUp) <= 0);
  Assert(EdgeSign(dstLo, tess^.event, orgLo) >= 0);
  Assert((orgUp <> tess^.event) and (orgLo <> tess^.event));
  Assert((not regUp^.fixUpperEdge) and (not regLo^.fixUpperEdge));

  if orgUp = orgLo then
    Exit(False);  // right endpoints are the same

  tMinUp := MIN(orgUp^.t, dstUp^.t);
  tMaxLo := MAX(orgLo^.t, dstLo^.t);
  if tMinUp > tMaxLo then
    Exit(False);  // t ranges do not overlap

  if VertLeq(orgUp, orgLo) then begin
    if EdgeSign(dstLo, orgUp, orgLo) > 0 then
      Exit(False);
  end else begin
    if EdgeSign(dstUp, orgLo, orgUp) < 0 then
      Exit(False);
  end;

  // At this point the edges intersect, at least marginally
  DebugEvent(tess);

  tesedgeIntersect(dstUp, orgUp, dstLo, orgLo, @isect);
  // The following properties are guaranteed:
  Assert(MIN(orgUp^.t, dstUp^.t) <= isect.t);
  Assert(isect.t <= MAX(orgLo^.t, dstLo^.t));
  Assert(MIN(dstLo^.s, dstUp^.s) <= isect.s);
  Assert(isect.s <= MAX(orgLo^.s, orgUp^.s));

  if VertLeq(@isect, tess^.event) then begin
    //  The intersection point lies slightly to the left of the sweep line,
    // so move it until it''s slightly to the right of the sweep line.
    // (If we had perfect numerical precision, this would never happen
    // in the first place).  The easiest and safest thing to do is
    // replace the intersection by tess->event.
    //
    isect.s := tess^.event^.s;
    isect.t := tess^.event^.t;
  end;
  //  Similarly, if the computed intersection lies to the right of the
  // rightmost origin (which should rarely happen), it can cause
  // unbelievable inefficiency on sufficiently degenerate inputs.
  // (If you have the test program, try running test54.d with the
  // "X zoom" option turned on).
  //
  if VertLeq(orgUp, orgLo) then begin
    orgMin := orgUp;
  end else
    orgMin := orgLo;
  if VertLeq(orgMin, @isect) then begin
    isect.s := orgMin^.s;
    isect.t := orgMin^.t;
  end;

  if VertEq(@isect, orgUp) or VertEq(@isect, orgLo) then begin
    // Easy case -- intersection at one of the right endpoints
    CheckForRightSplice(tess, regUp);
    Exit(False);
  end;

  if ((not VertEq(dstUp, tess^.event))
    and (EdgeSign(dstUp, tess^.event, @isect) >= 0))
    or ((not VertEq(dstLo, tess^.event))
    and (EdgeSign(dstLo, tess^.event, @isect) <= 0))
  then begin
    //  Very unusual -- the new upper or lower edge would pass on the
    // wrong side of the sweep event, or through it.  This can happen
    // due to very small numerical errors in the intersection calculation.
    //
    if dstLo = tess^.event then begin
      // Splice dstLo into eUp, and process the new region(s)
      if tessMeshSplitEdge(tess^.mesh, eUp^.Sym) = nil then
        longjmp(tess^.env, 1);
      if not tessMeshSplice(tess^.mesh, eLo^.Sym, eUp) then
        longjmp(tess^.env, 1);
      regUp := TopLeftRegion(tess, regUp);
      if regUp = nil then
        longjmp(tess^.env, 1);
      eUp := RegionBelow(regUp)^.eUp;
      FinishLeftRegions(tess, RegionBelow(regUp), regLo);
      AddRightEdges(tess, regUp, eUp^.Oprev, eUp, eUp, True);
      Exit(True);
    end;
    if dstUp = tess^.event then begin
      // Splice dstUp into eLo, and process the new region(s)
      if tessMeshSplitEdge(tess^.mesh, eLo^.Sym) = nil then
        longjmp(tess^.env, 1);
      if not tessMeshSplice(tess^.mesh, eUp^.Lnext, eLo^.Oprev) then
        longjmp(tess^.env, 1);
      regLo := regUp;
      regUp := TopRightRegion(regUp);
      e := RegionBelow(regUp)^.eUp^.Rprev;
      regLo^.eUp := eLo^.Oprev;
      eLo := FinishLeftRegions(tess, regLo, nil);
      AddRightEdges(tess, regUp, eLo^.Onext, eUp^.Rprev, e, True);
      Exit(True);
    end;
    //  Special case: called from ConnectRightVertex.  If either
    // edge passes on the wrong side of tess->event, split it
    // (and wait for ConnectRightVertex to splice it appropriately).
    //
    if EdgeSign(dstUp, tess^.event, @isect) >= 0 then begin
      regUp^.dirty := True;
      RegionAbove(regUp)^.dirty := True;
      if tessMeshSplitEdge(tess^.mesh, eUp^.Sym) = nil then
        longjmp(tess^.env, 1);
      eUp^.Org^.s := tess^.event^.s;
      eUp^.Org^.t := tess^.event^.t;
    end;
    if EdgeSign(dstLo, tess^.event, @isect) <= 0 then begin
      regLo^.dirty := True;
      regUp^.dirty := True;
      if tessMeshSplitEdge(tess^.mesh, eLo^.Sym) = nil then
        longjmp(tess^.env, 1);
      eLo^.Org^.s := tess^.event^.s;
      eLo^.Org^.t := tess^.event^.t;
    end;
    // leave the rest for ConnectRightVertex
    Exit(False);
  end;

  //  General case -- split both edges, splice into new vertex.
  // When we do the splice operation, the order of the arguments is
  // arbitrary as far as correctness goes.  However, when the operation
  // creates a new face, the work done is proportional to the size of
  // the new face.  We expect the faces in the processed part of
  // the mesh (ie. eUp->Lface) to be smaller than the faces in the
  // unprocessed original contours (which will be eLo->Oprev->Lface).
  //
  if tessMeshSplitEdge(tess^.mesh, eUp^.Sym) = nil then
    longjmp(tess^.env, 1);
  if tessMeshSplitEdge(tess^.mesh, eLo^.Sym) = nil then
    longjmp(tess^.env, 1);
  if not tessMeshSplice(tess^.mesh, eLo^.Oprev, eUp) then
    longjmp(tess^.env, 1);
  eUp^.Org^.s := isect.s;
  eUp^.Org^.t := isect.t;
  eUp^.Org^.pqHandle := pqInsert(@tess^.alloc, tess^.pq, eUp^.Org);
  if eUp^.Org^.pqHandle = INV_HANDLE then begin
    pqDeletePriorityQ(@tess^.alloc, tess^.pq);
    tess^.pq := nil;
    longjmp(tess^.env, 1);
  end;
  GetIntersectData(tess, eUp^.Org, orgUp, dstUp, orgLo, dstLo);
  regLo^.dirty := True;
  regUp^.dirty := True;
  RegionAbove(regUp)^.dirty := True;
  Exit(False);
end;

procedure WalkDirtyRegions(tess: PTESStesselator; regUp: PActiveRegion);
//
// When the upper or lower edge of any region changes, the region is
// marked "dirty".  This routine walks through all the dirty regions
// and makes sure that the dictionary invariants are satisfied
// (see the comments at the beginning of this file).  Of course
// new dirty regions can be created as we make changes to restore
// the invariants.
//
var
  regLo: PActiveRegion;
  eUp, eLo: PTESShalfEdge;
begin
  regLo := RegionBelow(regUp);

  while True do begin
    // Find the lowest dirty region (we walk from the bottom up).
    while regLo^.dirty do begin
      regUp := regLo;
      regLo := RegionBelow(regLo);
    end;
    if not regUp^.dirty then begin
      regLo := regUp;
      regUp := RegionAbove(regUp);
      if (regUp = nil) or not regUp^.dirty then begin
        // We've walked all the dirty regions
        Exit;
      end;
    end;
    regUp^.dirty := FALSE;
    eUp := regUp^.eUp;
    eLo := regLo^.eUp;

    if eUp^.Dst <> eLo^.Dst then begin
      // Check that the edge ordering is obeyed at the Dst vertices.
      if CheckForLeftSplice(tess, regUp) then begin

        //  If the upper or lower edge was marked fixUpperEdge, then
        // we no longer need it (since these edges are needed only for
        // vertices which otherwise have no right-going edges).
        //
        if regLo^.fixUpperEdge then begin
          DeleteRegion( tess, regLo );
          if not tessMeshDelete(tess^.mesh, eLo) then
            longjmp(tess^.env, 1);
          regLo := RegionBelow(regUp);
          eLo := regLo^.eUp;
        end else if regUp^.fixUpperEdge then begin
          DeleteRegion(tess, regUp);
          if not tessMeshDelete(tess^.mesh, eUp) then
            longjmp(tess^.env, 1);
          regUp := RegionAbove(regLo);
          eUp := regUp^.eUp;
        end;
      end;
    end;
    if eUp^.Org <> eLo^.Org then begin
      if (eUp^.Dst <> eLo^.Dst)
        and (not regUp^.fixUpperEdge) and (not regLo^.fixUpperEdge)
        and ((eUp^.Dst = tess^.event) or (eLo^.Dst = tess^.event))
      then begin
        //  When all else fails in CheckForIntersect(), it uses tess->event
        // as the intersection location.  To make this possible, it requires
        // that tess->event lie between the upper and lower edges, and also
        // that neither of these is marked fixUpperEdge (since in the worst
        // case it might splice one of these edges into tess->event, and
        // violate the invariant that fixable edges are the only right-going
        // edge from their associated vertex).
        //
        if CheckForIntersect(tess, regUp) then begin
          // WalkDirtyRegions() was called recursively; we're done
          Exit;
        end;
      end else begin
        //  Even though we can't use CheckForIntersect(), the Org vertices
        // may violate the dictionary edge ordering.  Check and correct this.
        //
        CheckForRightSplice(tess, regUp);
      end;
    end;
    if (eUp^.Org = eLo^.Org) and (eUp^.Dst = eLo^.Dst) then begin
      // A degenerate loop consisting of only two edges -- delete it.
      AddWinding(eLo, eUp);
      DeleteRegion(tess, regUp);
      if not tessMeshDelete(tess^.mesh, eUp) then
        longjmp(tess^.env, 1);
      regUp := RegionAbove(regLo);
    end;
  end;
end;


procedure ConnectRightVertex(tess: PTESStesselator; regUp: PActiveRegion;
                             eBottomLeft: PTESShalfEdge);
//
// Purpose: connect a "right" vertex vEvent (one where all edges go left)
// to the unprocessed portion of the mesh.  Since there are no right-going
// edges, two regions (one above vEvent and one below) are being merged
// into one.  "regUp" is the upper of these two regions.
//
// There are two reasons for doing this (adding a right-going edge):
//  - if the two regions being merged are "inside", we must add an edge
//    to keep them separated (the combined region would not be monotone).
//  - in any case, we must leave some record of vEvent in the dictionary,
//    so that we can merge vEvent with features that we have not seen yet.
//    For example, maybe there is a vertical edge which passes just to
//    the right of vEvent; we would like to splice vEvent into this edge.
//
// However, we don't want to connect vEvent to just any vertex.  We don''t
// want the new edge to cross any other edges; otherwise we will create
// intersection vertices even when the input data had no self-intersections.
// (This is a bad thing; if the user's input data has no intersections,
// we don't want to generate any false intersections ourselves.)
//
// Our eventual goal is to connect vEvent to the leftmost unprocessed
// vertex of the combined region (the union of regUp and regLo).
// But because of unseen vertices with all right-going edges, and also
// new vertices which may be created by edge intersections, we don''t
// know where that leftmost unprocessed vertex is.  In the meantime, we
// connect vEvent to the closest vertex of either chain, and mark the region
// as "fixUpperEdge".  This flag says to delete and reconnect this edge
// to the next processed vertex on the boundary of the combined region.
// Quite possibly the vertex we connected to will turn out to be the
// closest one, in which case we won''t need to make any changes.
//
var
  eNew: PTESShalfEdge;
  eTopLeft: PTESShalfEdge;
  regLo: PActiveRegion;
  eUp: PTESShalfEdge;
  eLo: PTESShalfEdge;
  degenerate: Boolean;
begin
  eTopLeft := eBottomLeft^.Onext;
  regLo := RegionBelow(regUp);
  eUp := regUp^.eUp;
  eLo := regLo^.eUp;
  degenerate := False;

  if eUp^.Dst <> eLo^.Dst then begin
    CheckForIntersect(tess, regUp);
  end;

  //  Possible new degeneracies: upper or lower edge of regUp may pass
  // through vEvent, or may coincide with new intersection vertex
  //
  if VertEq(eUp^.Org, tess^.event) then begin
    if not tessMeshSplice(tess^.mesh, eTopLeft^.Oprev, eUp) then
      longjmp(tess^.env, 1);
    regUp := TopLeftRegion(tess, regUp);
    if regUp = nil then
      longjmp(tess^.env, 1);
    eTopLeft := RegionBelow(regUp)^.eUp;
    FinishLeftRegions(tess, RegionBelow(regUp), regLo);
    degenerate := True;
  end;
  if VertEq(eLo^.Org, tess^.event) then begin
    if not tessMeshSplice(tess^.mesh, eBottomLeft, eLo^.Oprev) then
      longjmp(tess^.env, 1);
    eBottomLeft := FinishLeftRegions(tess, regLo, nil);
    degenerate := True;
  end;
  if degenerate then begin
    AddRightEdges(tess, regUp, eBottomLeft^.Onext, eTopLeft, eTopLeft, True);
    Exit;
  end;

  //  Non-degenerate situation -- need to add a temporary, fixable edge.
  // Connect to the closer of eLo->Org, eUp->Org.
  //
  if VertLeq(eLo^.Org, eUp^.Org) then begin
    eNew := eLo^.Oprev;
  end else begin
    eNew := eUp;
  end;
  eNew := tessMeshConnect(tess^.mesh, eBottomLeft^.Lprev, eNew);
  if eNew = nil then
    longjmp(tess^.env, 1);

  //  Prevent cleanup, otherwise eNew might disappear before we've even
  // had a chance to mark it as a temporary edge.
  //
  AddRightEdges(tess, regUp, eNew, eNew^.Onext, eNew^.Onext, False);
  eNew^.Sym^.activeRegion^.fixUpperEdge := True;
  WalkDirtyRegions(tess, regUp);
end;

//  Because vertices at exactly the same location are merged together
// before we process the sweep event, some degenerate cases can't occur.
// However if someone eventually makes the modifications required to
// merge features which are close together, the cases below marked
// TOLERANCE_NONZERO will be useful.  They were debugged before the
// code to merge identical vertices in the main loop was added.
//
const TOLERANCE_NONZERO = False;

procedure ConnectLeftDegenerate(tess: PTESStesselator;
              regUp: PActiveRegion; vEvent: PTESSvertex);
//
// The event vertex lies exacty on an already-processed edge or vertex.
// Adding the new vertex involves splicing it into the already-processed
// part of the mesh.
//
var
  e, eTopLeft, eTopRight, eLast: PTESShalfEdge;
  reg: PActiveRegion;
begin
  e := regUp^.eUp;
  if VertEq(e^.Org, vEvent) then begin
    //  e->Org is an unprocessed vertex - just combine them, and wait
    // for e->Org to be pulled from the queue
    //
    Assert(TOLERANCE_NONZERO);
    SpliceMergeVertices(tess, e, vEvent^.anEdge);
    Exit;
  end;

  if not VertEq(e^.Dst, vEvent) then begin
    // General case -- splice vEvent into edge e which passes through it
    if tessMeshSplitEdge(tess^.mesh, e^.Sym) = nil then
      longjmp(tess^.env, 1);
    if regUp^.fixUpperEdge then begin
      // This edge was fixable -- delete unused portion of original edge
      if not tessMeshDelete(tess^.mesh, e^.Onext) then
        longjmp(tess^.env, 1);
      regUp^.fixUpperEdge := False;
    end;
    if not tessMeshSplice(tess^.mesh, vEvent^.anEdge, e) then
      longjmp(tess^.env, 1);
    SweepEvent(tess, vEvent);  // recurse
    Exit;
  end;

  //  vEvent coincides with e->Dst, which has already been processed.
  // Splice in the additional right-going edges.
  //
  Assert(TOLERANCE_NONZERO);
  regUp := TopRightRegion(regUp);
  reg := RegionBelow( regUp );
  eTopRight := reg^.eUp^.Sym;
  eTopLeft := eTopRight^.Onext;
  eLast := eTopRight^.Onext;
  if reg^.fixUpperEdge then begin
    //  Here e->Dst has only a single fixable edge going right.
    // We can delete it since now we have some real right-going edges.
    //
    Assert(eTopLeft <> eTopRight);   // there are some left edges too
    DeleteRegion(tess, reg);
    if not tessMeshDelete(tess^.mesh, eTopRight) then
      longjmp(tess^.env, 1);
    eTopRight := eTopLeft^.Oprev;
  end;
  if not tessMeshSplice(tess^.mesh, vEvent^.anEdge, eTopRight) then
    longjmp(tess^.env, 1);
  if not EdgeGoesLeft(eTopLeft) then begin
    // e->Dst had no left-going edges -- indicate this to AddRightEdges()
    eTopLeft := nil;
  end;
  AddRightEdges(tess, regUp, eTopRight^.Onext, eLast, eTopLeft, True);
end;


procedure ConnectLeftVertex(tess: PTESStesselator; vEvent: PTESSvertex);
//
// Purpose: connect a "left" vertex (one where both edges go right)
// to the processed portion of the mesh.  Let R be the active region
// containing vEvent, and let U and L be the upper and lower edge
// chains of R.  There are two possibilities:
//
// - the normal case: split R into two regions, by connecting vEvent to
//   the rightmost vertex of U or L lying to the left of the sweep line
//
// - the degenerate case: if vEvent is close enough to U or L, we
//   merge vEvent into that edge chain.  The subcases are:
//  - merging with the rightmost vertex of U or L
//  - merging with the active edge of U or L
//  - merging with an already-processed portion of U or L
//
var
  regUp, regLo, reg: PActiveRegion;
  eUp, eLo, eNew: PTESShalfEdge;
  temp: TActiveRegion;
  tempHalfEdge: PTESShalfEdge;
begin
  // Assert( vEvent->anEdge->Onext->Onext == vEvent->anEdge );

  // Get a pointer to the active region containing vEvent
  temp.eUp := vEvent^.anEdge^.Sym;
  // __GL_DICTLISTKEY tessDictListSearch
  regUp := PActiveRegion(dictKey(dictSearch(tess^.dict, @temp)));
  regLo := RegionBelow(regUp);
  if regLo = nil then begin
    // This may happen if the input polygon is coplanar.
    Exit;
  end;
  eUp := regUp^.eUp;
  eLo := regLo^.eUp;

  // Try merging with U or L first
  if EdgeSign(eUp^.Dst, vEvent, eUp^.Org) = 0 then begin
    ConnectLeftDegenerate(tess, regUp, vEvent);
    Exit;
  end;

  //  Connect vEvent to rightmost processed vertex of either chain.
  // e->Dst is the vertex that we will connect to vEvent.
  //
  if VertLeq(eLo^.Dst, eUp^.Dst) then begin
    reg := regUp;
  end else
    reg := regLo;

  if regUp^.inside or reg^.fixUpperEdge then begin
    if reg = regUp then begin
      eNew := tessMeshConnect(tess^.mesh, vEvent^.anEdge^.Sym, eUp^.Lnext);
      if eNew = nil then
        longjmp(tess^.env, 1);
    end else begin
      tempHalfEdge := tessMeshConnect(tess^.mesh, eLo^.Dnext, vEvent^.anEdge);
      if tempHalfEdge = nil then
        longjmp(tess^.env, 1);

      eNew := tempHalfEdge^.Sym;
    end;
    if reg^.fixUpperEdge then begin
      if not FixUpperEdge(tess, reg, eNew) then
        longjmp(tess^.env, 1);
    end else begin
      ComputeWinding(tess, AddRegionBelow(tess, regUp, eNew));
    end;
    SweepEvent(tess, vEvent);
  end else begin
    //  The new vertex is in a region which does not belong to the polygon.
    // We don''t need to connect this vertex to the rest of the mesh.
    //
    AddRightEdges(tess, regUp, vEvent^.anEdge, vEvent^.anEdge, nil, True);
  end;
end;


procedure SweepEvent(tess: PTESStesselator; vEvent: PTESSvertex);
//
// Does everything necessary when the sweep line crosses a vertex.
// Updates the mesh and the edge dictionary.
//
var
  regUp, reg: PActiveRegion;
  e, eTopLeft, eBottomLeft: PTESShalfEdge;
begin
  tess^.event := vEvent;    // for access in EdgeLeq()
  DebugEvent(tess);

  //  Check if this vertex is the right endpoint of an edge that is
  // already in the dictionary.  In this case we don't need to waste
  // time searching for the location to insert new edges.
  //
  e := vEvent^.anEdge;
  while e^.activeRegion = nil do begin
    e := e^.Onext;
    if e = vEvent^.anEdge then begin
      // All edges go right -- not incident to any processed edges
      ConnectLeftVertex(tess, vEvent);
      Exit;;
    end;
  end;

  //  Processing consists of two phases: first we "finish" all the
  // active regions where both the upper and lower edges terminate
  // at vEvent (ie. vEvent is closing off these regions).
  // We mark these faces "inside" or "outside" the polygon according
  // to their winding number, and delete the edges from the dictionary.
  // This takes care of all the left-going edges from vEvent.
  //
  regUp := TopLeftRegion(tess, e^.activeRegion);
  if regUp = nil then
    longjmp(tess^.env, 1);
  reg := RegionBelow(regUp);
  eTopLeft := reg^.eUp;
  eBottomLeft := FinishLeftRegions(tess, reg, nil);

  //  Next we process all the right-going edges from vEvent.  This
  // involves adding the edges to the dictionary, and creating the
  // associated "active regions" which record information about the
  // regions between adjacent dictionary edges.
  //
  if eBottomLeft^.Onext = eTopLeft then begin
    // No right-going edges -- add a temporary "fixable" edge
    ConnectRightVertex(tess, regUp, eBottomLeft);
  end else begin
    AddRightEdges(tess, regUp, eBottomLeft^.Onext, eTopLeft, eTopLeft, True);
  end;
end;


//  Make the sentinel coordinates big enough that they will never be
// merged with real input features.
//

procedure AddSentinel(tess: PTESStesselator; smin, smax, t: TESSreal);
//
// We add two sentinel edges above and below all other edges,
// to avoid special cases at the top and bottom.
//
var
  e: PTESShalfEdge;
  reg: PActiveRegion;
begin
  reg := PActiveRegion(bucketAlloc(tess^.regionPool));
  if reg = nil then
    longjmp(tess^.env, 1);

  e := tessMeshMakeEdge(tess^.mesh);
  if e = nil then
    longjmp(tess^.env, 1);

  e^.Org^.s := smax;
  e^.Org^.t := t;
  e^.Dst^.s := smin;
  e^.Dst^.t := t;
  tess^.event := e^.Dst;    // initialize it

  reg^.eUp := e;
  reg^.windingNumber := 0;
  reg^.inside := False;
  reg^.fixUpperEdge := False;
  reg^.sentinel := True;
  reg^.dirty := False;
  reg^.nodeUp := dictInsert(tess^.dict, reg);
  if reg^.nodeUp = nil then
    longjmp(tess^.env, 1);
end;


procedure InitEdgeDict(tess: PTESStesselator);
//
// We maintain an ordering of edge intersections with the sweep line.
// This order is maintained in a dynamic dictionary.
//
var
  w, h: TESSreal;
  smin, smax, tmin, tmax: TESSreal;
begin
  tess^.dict := dictNewDict(@tess^.alloc, tess, @EdgeLeq);
  if tess^.dict = nil then
    longjmp(tess^.env, 1);

  // If the bbox is empty, ensure that sentinels are not coincident by slightly enlarging it.
  w := (tess^.bmax[0] - tess^.bmin[0]) + TESSreal(0.01);
  h := (tess^.bmax[1] - tess^.bmin[1]) + TESSreal(0.01);

  smin := tess^.bmin[0] - w;
  smax := tess^.bmax[0] + w;
  tmin := tess^.bmin[1] - h;
  tmax := tess^.bmax[1] + h;

  AddSentinel(tess, smin, smax, tmin);
  AddSentinel(tess, smin, smax, tmax);
end;


procedure DoneEdgeDict(tess: PTESStesselator);
var
  reg: PActiveRegion;
  fixedEdges: LongInt;
begin
  fixedEdges := 0;

  reg := PActiveRegion(dictKey(dictMin(tess^.dict)));
  while reg <> nil do begin
    //
    // At the end of all processing, the dictionary should contain
    // only the two sentinel edges, plus at most one "fixable" edge
    // created by ConnectRightVertex().
    //
    if not reg^.sentinel then begin
      Assert(reg^.fixUpperEdge);
      Inc(fixedEdges);
      Assert(fixedEdges = 1);
    end;
    Assert(reg^.windingNumber = 0);
    DeleteRegion(tess, reg);
    //    tessMeshDelete( reg^.eUp );
    reg := PActiveRegion(dictKey(dictMin(tess^.dict)));
  end;
  dictDeleteDict(@tess^.alloc, tess^.dict);
end;


procedure RemoveDegenerateEdges(tess: PTESStesselator);
//
// Remove zero-length edges, and contours with fewer than 3 vertices.
//
var
  e, eNext, eLnext: PTESShalfEdge;
  eHead: PTESShalfEdge;
begin
  eHead := @tess^.mesh^.eHead;

  //LINTED
  e := eHead^.next;
  while e <> eHead do begin
    eNext := e^.next;
    eLnext := e^.Lnext;

    if VertEq(e^.Org, e^.Dst) and (e^.Lnext^.Lnext <> e) then begin
      // Zero-length edge, contour has at least 3 edges

      SpliceMergeVertices(tess, eLnext, e);  // deletes e->Org
      if not tessMeshDelete(tess^.mesh, e) then
        longjmp(tess^.env, 1); // e is a self-loop
      e := eLnext;
      eLnext := e^.Lnext;
    end;
    if eLnext^.Lnext = e then begin
      // Degenerate contour (one or two edges)

      if eLnext <> e then begin
        if (eLnext = eNext) or (eLnext = eNext^.Sym) then begin
          eNext := eNext^.next;
        end;
        if not tessMeshDelete(tess^.mesh, eLnext) then
          longjmp(tess^.env, 1);
      end;
      if (e = eNext) or (e = eNext^.Sym) then begin
        eNext := eNext^.next;
      end;
      if not tessMeshDelete(tess^.mesh, e) then
        longjmp(tess^.env, 1);
    end;
    e := eNext;
  end;
end;

function InitPriorityQ(tess: PTESStesselator): Boolean;
//
// Insert all vertices into the priority queue which determines the
// order in which vertices cross the sweep line.
//
var
  pq: PPriorityQ;
  v, vHead: PTESSvertex;
  vertexCount: PtrInt;
begin
  vertexCount := 0;

  vHead := @tess^.mesh^.vHead;
  v := vHead^.next;
  while v <> vHead do begin
    Inc(vertexCount);
    v := v^.next;
  end;
  // Make sure there is enough space for sentinels.
  Inc(vertexCount, MAXInt(8, tess^.alloc.extraVertices));

  pq := pqNewPriorityQ(@tess^.alloc, vertexCount, TPriorityQLeq(@tesvertLeq));
  tess^.pq := pq;
  if pq = nil then
    Exit(False);

  vHead := @tess^.mesh^.vHead;
  v := vHead^.next;
  while v <> vHead do begin
    v^.pqHandle := pqInsert(@tess^.alloc, pq, v);
    if v^.pqHandle = INV_HANDLE then
      break;
    v := v^.next
  end;
  if (v <> vHead) or not pqInit(@tess^.alloc, pq) then begin
    pqDeletePriorityQ(@tess^.alloc, tess^.pq);
    tess^.pq := nil;
    Exit(False);
  end;

  Exit(True);
end;


procedure DonePriorityQ(tess: PTESStesselator);
begin
  pqDeletePriorityQ(@tess^.alloc, tess^.pq);
end;


function RemoveDegenerateFaces(tess: PTESStesselator; mesh: PTESSmesh): Boolean;
//
// Delete any degenerate faces with only two edges.  WalkDirtyRegions()
// will catch almost all of these, but it won't catch degenerate faces
// produced by splice operations on already-processed edges.
// The two places this can happen are in FinishLeftRegions(), when
// we splice in a "temporary" edge produced by ConnectRightVertex(),
// and in CheckForLeftSplice(), where we splice already-processed
// edges to ensure that our dictionary invariants are not violated
// by numerical errors.
//
// In both these cases it is *very* dangerous to delete the offending
// edge at the time, since one of the routines further up the stack
// will sometimes be keeping a pointer to that edge.
//
var
  f, fNext: PTESSface;
  e: PTESShalfEdge;
begin
  //LINTED
  f := mesh^.fHead.next;
  while f <> @mesh^.fHead do begin
    fNext := f^.next;
    e := f^.anEdge;
    Assert(e^.Lnext <> e);

    if e^.Lnext^.Lnext = e then begin
      // A face with only two edges
      AddWinding(e^.Onext, e);
      if not tessMeshDelete(tess^.mesh, e) then
        Exit(False);
    end;

    f := fNext;
  end;
  Exit(True);
end;

function tessComputeInterior(tess: PTESStesselator): Boolean;
//
// tessComputeInterior( tess ) computes the planar arrangement specified
// by the given contours, and further subdivides this arrangement
// into regions.  Each region is marked "inside" if it belongs
// to the polygon, according to the rule given by tess->windingRule.
// Each interior region is guaranteed be monotone.
//
var
  v, vNext: PTESSvertex;
begin
  //  Each vertex defines an event for our sweep line.  Start by inserting
  // all the vertices in a priority queue.  Events are processed in
  // lexicographic order, ie.
  //
  //  e1 < e2  iff  e1.x < e2.x || (e1.x == e2.x && e1.y < e2.y)
  //
  RemoveDegenerateEdges(tess);
  if not InitPriorityQ(tess) then
    Exit(False); // if error
  InitEdgeDict(tess);

  v := PTESSvertex(pqExtractMin(tess^.pq));
  while v <> nil do begin
    while True do begin
      vNext := PTESSvertex(pqMinimum(tess^.pq));
      if (vNext = nil) or not VertEq(vNext, v) then
        break;

      //  Merge together all vertices at exactly the same location.
      // This is more efficient than processing them one at a time,
      // simplifies the code (see ConnectLeftDegenerate), and is also
      // important for correct handling of certain degenerate cases.
      // For example, suppose there are two identical edges A and B
      // that belong to different contours (so without this code they would
      // be processed by separate sweep events).  Suppose another edge C
      // crosses A and B from above.  When A is processed, we split it
      // at its intersection point with C.  However this also splits C,
      // so when we insert B we may compute a slightly different
      // intersection point.  This might leave two edges with a small
      // gap between them.  This kind of error is especially obvious
      // when using boundary extraction (TESS_BOUNDARY_ONLY).
      //
      vNext := PTESSvertex(pqExtractMin(tess^.pq));
      SpliceMergeVertices(tess, v^.anEdge, vNext^.anEdge);
    end;
    SweepEvent(tess, v);
    v := PTESSvertex(pqExtractMin(tess^.pq));
  end;

  // Set tess->event for debugging purposes
  tess^.event := (PActiveRegion(dictKey(dictMin(tess^.dict))))^.eUp^.Org;
  DebugEvent(tess);
  DoneEdgeDict(tess);
  DonePriorityQ(tess);

  if not RemoveDegenerateFaces(tess, tess^.mesh) then
    Exit(False);
  tessMeshCheckMesh(tess^.mesh);

  Exit(True);
end;

end.
