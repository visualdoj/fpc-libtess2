unit libtess2_tesselator;

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
*/}
{*
** pascal port:     Doj, 2019
** tesselator.h:    Mikko Mononen, July 2009.
** tess.h, tess.c:  Eric Veach, July 1994.
*}

{$MODE FPC}
{$MODESWITCH RESULT}
{$MODESWITCH OUT}

interface

uses
  libtess2_structs,
  libtess2_mesh,
  libtess2_geom,
  libtess2_bucketalloc,
  libtess2_sweep;

// See OpenGL Red Book for description of the winding rules
// http://www.glprogramming.com/red/chapter11.html
type
  TTessWindingRule = libtess2_structs.TTessWindingRule;

const
  TESS_WINDING_ODD          = TTessWindingRule.TESS_WINDING_ODD;
  TESS_WINDING_NONZERO      = TTessWindingRule.TESS_WINDING_NONZERO;
  TESS_WINDING_POSITIVE     = TTessWindingRule.TESS_WINDING_POSITIVE;
  TESS_WINDING_NEGATIVE     = TTessWindingRule.TESS_WINDING_NEGATIVE;
  TESS_WINDING_ABS_GEQ_TWO  = TTessWindingRule.TESS_WINDING_ABS_GEQ_TWO;

  TESS_UNDEF  = libtess2_structs.TESS_UNDEF;

type
// The contents of the tessGetElements() depends on element type being passed to tessTesselate().
// Tesselation result element types:
// TESS_POLYGONS
//   Each element in the element array is polygon defined as 'polySize' number of vertex indices.
//   If a polygon has than 'polySize' vertices, the remaining indices are stored as TESS_UNDEF.
//   Example, drawing a polygon:
//     const int nelems = tessGetElementCount(tess);
//     const TESSindex* elems = tessGetElements(tess);
//     for (int i = 0; i < nelems; i++) {
//         const TESSindex* poly = &elems[i * polySize];
//         glBegin(GL_POLYGON);
//         for (int j = 0; j < polySize; j++) {
//             if (poly[j] == TESS_UNDEF) break;
//             glVertex2fv(&verts[poly[j]*vertexSize]);
//         }
//         glEnd();
//     }
//
// TESS_CONNECTED_POLYGONS
//   Each element in the element array is polygon defined as 'polySize' number of vertex indices,
//   followed by 'polySize' indices to neighour polygons, that is each element is 'polySize' * 2 indices.
//   If a polygon has than 'polySize' vertices, the remaining indices are stored as TESS_UNDEF.
//   If a polygon edge is a boundary, that is, not connected to another polygon, the neighbour index is TESS_UNDEF.
//   Example, flood fill based on seed polygon:
//     const int nelems = tessGetElementCount(tess);
//     const TESSindex* elems = tessGetElements(tess);
//     unsigned char* visited = (unsigned char*)calloc(nelems);
//     TESSindex stack[50];
//     int nstack = 0;
//     stack[nstack++] = seedPoly;
//     visited[startPoly] = 1;
//     while (nstack > 0) {
//          TESSindex idx = stack[--nstack];
//          const TESSindex* poly = &elems[idx * polySize * 2];
//          const TESSindex* nei = &poly[polySize];
//          for (int i = 0; i < polySize; i++) {
//              if (poly[i] == TESS_UNDEF) break;
//              if (nei[i] != TESS_UNDEF && !visited[nei[i]])
//                  stack[nstack++] = nei[i];
//                  visited[nei[i]] = 1;
//              }
//          }
//     }
//
// TESS_BOUNDARY_CONTOURS
//   Each element in the element array is [base index, count] pair defining a range of vertices for a contour.
//   The first value is index to first vertex in contour and the second value is number of vertices in the contour.
//   Example, drawing contours:
//     const int nelems = tessGetElementCount(tess);
//     const TESSindex* elems = tessGetElements(tess);
//     for (int i = 0; i < nelems; i++) {
//         const TESSindex base = elems[i * 2];
//         const TESSindex count = elems[i * 2 + 1];
//         glBegin(GL_LINE_LOOP);
//         for (int j = 0; j < count; j++) {
//             glVertex2fv(&verts[(base+j) * vertexSize]);
//         }
//         glEnd();
//     }
TTessElementType = (
  TESS_POLYGONS,
  TESS_CONNECTED_POLYGONS,
  TESS_BOUNDARY_CONTOURS
);


// TESS_CONSTRAINED_DELAUNAY_TRIANGULATION
//   If enabled, the initial triagulation is improved with non-robust Constrained Delayney triangulation.
//   Disable by default.
//
// TESS_REVERSE_CONTOURS
//   If enabled, tessAddContour() will treat CW contours as CCW and vice versa
//   Disabled by default.

type TTessOption = (
  TESS_CONSTRAINED_DELAUNAY_TRIANGULATION,
  TESS_REVERSE_CONTOURS
);

TESSreal = libtess2_structs.TESSreal;
PTESSreal = libtess2_structs.PTESSreal;

TESSindex = libtess2_structs.TESSindex;
PTESSindex = libtess2_structs.PTESSindex;

PTESStesselator = libtess2_structs.PTESStesselator;

// Custom memory allocator interface.
// The internal memory allocator allocates mesh edges, vertices and faces
// as well as dictionary nodes and active regions in buckets and uses simple
// freelist to speed up the allocation. The bucket size should roughly match your
// expected input data. For example if you process only hundreds of vertices,
// a bucket size of 128 might be ok, where as when processing thousands of vertices
// bucket size of 1024 might be approproate. The bucket size is a compromise between
// how often to allocate memory from the system versus how much extra space the system
// should allocate. Reasonable defaults are show in commects below, they will be used if
// the bucket sizes are zero.
//
// The use may left the memrealloc to be null. In that case, the tesselator will not try to
// dynamically grow int's internal arrays. The tesselator only needs the reallocation when it
// has found intersecting segments and needs to add new vertex. This defency can be cured by
// allocating some extra vertices beforehand. The 'extraVertices' variable allows to specify
// number of expected extra vertices.
TESSalloc = libtess2_structs.TESSalloc;
PTESSalloc = ^TESSalloc;

//
// Example use:
//
//
//
//

// tessNewTess() - Creates a new tesselator.
// Use tessDeleteTess() to delete the tesselator.
// Parameters:
//   alloc - pointer to a filled TESSalloc struct or NULL to use default malloc based allocator.
// Returns:
//   new tesselator object.
function tessNewTess(alloc: PTESSalloc): PTESStesselator;

// tessDeleteTess() - Deletes a tesselator.
// Parameters:
//   tess - pointer to tesselator object to be deleted.
procedure tessDeleteTess(tess: PTESStesselator);

// tessAddContour() - Adds a contour to be tesselated.
// The type of the vertex coordinates is assumed to be TESSreal.
// Parameters:
//   tess - pointer to tesselator object.
//   size - number of coordinates per vertex. Must be 2 or 3.
//   pointer - pointer to the first coordinate of the first vertex in the array.
//   stride - defines offset in bytes between consecutive vertices.
//   count - number of vertices in contour.
procedure tessAddContour(tess: PTESStesselator; size: PtrInt; vertices: Pointer; stride: PtrInt; count: PtrInt);

// tessSetOption() - Toggles optional tessellation parameters
// Parameters:
//  option - one of TessOption
//  value - 1 if enabled, 0 if disabled.
procedure tessSetOption(tess: PTESStesselator; option: TTessOption; value: PtrInt);

// tessTesselate() - tesselate contours.
// Parameters:
//   tess - pointer to tesselator object.
//   windingRule - winding rules used for tesselation, must be one of TessWindingRule.
//   elementType - defines the tesselation result element type, must be one of TessElementType.
//   polySize - defines maximum vertices per polygons if output is polygons.
//   vertexSize - defines the number of coordinates in tesselation result vertex, must be 2 or 3.
//   normal - defines the normal of the input contours, of null the normal is calculated automatically.
// Returns:
//   1 if succeed, 0 if failed.
function tessTesselate(tess: PTESStesselator; windingRule: TTessWindingRule; elementType: TTessElementType; polySize: PtrInt; vertexSize: PtrInt; normal: PTESSreal): Boolean;

// tessGetVertexCount() - Returns number of vertices in the tesselated output.
function tessGetVertexCount(tess: PTESStesselator): PtrInt;

// tessGetVertices() - Returns pointer to first coordinate of first vertex.
function tessGetVertices(tess: PTESStesselator): PTESSreal;

// tessGetVertexIndices() - Returns pointer to first vertex index.
// Vertex indices can be used to map the generated vertices to the original vertices.
// Every point added using tessAddContour() will get a new index starting at 0.
// New vertices generated at the intersections of segments are assigned value TESS_UNDEF.
function tessGetVertexIndices(tess: PTESStesselator): PTESSindex;

// tessGetElementCount() - Returns number of elements in the the tesselated output.
function tessGetElementCount(tess: PTESStesselator): PtrInt;

// tessGetElements() - Returns pointer to the first element.
function tessGetElements(tess: PTESStesselator): PTESSindex;

implementation

{$IF Defined(FOR_TRITE_TEST_PROGRAM)}
uses
  libtess2_trite;

procedure Normalize(v: PTESSreal);
var
  len: TESSreal;
begin
  len := v[0] * v[0] + v[1] * v[1] + v[2] * v[2];

  Assert(len > 0);
  len := Sqrt(len);
  v[0] := v[0] / len;
  v[1] := v[1] / len;
  v[2] := v[2] / len;
end;
{$ENDIF}

function Dot(u, v: PTESSreal): TESSreal; inline;
begin
  Exit(u[0] * v[0] + u[1] * v[1] + u[2] * v[2]);
end;

function LongAxis(v: PTESSreal): LongInt;
var
  i: LongInt;
begin
  i := 0;
  if Abs(v[1]) > Abs(v[0]) then
    i := 1;
  if Abs(v[2]) > Abs(v[i]) then
    i := 2;
  Exit(i);
end;

function ShortAxis(v: PTESSreal): LongInt;
var
  i: LongInt;
begin
  i := 0;
  if Abs(v[1]) < Abs(v[0]) then
    i := 1;
  if Abs(v[2]) < Abs(v[i]) then
    i := 2;
  Exit(i);
end;

procedure ComputeNormal(tess: PTESStesselator; norm: PTESSreal);
var
  v, v1, v2: PTESSvertex;
  c, tLen2, maxLen2: TESSreal;
  maxVal, minVal, d1, d2, tNorm: array[0 .. 2] of TESSreal;
  maxVert, minVert: array[0 .. 2] of PTESSvertex;
  vHead: PTESSvertex;
  i: LongInt;
begin
  vHead := @tess^.mesh^.vHead;

  v := vHead^.next;
  for I := 0 to 2 do begin
    c := v^.coords[i];
    minVal[i] := c;
    minVert[i] := v;
    maxVal[i] := c;
    maxVert[i] := v;
  end;

  v := vHead^.next;
  while v <> vHead do begin
    for i := 0 to 2 do begin
      c := v^.coords[i];
      if c < minVal[i] then begin
        minVal[i] := c;
        minVert[i] := v;
      end;
      if c > maxVal[i] then begin
        maxVal[i] := c;
        maxVert[i] := v;
      end;
    end;
    v := v^.next
  end;

  // Find two vertices separated by at least 1/sqrt(3) of the maximum
  // distance between any two vertices
  i := 0;
  if maxVal[1] - minVal[1] > maxVal[0] - minVal[0] then
    i := 1;
  if maxVal[2] - minVal[2] > maxVal[i] - minVal[i] then
    i := 2;
  if minVal[i] >= maxVal[i] then begin
    // All vertices are the same -- normal doesn't matter
    norm[0] := 0;
    norm[1] := 0;
    norm[2] := 1;
    Exit;
  end;

  // Look for a third vertex which forms the triangle with maximum area
  // (Length of normal == twice the triangle area)
  maxLen2 := 0;
  v1 := minVert[i];
  v2 := maxVert[i];
  d1[0] := v1^.coords[0] - v2^.coords[0];
  d1[1] := v1^.coords[1] - v2^.coords[1];
  d1[2] := v1^.coords[2] - v2^.coords[2];
  v := vHead^.next;
  while v <> vHead do begin
    d2[0] := v^.coords[0] - v2^.coords[0];
    d2[1] := v^.coords[1] - v2^.coords[1];
    d2[2] := v^.coords[2] - v2^.coords[2];
    tNorm[0] := d1[1]*d2[2] - d1[2]*d2[1];
    tNorm[1] := d1[2]*d2[0] - d1[0]*d2[2];
    tNorm[2] := d1[0]*d2[1] - d1[1]*d2[0];
    tLen2 := tNorm[0]*tNorm[0] + tNorm[1]*tNorm[1] + tNorm[2]*tNorm[2];
    if tLen2 > maxLen2 then begin
      maxLen2 := tLen2;
      norm[0] := tNorm[0];
      norm[1] := tNorm[1];
      norm[2] := tNorm[2];
    end;
    v := v^.next;
  end;

  if maxLen2 <= 0 then begin
    // All points lie on a single line -- any decent normal will do
    norm[0] := 0;
    norm[1] := 0;
    norm[2] := 0;
    norm[ShortAxis(d1)] := 1;
  end;
end;

procedure CheckOrientation(tess: PTESStesselator);
label
  LContinue;
var
  area: TESSreal;
  f, fHead: PTESSface;
  v, vHead: PTESSvertex;
  e: PTESShalfEdge;
begin
  fHead := @tess^.mesh^.fHead;
  vHead := @tess^.mesh^.vHead;

  // When we compute the normal automatically, we choose the orientation
  // so that the the sum of the signed areas of all contours is non-negative.
  area := 0;
  f := fHead^.next;
  while f <> fHead do begin
    e := f^.anEdge;
    if e^.winding <= 0 then
      goto LContinue;
    repeat
      area := area + (e^.Org^.s - e^.Dst^.s) * (e^.Org^.t + e^.Dst^.t);
      e := e^.Lnext;
    until e = f^.anEdge;
  LContinue:
    f := f^.next;
  end;
  if area < 0 then begin
    // Reverse the orientation by flipping all the t-coordinates
    v := vHead^.next;
    while v <> vHead do begin
      v^.t := - v^.t;
      v := v^.next;
    end;
    tess^.tUnit[0] := - tess^.tUnit[0];
    tess^.tUnit[1] := - tess^.tUnit[1];
    tess^.tUnit[2] := - tess^.tUnit[2];
  end;
end;

{$IF Defined(FOR_TRITE_TEST_PROGRAM)}
function S_UNIT_X: TESSreal; inline;
begin
  if RandomSweep then begin
    Exit(2*drand48()-1);
  end else
    Exit(1.0);
end;

function S_UNIT_Y: TESSreal; inline;
begin
  if RandomSweep then begin
    Exit(2*drand48()-1);
  end else
    Exit(0.0);
end;
{$ELSE}
{$IF Defined(SLANTED_SWEEP)}
// The "feature merging" is not intended to be complete.  There are
// special cases where edges are nearly parallel to the sweep line
// which are not implemented.  The algorithm should still behave
// robustly (ie. produce a reasonable tesselation) in the presence
// of such edges, however it may miss features which could have been
// merged.  We could minimize this effect by choosing the sweep line
// direction to be something unusual (ie. not parallel to one of the
// coordinate axes).
//
const S_UNIT_X: TESSreal = 0.50941539564955385; // Pre-normalized
const S_UNIT_Y: TESSreal = 0.86052074622010633;
{$ELSE}
const S_UNIT_X: TESSreal = 1.0;
const S_UNIT_Y: TESSreal = 0.0;
{$ENDIF}
{$ENDIF}

// Determine the polygon normal and project vertices onto the plane
// of the polygon.
//
procedure tessProjectPolygon(tess: PTESStesselator);
var
  v, vHead: PTESSvertex;
  norm: array[0 .. 2] of TESSreal;
  sUnit, tUnit: PTESSreal;
  i: LongInt;
  first: Boolean;
  computedNormal: Boolean;
{$IF Defined(FOR_TRITE_TEST_PROGRAM) or Defined(TRUE_PROJECT)}
  w: TESSreal;
{$ENDIF}
begin
  vHead := @tess^.mesh^.vHead;
  computedNormal := False;

  norm[0] := tess^.normal[0];
  norm[1] := tess^.normal[1];
  norm[2] := tess^.normal[2];
  if (norm[0] = 0) and (norm[1] = 0) and (norm[2] = 0) then begin
    ComputeNormal(tess, norm);
    computedNormal := True;
  end;
  sUnit := tess^.sUnit;
  tUnit := tess^.tUnit;
  i := LongAxis(norm);

{$IF Defined(FOR_TRITE_TEST_PROGRAM) or Defined(TRUE_PROJECT)}
  // Choose the initial sUnit vector to be approximately perpendicular
  // to the normal.
  //
  Normalize(@norm[0]);

  sUnit[i] := 0;
  sUnit[(i+1) mod 3] := S_UNIT_X;
  sUnit[(i+2) mod 3] := S_UNIT_Y;

  // Now make it exactly perpendicular
  w := Dot(sUnit, @norm[0]);
  sUnit[0] := sUnit[0] - w * norm[0];
  sUnit[1] := sUnit[1] - w * norm[1];
  sUnit[2] := sUnit[2] - w * norm[2];
  Normalize(sUnit);

  // Choose tUnit so that (sUnit,tUnit,norm) form a right-handed frame
  tUnit[0] := norm[1]*sUnit[2] - norm[2]*sUnit[1];
  tUnit[1] := norm[2]*sUnit[0] - norm[0]*sUnit[2];
  tUnit[2] := norm[0]*sUnit[1] - norm[1]*sUnit[0];
  Normalize(tUnit);
{$ELSE}
  // Project perpendicular to a coordinate axis -- better numerically
  sUnit[i] := 0;
  sUnit[(i+1) mod 3] := S_UNIT_X;
  sUnit[(i+2) mod 3] := S_UNIT_Y;

  tUnit[i] := 0;
  if norm[i] > 0 then begin
    tUnit[(i+1) mod 3] := -S_UNIT_Y;
    tUnit[(i+2) mod 3] :=  S_UNIT_X;
  end else begin
    tUnit[(i+1) mod 3] :=  S_UNIT_Y;
    tUnit[(i+2) mod 3] := -S_UNIT_X;
  end;
{$ENDIF}

  // Project the vertices onto the sweep plane
  v := vHead^.next;
  while v <> vHead do begin
    v^.s := Dot(v^.coords, sUnit);
    v^.t := Dot(v^.coords, tUnit);
    v := v^.next;
  end;
  if computedNormal then begin
    CheckOrientation(tess);
  end;

  // Compute ST bounds.
  first := True;
  v := vHead^.next;
  while v <> vHead do begin
    if first then begin
      tess^.bmin[0] := v^.s;
      tess^.bmax[0] := v^.s;
      tess^.bmin[1] := v^.t;
      tess^.bmax[1] := v^.t;
      first := False;
    end else begin
      if v^.s < tess^.bmin[0] then tess^.bmin[0] := v^.s;
      if v^.s > tess^.bmax[0] then tess^.bmax[0] := v^.s;
      if v^.t < tess^.bmin[1] then tess^.bmin[1] := v^.t;
      if v^.t > tess^.bmax[1] then tess^.bmax[1] := v^.t;
    end;
    v := v^.next;
  end;
end;

procedure AddWinding(eDst, eSrc: PTESShalfEdge); inline;
begin
  Inc(eDst^.winding, eSrc^.winding);
  Inc(eDst^.Sym^.winding, eSrc^.Sym^.winding);
end;

// tessMeshTessellateMonoRegion( face ) tessellates a monotone region
// (what else would it do??)  The region must consist of a single
// loop of half-edges (see mesh.h) oriented CCW.  "Monotone" in this
// case means that any vertical line intersects the interior of the
// region in a single interval.
//
// Tessellation consists of adding interior edges (actually pairs of
// half-edges), to split the region into non-overlapping triangles.
//
// The basic idea is explained in Preparata and Shamos (which I don''t
// have handy right now), although their implementation is more
// complicated than this one.  The are two edge chains, an upper chain
// and a lower chain.  We process all vertices from both chains in order,
// from right to left.
//
// The algorithm ensures that the following invariant holds after each
// vertex is processed: the untessellated region consists of two
// chains, where one chain (say the upper) is a single edge, and
// the other chain is concave.  The left vertex of the single edge
// is always to the left of all vertices in the concave chain.
//
// Each step consists of adding the rightmost unprocessed vertex to one
// of the two chains, and forming a fan of triangles from the rightmost
// of two chain endpoints.  Determining whether we can add each triangle
// to the fan is a simple orientation test.  By making the fan as large
// as possible, we restore the invariant (check it yourself).
//
function tessMeshTessellateMonoRegion(mesh: PTESSmesh; face: PTESSface): Boolean;
var
  up, lo: PTESShalfEdge;
  tempHalfEdge: PTESShalfEdge;
begin
  //  All edges are oriented CCW around the boundary of the region.
  // First, find the half-edge whose origin vertex is rightmost.
  // Since the sweep goes from left to right, face->anEdge should
  // be close to the edge we want.
  //
  up := face^.anEdge;
  Assert((up^.Lnext <> up) and (up^.Lnext^.Lnext <> up));

  while VertLeq(up^.Dst, up^.Org) do
    up := up^.Lprev;
  while VertLeq(up^.Org, up^.Dst) do
    up := up^.Lnext;
  lo := up^.Lprev;

  while up^.Lnext <> lo do begin
    if VertLeq(up^.Dst, lo^.Org) then begin
      //  up->Dst is on the left.  It is safe to form triangles from lo->Org.
      // The EdgeGoesLeft test guarantees progress even when some triangles
      // are CW, given that the upper and lower chains are truly monotone.
      //
      while (lo^.Lnext <> up) and (EdgeGoesLeft(lo^.Lnext)
        or (EdgeSign(lo^.Org, lo^.Dst, lo^.Lnext^.Dst) <= 0))
      do begin
        tempHalfEdge := tessMeshConnect(mesh, lo^.Lnext, lo);
        if tempHalfEdge = nil then
          Exit(False);
        lo := tempHalfEdge^.Sym;
      end;
      lo := lo^.Lprev;
    end else begin
      // lo->Org is on the left.  We can make CCW triangles from up->Dst.
      while (lo^.Lnext <> up) and (EdgeGoesRight(up^.Lprev)
        or (EdgeSign(up^.Dst, up^.Org, up^.Lprev^.Org) >= 0))
      do begin
        tempHalfEdge := tessMeshConnect(mesh, up, up^.Lprev);
        if tempHalfEdge = nil then
          Exit(False);
        up := tempHalfEdge^.Sym;
      end;
      up := up^.Lnext;
    end;
  end;

  //  Now lo->Org == up->Dst == the leftmost vertex.  The remaining region
  // can be tessellated in a fan from this leftmost vertex.
  //
  Assert(lo^.Lnext <> up);
  while lo^.Lnext^.Lnext <> up do begin
    tempHalfEdge := tessMeshConnect(mesh, lo^.Lnext, lo);
    if tempHalfEdge = nil then
      Exit(False);
    lo := tempHalfEdge^.Sym;
  end;

  Exit(True);
end;

//  tessMeshTessellateInterior( mesh ) tessellates each region of
// the mesh which is marked "inside" the polygon.  Each such region
// must be monotone.
//
function tessMeshTessellateInterior(mesh: PTESSmesh): Boolean;
var
  f, next: PTESSface;
begin
  //LINTED
  f := mesh^.fHead.next;
  while f <> @mesh^.fHead do begin
    // Make sure we don''t try to tessellate the new triangles.
    next := f^.next;
    if f^.inside then begin
      if not tessMeshTessellateMonoRegion(mesh, f) then
        Exit(False);
    end;
    f := next;
  end;
  Exit(True);
end;

type
PEdgeStackNode = ^TEdgeStackNode;
TEdgeStackNode = record
  edge: PTESShalfEdge;
  next: PEdgeStackNode;
end;

type
PEdgeStack = ^TEdgeStack;
TEdgeStack = record
  top: PEdgeStackNode;
  nodeBucket: PBucketAlloc;
end;

function stackInit(stack: PEdgeStack; alloc: PTESSalloc): Boolean;
begin
  stack^.top := nil;
  stack^.nodeBucket := createBucketAlloc(alloc, 'CDT nodes', SizeOf(TEdgeStackNode), 512);
  Exit(stack^.nodeBucket <> nil);
end;

procedure stackDelete(stack: PEdgeStack);
begin
  deleteBucketAlloc(stack^.nodeBucket);
end;

function stackEmpty(stack: PEdgeStack): Boolean;
begin
  Exit(stack^.top = nil);
end;

procedure stackPush(stack: PEdgeStack; e: PTESShalfEdge);
var
  node: PEdgeStackNode;
begin
  node := bucketAlloc(stack^.nodeBucket);
  if node = nil then
    Exit;
  node^.edge := e;
  node^.next := stack^.top;
  stack^.top := node;
end;

function stackPop(stack: PEdgeStack): PTESShalfEdge;
var
  e: PTESShalfEdge;
  node: PEdgeStackNode;
begin
  e := nil;
  node := stack^.top;
  if node <> nil then begin
    stack^.top := node^.next;
    e := node^.edge;
    bucketFree(stack^.nodeBucket, node);
  end;
  Exit(e);
end;


//  Starting with a valid triangulation, uses the Edge Flip algorithm to
//  refine the triangulation into a Constrained Delaunay Triangulation.
procedure tessMeshRefineDelaunay(mesh: PTESSmesh; alloc: PTESSalloc);
var
  f: PTESSface;
  stack: TEdgeStack;
  e: PTESShalfEdge;
  maxFaces, maxIter, iter: LongInt;
  edges: array[0 .. 3] of PTESShalfEdge;
  i: LongInt;
begin
  // At this point, we have a valid, but not optimal, triangulation.
  // We refine the triangulation using the Edge Flip algorithm
  //
  //  1) Find all internal edges
  //  2) Mark all dual edges
  //  3) insert all dual edges into a queue

  maxFaces := 0;
  maxIter := 0;
  iter := 0;

  stackInit(@stack, alloc);

  f := mesh^.fHead.next;
  while f <> @mesh^.fHead do begin
    if f^.inside then begin
      e := f^.anEdge;
      repeat
        e^.mark := EdgeIsInternal(e); // Mark internal edges
        if e^.mark and not e^.Sym^.mark then
          stackPush(@stack, e); // Insert into queue
        e := e^.Lnext;
      until e = f^.anEdge;
      Inc(maxFaces);
    end;
    f := f^.next;
  end;

  // The algorithm should converge on O(n^2), since the predicate is not robust,
  // we'll save guard against infinite loop.
  maxIter := maxFaces * maxFaces;

  // Pop stack until we find a reversed edge
  // Flip the reversed edge, and insert any of the four opposite edges
  // which are internal and not already in the stack (!marked)
  while (not stackEmpty(@stack)) and (iter < maxIter) do begin
    e := stackPop(@stack);
    e^.mark := False;
    e^.Sym^.mark := False;
    if not tesedgeIsLocallyDelaunay(e) then begin
      tessMeshFlipEdge(mesh, e);
      // for each opposite edge
      edges[0] := e^.Lnext;
      edges[1] := e^.Lprev;
      edges[2] := e^.Sym^.Lnext;
      edges[3] := e^.Sym^.Lprev;
      for i := 0 to 3 do begin
        if (not edges[i]^.mark) and EdgeIsInternal(edges[i]) then begin
          edges[i]^.mark := True;
          edges[i]^.Sym^.mark := True;
          stackPush(@stack, edges[i]);
        end;
      end;
    end;
    Inc(iter);
  end;

  stackDelete(@stack);
end;


//  tessMeshDiscardExterior( mesh ) zaps (ie. sets to NULL) all faces
// which are not marked "inside" the polygon.  Since further mesh operations
// on NULL faces are not allowed, the main purpose is to clean up the
// mesh so that exterior loops are not represented in the data structure.
//
procedure tessMeshDiscardExterior(mesh: PTESSmesh);
var
  f, next: PTESSface;
begin
  //LINTED
  f := mesh^.fHead.next;
  while f <> @mesh^.fHead do begin
    // Since f will be destroyed, save its next pointer.
    next := f^.next;
    if not f^.inside then begin
      tessMeshZapFace(mesh, f);
    end;
    f := next;
  end;
end;

//  tessMeshSetWindingNumber( mesh, value, keepOnlyBoundary ) resets the
// winding numbers on all edges so that regions marked "inside" the
// polygon have a winding number of "value", and regions outside
// have a winding number of 0.
//
// If keepOnlyBoundary is TRUE, it also deletes all edges which do not
// separate an interior region from an exterior one.
//
function tessMeshSetWindingNumber(mesh: PTESSmesh; value: LongInt;
               keepOnlyBoundary: Boolean): Boolean;
var
  e, eNext: PTESShalfEdge;
begin
  e := mesh^.eHead.next;
  while e <> @mesh^.eHead do begin
    eNext := e^.next;
    if e^.Rface^.inside <> e^.Lface^.inside then begin

      // This is a boundary edge (one side is interior, one is exterior).
      if e^.Lface^.inside then begin
        e^.winding := value;
      end else
        e^.winding := -value;
    end else begin

      // Both regions are interior, or both are exterior.
      if not keepOnlyBoundary then begin
        e^.winding := 0;
      end else begin
        if not tessMeshDelete(mesh, e) then
          Exit(False);
      end;
    end;
    e := eNext;
  end;
  Exit(True);
end;

function heapAlloc(userData: Pointer; size: SizeUInt): Pointer;
begin
  TESS_NOTUSED(userData);
  Exit(GetMem(size));
end;

function heapRealloc(userData: Pointer; ptr: Pointer; size: SizeUInt): Pointer;
begin
  TESS_NOTUSED(userData);
  Exit(ReAllocMem(ptr, size));
end;

procedure heapFree(userData: Pointer; ptr: Pointer);
begin
  TESS_NOTUSED(userData);
  FreeMem(ptr);
end;

const defaulAlloc: TESSalloc = (
  memalloc: @heapAlloc;
  memrealloc: @heapRealloc;
  memfree: @heapFree;
  userData: nil;
  meshEdgeBucketSize: 0;
  meshVertexBucketSize: 0;
  meshFaceBucketSize: 0;
  dictNodeBucketSize: 0;
  regionBucketSize: 0;
  extraVertices: 0;
);

function tessNewTess(alloc: PTESSalloc): PTESStesselator;
var
  tess: PTESStesselator;
begin
  if alloc = nil then
    alloc := @defaulAlloc;

  //  Only initialize fields which can be changed by the api.  Other fields
  // are initialized where they are used.
  //

  tess := PTESStesselator(alloc^.memalloc(alloc^.userData, SizeOf(TESStesselator)));
  if tess = nil then
    Exit(nil);          // out of memory
  tess^.alloc := alloc^;
  // Check and set defaults.
  if tess^.alloc.meshEdgeBucketSize = 0 then
    tess^.alloc.meshEdgeBucketSize := 512;
  if tess^.alloc.meshVertexBucketSize = 0 then
    tess^.alloc.meshVertexBucketSize := 512;
  if tess^.alloc.meshFaceBucketSize = 0 then
    tess^.alloc.meshFaceBucketSize := 256;
  if tess^.alloc.dictNodeBucketSize = 0 then
    tess^.alloc.dictNodeBucketSize := 512;
  if tess^.alloc.regionBucketSize = 0 then
    tess^.alloc.regionBucketSize := 256;

  tess^.normal[0] := 0;
  tess^.normal[1] := 0;
  tess^.normal[2] := 0;

  tess^.bmin[0] := 0;
  tess^.bmin[1] := 0;
  tess^.bmax[0] := 0;
  tess^.bmax[1] := 0;

  tess^.reverseContours := False;

  tess^.windingRule := TESS_WINDING_ODD;
  tess^.processCDT := False;

  if tess^.alloc.regionBucketSize < 16 then
    tess^.alloc.regionBucketSize := 16;
  if tess^.alloc.regionBucketSize > 4096 then
    tess^.alloc.regionBucketSize := 4096;
  tess^.regionPool := createBucketAlloc(@tess^.alloc, 'Regions',
                      SizeOf(TActiveRegion), tess^.alloc.regionBucketSize);

  // Initialize to begin polygon.
  tess^.mesh := nil;

  tess^.outOfMemory := False;
  tess^.vertexIndexCounter := 0;

  tess^.vertices := nil;
  tess^.vertexIndices := nil;
  tess^.vertexCount := 0;
  tess^.elements := nil;
  tess^.elementCount := 0;

  Exit(tess);
end;

procedure tessDeleteTess(tess: PTESStesselator);
var
  alloc: TESSalloc;
begin
  alloc := tess^.alloc;

  deleteBucketAlloc(tess^.regionPool);

  if tess^.mesh <> nil then begin
    tessMeshDeleteMesh(@alloc, tess^.mesh);
    tess^.mesh := nil;
  end;
  if tess^.vertices <> nil then begin
    alloc.memfree(alloc.userData, tess^.vertices);
    tess^.vertices := nil;
  end;
  if tess^.vertexIndices <> nil then begin
    alloc.memfree(alloc.userData, tess^.vertexIndices);
    tess^.vertexIndices := nil;
  end;
  if tess^.elements <> nil then begin
    alloc.memfree(alloc.userData, tess^.elements);
    tess^.elements := nil;
  end;

  alloc.memfree(alloc.userData, tess);
end;


function GetNeighbourFace(edge: PTESShalfEdge): TESSindex;
begin
  if edge^.Rface = nil then
    Exit(TESS_UNDEF);
  if not edge^.Rface^.inside then
    Exit(TESS_UNDEF);
  Exit(edge^.Rface^.n);
end;

procedure OutputPolymesh(tess: PTESStesselator; mesh: PTESSmesh; elementType: TTessElementType; polySize, vertexSize: PtrInt);
label
  LContinue1, LContinue2;
var
  v: PTESSvertex;
  f: PTESSface;
  edge: PTESShalfEdge;
  maxFaceCount: PtrInt;
  maxVertexCount: PtrInt;
  faceVerts, i: PtrInt;
  elements: PTESSindex;
  vert: PTESSreal;
begin
  v := nil;
  f := nil;
  edge := nil;
  maxFaceCount := 0;
  maxVertexCount := 0;
  elements := nil;

  // Assume that the input data is triangles now.
  // Try to merge as many polygons as possible
  if polySize > 3 then begin
    if not tessMeshMergeConvexFaces(mesh, polySize) then begin
      tess^.outOfMemory := True;
      Exit;
    end;
  end;

  // Mark unused
  v := mesh^.vHead.next;
  while v <> @mesh^.vHead do begin
    v^.n := TESS_UNDEF;
    v := v^.next;
  end;

  // Create unique IDs for all vertices and faces.
  f := mesh^.fHead.next;
  while f <> @mesh^.fHead do begin
    f^.n := TESS_UNDEF;
    if not f^.inside then
      goto LContinue1;

    edge := f^.anEdge;
    faceVerts := 0;
    repeat
      v := edge^.Org;
      if v^.n = TESS_UNDEF then begin
        v^.n := maxVertexCount;
        Inc(maxVertexCount);
      end;
      Inc(faceVerts);
      edge := edge^.Lnext;
    until edge = f^.anEdge;

    Assert(faceVerts <= polySize);

    f^.n := maxFaceCount;
    Inc(maxFaceCount);

  LContinue1:
    f := f^.next;
  end;

  tess^.elementCount := maxFaceCount;
  if elementType = TESS_CONNECTED_POLYGONS then
    maxFaceCount := 2 * maxFaceCount;
  tess^.elements := PTESSindex(tess^.alloc.memalloc(tess^.alloc.userData,
                             SizeOf(TESSindex) * maxFaceCount * polySize));
  if tess^.elements = nil then begin
    tess^.outOfMemory := True;
    Exit;
  end;

  tess^.vertexCount := maxVertexCount;
  tess^.vertices := PTESSreal(tess^.alloc.memalloc(tess^.alloc.userData,
                            SizeOf(TESSreal) * tess^.vertexCount * vertexSize));
  if tess^.vertices = nil then begin
    tess^.outOfMemory := True;
    Exit;
  end;

  tess^.vertexIndices := PTESSindex(tess^.alloc.memalloc(tess^.alloc.userData,
                                 SizeOf(TESSindex) * tess^.vertexCount));
  if tess^.vertexIndices = nil then begin
    tess^.outOfMemory := True;
    Exit;
  end;

  // Output vertices.
  v := mesh^.vHead.next;
  while v <> @mesh^.vHead do begin
    if v^.n <> TESS_UNDEF then begin
      // Store coordinate
      vert := @tess^.vertices[v^.n*vertexSize];
      vert[0] := v^.coords[0];
      vert[1] := v^.coords[1];
      if vertexSize > 2 then
        vert[2] := v^.coords[2];
      // Store vertex index.
      tess^.vertexIndices[v^.n] := v^.idx;
    end;
    v := v^.next;
  end;

  // Output indices.
  elements := tess^.elements;
  f := mesh^.fHead.next;
  while f <> @mesh^.fHead do begin
    if not f^.inside then
      goto LContinue2;

    // Store polygon
    edge := f^.anEdge;
    faceVerts := 0;
    repeat
      v := edge^.Org;
      elements^ := v^.n;
      Inc(elements);
      Inc(faceVerts);
      edge := edge^.Lnext;
    until edge = f^.anEdge;
    // Fill unused.
    for i := faceVerts to polySize - 1 do begin
      elements^ := TESS_UNDEF;
      Inc(elements);
    end;

    // Store polygon connectivity
    if elementType = TESS_CONNECTED_POLYGONS then begin
      edge := f^.anEdge;
      repeat
        elements^ := GetNeighbourFace(edge);
        Inc(elements);
        edge := edge^.Lnext;
      until edge = f^.anEdge;
      // Fill unused.
      for i := faceVerts to polySize - 1 do begin
        elements^ := TESS_UNDEF;
        Inc(elements);
      end;
    end;

  LContinue2:
    f := f^.next;
  end;
end;

procedure OutputContours(tess: PTESStesselator; mesh: PTESSmesh; vertexSize: PtrInt);
label
  LContinue1, LContinue2;
var
  f: PTESSface;
  edge: PTESShalfEdge;
  start: PTESShalfEdge;
  verts: PTESSreal;
  elements: PTESSindex;
  vertInds: PTESSindex;
  startVert: LongInt;
  vertCount: LongInt;
begin
  f := nil;
  edge := nil;
  start := nil;
  verts := nil;
  elements := nil;
  vertInds := nil;
  startVert := 0;
  vertCount := 0;

  tess^.vertexCount := 0;
  tess^.elementCount := 0;

  f := mesh^.fHead.next;
  while f <> @mesh^.fHead do begin
    if not f^.inside then
      goto LContinue1;

    start := f^.anEdge;
    edge := f^.anEdge;
    repeat
      Inc(tess^.vertexCount);
      edge := edge^.Lnext;
    until edge = start;

    Inc(tess^.elementCount);

  LContinue1:
    f := f^.next;
  end;

  tess^.elements := PTESSindex(tess^.alloc.memalloc(tess^.alloc.userData,
                            SizeOf(TESSindex) * tess^.elementCount * 2));
  if tess^.elements = nil then begin
    tess^.outOfMemory := True;
    Exit;
  end;

  tess^.vertices := PTESSreal(tess^.alloc.memalloc(tess^.alloc.userData,
                            SizeOf(TESSreal) * tess^.vertexCount * vertexSize));
  if tess^.vertices = nil then begin
    tess^.outOfMemory := True;
    Exit;
  end;

  tess^.vertexIndices := PTESSindex(tess^.alloc.memalloc(tess^.alloc.userData,
                                SizeOf(TESSindex) * tess^.vertexCount));
  if tess^.vertexIndices = nil then begin
    tess^.outOfMemory := True;
    Exit;
  end;

  verts := tess^.vertices;
  elements := tess^.elements;
  vertInds := tess^.vertexIndices;

  startVert := 0;

  f := mesh^.fHead.next;
  while f <> @mesh^.fHead do begin
    if not f^.inside then
      goto LContinue2;

    vertCount := 0;
    start := f^.anEdge;
    edge := f^.anEdge;
    repeat
      verts^ := edge^.Org^.coords[0];
      Inc(verts);
      verts^ := edge^.Org^.coords[1];
      Inc(verts);
      if vertexSize > 2 then begin
        verts^ := edge^.Org^.coords[2];
        Inc(verts);
      end;
      vertInds^ := edge^.Org^.idx;
      Inc(vertInds);
      Inc(vertCount);
      edge := edge^.Lnext;
    until edge = start;

    elements[0] := startVert;
    elements[1] := vertCount;
    Inc(elements, 2);

    Inc(startVert, vertCount);

  LContinue2:
    f := f^.next;
  end;
end;

procedure tessAddContour(tess: PTESStesselator; size: PtrInt; vertices: Pointer;
                         stride: PtrInt; count: PtrInt);
var
  src: PByte;
  e: PTESShalfEdge;
  i: PtrInt;
  coords: PTESSreal;
begin
  src := PByte(vertices);

  if tess^.mesh = nil then
    tess^.mesh := tessMeshNewMesh(@tess^.alloc);
  if tess^.mesh = nil then begin
    tess^.outOfMemory := True;
    Exit;
  end;

  if size < 2 then
    size := 2;
  if size > 3 then
    size := 3;

  e := nil;

  for i := 0 to count - 1 do begin
    coords := PTESSreal(src);
    Inc(src, stride);

    if e = nil then begin
      // Make a self-loop (one vertex, one edge).
      e := tessMeshMakeEdge(tess^.mesh);
      if e = nil then begin
        tess^.outOfMemory := True;
        Exit;
      end;
      if not tessMeshSplice(tess^.mesh, e, e^.Sym) then begin
        tess^.outOfMemory := True;
        Exit;
      end;
    end else begin
      //  Create a new vertex and edge which immediately follow e
      // in the ordering around the left face.
      //
      if tessMeshSplitEdge(tess^.mesh, e) = nil then begin
        tess^.outOfMemory := True;
        Exit;
      end;
      e := e^.Lnext;
    end;

    // The new vertex is now e^.Org.
    e^.Org^.coords[0] := coords[0];
    e^.Org^.coords[1] := coords[1];
    if size > 2 then begin
      e^.Org^.coords[2] := coords[2];
    end else
      e^.Org^.coords[2] := 0;
    // Store the insertion number so that the vertex can be later recognized.
    e^.Org^.idx := tess^.vertexIndexCounter;
    Inc(tess^.vertexIndexCounter);

    //  The winding of an edge says how the winding number changes as we
    // cross from the edge''s right face to its left face.  We add the
    // vertices in such an order that a CCW contour will add +1 to
    // the winding number of the region inside the contour.
    //
    if tess^.reverseContours then begin
      e^.winding := -1;
      e^.Sym^.winding := 1;
    end else begin
      e^.winding := 1;
      e^.Sym^.winding := -1;
    end;
  end;
end;

procedure tessSetOption(tess: PTESStesselator; option: TTessOption; value: PtrInt);
begin
  case option of
  TESS_CONSTRAINED_DELAUNAY_TRIANGULATION:
    tess^.processCDT := value > 0;
  TESS_REVERSE_CONTOURS:
    tess^.reverseContours := value > 0;
  end;
end;


function tessTesselate(tess: PTESStesselator; windingRule: TTessWindingRule; elementType: TTessElementType;
                       polySize: PtrInt; vertexSize: PtrInt; normal: PTESSreal): Boolean;
var
  mesh: PTESSmesh;
  rc: Boolean;
begin
  rc := True;

  if tess^.vertices <> nil then begin
    tess^.alloc.memfree(tess^.alloc.userData, tess^.vertices);
    tess^.vertices := nil;
  end;
  if tess^.elements <> nil then begin
    tess^.alloc.memfree(tess^.alloc.userData, tess^.elements);
    tess^.elements := nil;
  end;
  if tess^.vertexIndices <> nil then begin
    tess^.alloc.memfree(tess^.alloc.userData, tess^.vertexIndices);
    tess^.vertexIndices := nil;
  end;

  tess^.vertexIndexCounter := 0;

  if normal <> nil then begin
    tess^.normal[0] := normal[0];
    tess^.normal[1] := normal[1];
    tess^.normal[2] := normal[2];
  end;

  tess^.windingRule := windingRule;

  if vertexSize < 2 then
    vertexSize := 2;
  if vertexSize > 3 then
    vertexSize := 3;

  if setjmp(tess^.env) <> 0 then begin
    // come back here if out of memory
    Exit(False);
  end;

  if tess^.mesh = nil then begin
    Exit(False);
  end;

  //  Determine the polygon normal and project vertices onto the plane
  // of the polygon.
  //
  tessProjectPolygon(tess);

  //  tessComputeInterior( tess ) computes the planar arrangement specified
  // by the given contours, and further subdivides this arrangement
  // into regions.  Each region is marked "inside" if it belongs
  // to the polygon, according to the rule given by tess->windingRule.
  // Each interior region is guaranteed be monotone.
  //
  if not tessComputeInterior(tess) then begin
    longjmp(tess^.env, 1);  // could've used a label
  end;

  mesh := tess^.mesh;

  //  If the user wants only the boundary contours, we throw away all edges
  // except those which separate the interior from the exterior.
  // Otherwise we tessellate all the regions marked "inside".
  //
  if elementType = TESS_BOUNDARY_CONTOURS then begin
    rc := tessMeshSetWindingNumber(mesh, 1, True);
  end else begin
    rc := tessMeshTessellateInterior(mesh);
    if rc and tess^.processCDT then
      tessMeshRefineDelaunay(mesh, @tess^.alloc);
  end;
  if not rc then
    longjmp(tess^.env,1);  // could've used a label

  tessMeshCheckMesh(mesh);

  if elementType = TESS_BOUNDARY_CONTOURS then begin
    OutputContours(tess, mesh, vertexSize);     // output contours
  end else begin
    OutputPolymesh(tess, mesh, elementType, polySize, vertexSize);     // output polygons
  end;

  tessMeshDeleteMesh(@tess^.alloc, mesh);
  tess^.mesh := nil;

  if tess^.outOfMemory then
    Exit(False);
  Exit(True);
end;

function tessGetVertexCount(tess: PTESStesselator): PtrInt;
begin
  Exit(tess^.vertexCount);
end;

function tessGetVertices(tess: PTESStesselator): PTESSreal;
begin
  Exit(tess^.vertices);
end;

function tessGetVertexIndices(tess: PTESStesselator): PTESSindex;
begin
  Exit(tess^.vertexIndices);
end;

function tessGetElementCount(tess: PTESStesselator): PtrInt;
begin
  Exit(tess^.elementCount);
end;

function tessGetElements(tess: PTESStesselator): PTESSindex;
begin
  Exit(tess^.elements);
end;

end.
