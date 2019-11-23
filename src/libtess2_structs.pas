unit libtess2_structs;

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
{$MODESWITCH ADVANCEDRECORDS}
{$MODESWITCH OUT}
{$MODESWITCH RESULT}

interface

type
TTessWindingRule = (
  TESS_WINDING_ODD,
  TESS_WINDING_NONZERO,
  TESS_WINDING_POSITIVE,
  TESS_WINDING_NEGATIVE,
  TESS_WINDING_ABS_GEQ_TWO
);

PActiveRegion = ^TActiveRegion;

// tesselator.h
TESSreal = Single;
PTESSreal = ^TESSreal;
TESSindex = PtrInt;
PTESSindex = ^TESSindex;
PTESStesselator = ^TESStesselator;

TESSmemalloc = function (userData: Pointer; Size: SizeUInt): Pointer;
TESSmemrealloc = function (userData: Pointer; Ptr: Pointer; Size: SizeUInt): Pointer;
TESSmemfree = procedure (userData: Pointer; ptr: Pointer);
PTESSalloc = ^TESSalloc;
TESSalloc = record
  memalloc:             TESSmemalloc;
  memrealloc:           TESSmemrealloc;
  memfree:              TESSmemfree;
  userData:             Pointer; // User data passed to the allocator functions.
  meshEdgeBucketSize:   LongInt; // 512
  meshVertexBucketSize: LongInt; // 512
  meshFaceBucketSize:   LongInt; // 256
  dictNodeBucketSize:   LongInt; // 512
  regionBucketSize:     LongInt; // 256
  extraVertices:        LongInt; // Number of extra vertices allocated for the priority queue.
end;

PBucket = ^TBucket;
TBucket = record
  next: PBucket;
end;

PBucketAlloc = ^TBucketAlloc;
TBucketAlloc = record
  freelist: Pointer;
  buckets: PBucket;
  itemSize: PtrUInt;
  bucketSize: PtrUInt;
  name: PAnsiChar;
  alloc: PTESSalloc;
end;

// mesh.h
PTESSvertex = ^TESSvertex;
PTESShalfEdge = ^TESShalfEdge;

TESSvertex = record
  next: PTESSvertex;      // next vertex (never NULL)
  prev: PTESSvertex;      // previous vertex (never NULL)
  anEdge: PTESShalfEdge;  // a half-edge with this origin

  // Internal data (keep hidden)
  coords: array[0 .. 2] of TESSreal; // vertex location in 3D
  s, t: TESSreal;     // projection onto the sweep plane
  pqHandle: PtrInt;   // to allow deletion from priority queue
  n: TESSindex;     // to allow identify unique vertices
  idx: TESSindex; // to allow map result to original verts
end;

PTESSface = ^TESSface;
TESSface = record
  next: PTESSface;        // next face (never NULL)
  prev: PTESSface;        // previous face (never NULL)
  anEdge: PTESShalfEdge;  // a half edge with this left face

  // Internal data (keep hidden)
  trail: PTESSface; // "stack" for conversion to strips
  n: TESSindex;     // to allow identiy unique faces
  marked: Boolean;  // flag for conversion to strips
  inside: Boolean;  // this face is in the polygon interior
end;

TESShalfEdge = record
  next:  PTESShalfEdge; // doubly-linked list (prev==Sym->next)
  Sym:   PTESShalfEdge; // same edge, opposite direction
  Onext: PTESShalfEdge; // next edge CCW around origin
  Lnext: PTESShalfEdge; // next edge CCW around left face
  Org:   PTESSvertex;   // origin vertex (Overtex too long)
  Lface: PTESSface;     // left face

  // Internal data (keep hidden)
  activeRegion: PActiveRegion; // a region with this upper edge (sweep.c)
  winding: PtrInt;  // change in winding number when crossing
                    // from the right face to the left face
  mark: Boolean;    // Used by the Edge Flip algorithm

  function GetRface: PTESSface; inline;
  function GetDst: PTESSvertex; inline;
  function GetOprev: PTESShalfEdge; inline;
  function GetLprev: PTESShalfEdge; inline;
  function GetDprev: PTESShalfEdge; inline;
  function GetRprev: PTESShalfEdge; inline;
  function GetDnext: PTESShalfEdge; inline;
  function GetRnext: PTESShalfEdge; inline;
  procedure SetRface(NewValue: PTESSface); inline;
  procedure SetDst(NewValue: PTESSvertex); inline;
  procedure SetOprev(NewValue: PTESShalfEdge); inline;
  procedure SetLprev(NewValue: PTESShalfEdge); inline;
  procedure SetDprev(NewValue: PTESShalfEdge); inline;
  procedure SetRprev(NewValue: PTESShalfEdge); inline;
  procedure SetDnext(NewValue: PTESShalfEdge); inline;
  procedure SetRnext(NewValue: PTESShalfEdge); inline;
  property Rface: PTESSface read GetRface write SetRface;
  property Dst: PTESSvertex read GetDst write SetDst;
  property Oprev: PTESShalfEdge read GetOprev write SetOprev;
  property Lprev: PTESShalfEdge read GetLprev write SetLprev;
  property Dprev: PTESShalfEdge read GetDprev write SetDprev;
  property Rprev: PTESShalfEdge read GetRprev write SetRprev;
  property Dnext: PTESShalfEdge read GetDnext write SetDnext; // 3 pointers;
  property Rnext: PTESShalfEdge read GetRnext write SetRnext; // 3 pointers;

  {property Rface: PTESSface read Sym^.Lface write Sym^.Lface;
  property Dst: PTESSvertex read Sym^.Org write Sym^.Org;
  property Oprev: PTESShalfEdge read Sym^.Lnext write Sym^.Lnext;
  property Lprev: PTESShalfEdge read Onext^.Sym write Onext^.Sym;
  property Dprev: PTESShalfEdge read Lnext^.Sym write Lnext^.Sym;
  property Rprev: PTESShalfEdge read Sym^.Onext write Sym^.Onext;
  property Dnext: PTESShalfEdge read Rprev^.Sym write Rprev^.Sym; // 3 pointers;
  property Rnext: PTESShalfEdge read Oprev^.Sym write Oprev^.Sym; // 3 pointers;}
end;

PTESSmesh = ^TESSmesh;
TESSmesh = record
  vHead:    TESSvertex;   // dummy header for vertex list
  fHead:    TESSface;     // dummy header for face list
  eHead:    TESShalfEdge; // dummy header for edge list
  eHeadSym: TESShalfEdge; // and its symmetric counterpart

  edgeBucket:   PBucketAlloc;
  vertexBucket: PBucketAlloc;
  faceBucket:   PBucketAlloc;
end;

// priorityq.h
PQkey = Pointer;
PPQkey = ^PQkey;
PPPQkey = ^PPQkey;

PQhandle = PtrInt;

PPQnode = ^PQnode;
PQnode = record
  handle: PQhandle;
end;

PPQhandleElem = ^PQhandleElem;
PQhandleElem = record
  key: PQkey;
  node: PQhandle;
end;

TPriorityQLeq = function (key1, key2: PQkey): Boolean;

PPriorityQHeap = ^TPriorityQHeap;
TPriorityQHeap = record
  nodes: PPQnode;
  handles: PPQhandleElem;
  size, max: PtrInt;
  freeList: PQhandle;
  initialized: Boolean;

  leq: TPriorityQLeq;
end;

PPriorityQ = ^TPriorityQ;
TPriorityQ = record
  heap: PPriorityQHeap;

  keys: PPQkey;
  order: PPPQkey;
  size, max: PQhandle;
  initialized: Boolean;

  leq: TPriorityQLeq;
end;

// dict.h
TDictKey = Pointer;

// Private data structures
PDictNode = ^TDictNode;
TDictNode = record
  key: TDictKey;
  next: PDictNode;
  prev: PDictNode;
end;

TDictLeq = function (frame: Pointer; key1, key2: TDictKey): Boolean;

PDict = ^TDict;
TDict = record
  head: TDictNode;
  frame: Pointer;
  nodePool: PBucketAlloc;
  Leq: TDictLeq;
end;

// sweep.h
//  For each pair of adjacent edges crossing the sweep line, there is
// an ActiveRegion to represent the region between them.  The active
// regions are kept in sorted order in a dynamic dictionary.  As the
// sweep line crosses each vertex, we update the affected regions.
//

TActiveRegion = record
  eUp: PTESShalfEdge;     // upper edge, directed right to left
  nodeUp: PDictNode;      // dictionary node corresponding to eUp
  windingNumber: PtrInt;  // used to determine which regions are
                          // inside the polygon
  inside: Boolean;   // is this region inside the polygon?
  sentinel: Boolean; // marks fake edges at t = +/-infinity
  dirty: Boolean;    // marks regions where the upper or lower
                     // edge has changed, but we haven't checked
                     // whether they intersect yet
  fixUpperEdge: Boolean; // marks temporary edges introduced when
                         // we process a "right vertex" (one without
                         // any edges leaving to the right)
end;

// tesselator.h
TESStesselator = record
  // state needed for collecting the input data
  mesh: PTESSmesh; // stores the input contours, and eventually the tessellation itself
  outOfMemory: Boolean;

  // state needed for projecting onto the sweep plane

  normal: array[0 .. 2] of TESSreal; // user-specified normal (if provided)
  sUnit:  array[0 .. 2] of TESSreal; // unit vector in s-direction (debugging)
  tUnit:  array[0 .. 2] of TESSreal; // unit vector in t-direction (debugging)

  bmin: array[0 .. 1] of TESSreal;
  bmax: array[0 .. 1] of TESSreal;

  processCDT: Boolean;      // option to run Constrained Delayney pass.
  reverseContours: Boolean; // tessAddContour() will treat CCW contours as CW and vice versa

  // state needed for the line sweep
  windingRule: TTessWindingRule; // rule for determining polygon interior

  dict: PDict;        // edge dictionary for sweep line
  pq: PPriorityQ;     // priority queue of vertex events
  event: PTESSvertex; // current sweep event being processed

  regionPool: PBucketAlloc;

  vertexIndexCounter: TESSindex;

  vertices: PTESSreal;
  vertexIndices: PTESSindex;
  vertexCount: LongInt;
  elements: PTESSindex;
  elementCount: LongInt;

  alloc: TESSalloc;

  env: jmp_buf; // place to jump to when memAllocs fail
end;

procedure TESS_NOTUSED(var v); inline;

const INV_HANDLE = $0fffffff;
const TESS_UNDEF = not TESSindex(0);

implementation

procedure TESS_NOTUSED(var v);
begin
end;

function TESShalfEdge.GetRface: PTESSface;
begin
  Exit(Sym^.Lface);
end;

function TESShalfEdge.GetDst: PTESSvertex;
begin
  Exit(Sym^.Org);
end;

function TESShalfEdge.GetOprev: PTESShalfEdge;
begin
  Exit(Sym^.Lnext);
end;

function TESShalfEdge.GetLprev: PTESShalfEdge;
begin
  Exit(Onext^.Sym);
end;

function TESShalfEdge.GetDprev: PTESShalfEdge;
begin
  Exit(Lnext^.Sym);
end;

function TESShalfEdge.GetRprev: PTESShalfEdge;
begin
  Exit(Sym^.Onext);
end;

function TESShalfEdge.GetDnext: PTESShalfEdge;
begin
  Exit(Rprev^.Sym);
end;

function TESShalfEdge.GetRnext: PTESShalfEdge;
begin
  Exit(Oprev^.Sym);
end;

procedure TESShalfEdge.SetRface(NewValue: PTESSface);
begin
  Sym^.Lface := NewValue;
end;

procedure TESShalfEdge.SetDst(NewValue: PTESSvertex);
begin
  Sym^.Org := NewValue;
end;

procedure TESShalfEdge.SetOprev(NewValue: PTESShalfEdge);
begin
  Sym^.Lnext := NewValue;
end;

procedure TESShalfEdge.SetLprev(NewValue: PTESShalfEdge);
begin
  Onext^.Sym := NewValue;
end;

procedure TESShalfEdge.SetDprev(NewValue: PTESShalfEdge);
begin
  Lnext^.Sym := NewValue;
end;

procedure TESShalfEdge.SetRprev(NewValue: PTESShalfEdge);
begin
  Sym^.Onext := NewValue;
end;

procedure TESShalfEdge.SetDnext(NewValue: PTESShalfEdge);
begin
  Rprev^.Sym := NewValue;
end;

procedure TESShalfEdge.SetRnext(NewValue: PTESShalfEdge);
begin
  Oprev^.Sym := NewValue;
end;

end.
