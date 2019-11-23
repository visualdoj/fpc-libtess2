unit libtess2_mesh;

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
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  libtess2_structs,
  libtess2_bucketalloc,
  libtess2_geom;

//  The mesh structure is similar in spirit, notation, and operations
// to the "quad-edge" structure (see L. Guibas and J. Stolfi, Primitives
// for the manipulation of general subdivisions and the computation of
// Voronoi diagrams, ACM Transactions on Graphics, 4(2):74-123, April 1985).
// For a simplified description, see the course notes for CS348a,
// "Mathematical Foundations of Computer Graphics", available at the
// Stanford bookstore (and taught during the fall quarter).
// The implementation also borrows a tiny subset of the graph-based approach
// use in Mantyla's Geometric Work Bench (see M. Mantyla, An Introduction
// to Sold Modeling, Computer Science Press, Rockville, Maryland, 1988).
//
// The fundamental data structure is the "half-edge".  Two half-edges
// go together to make an edge, but they point in opposite directions.
// Each half-edge has a pointer to its mate (the "symmetric" half-edge Sym),
// its origin vertex (Org), the face on its left side (Lface), and the
// adjacent half-edges in the CCW direction around the origin vertex
// (Onext) and around the left face (Lnext).  There is also a "next"
// pointer for the global edge list (see below).
//
// The notation used for mesh navigation:
//  Sym   = the mate of a half-edge (same edge, but opposite direction)
//  Onext = edge CCW around origin vertex (keep same origin)
//  Dnext = edge CCW around destination vertex (keep same dest)
//  Lnext = edge CCW around left face (dest becomes new origin)
//  Rnext = edge CCW around right face (origin becomes new dest)
//
// "prev" means to substitute CW for CCW in the definitions above.
//
// The mesh keeps global lists of all vertices, faces, and edges,
// stored as doubly-linked circular lists with a dummy header node.
// The mesh stores pointers to these dummy headers (vHead, fHead, eHead).
//
// The circular edge list is special; since half-edges always occur
// in pairs (e and e->Sym), each half-edge stores a pointer in only
// one direction.  Starting at eHead and following the e->next pointers
// will visit each *edge* once (ie. e or e->Sym, but not both).
// e->Sym stores a pointer in the opposite direction, thus it is
// always true that e->Sym->next->Sym->next == e.
//
// Each vertex has a pointer to next and previous vertices in the
// circular list, and a pointer to a half-edge with this vertex as
// the origin (NULL if this is the dummy header).  There is also a
// field "data" for client data.
//
// Each face has a pointer to the next and previous faces in the
// circular list, and a pointer to a half-edge with this face as
// the left face (NULL if this is the dummy header).  There is also
// a field "data" for client data.
//
// Note that what we call a "face" is really a loop; faces may consist
// of more than one loop (ie. not simply connected), but there is no
// record of this in the data structure.  The mesh may consist of
// several disconnected regions, so it may not be possible to visit
// the entire mesh by starting at a half-edge and traversing the edge
// structure.
//
// The mesh does NOT support isolated vertices; a vertex is deleted along
// with its last edge.  Similarly when two faces are merged, one of the
// faces is deleted (see tessMeshDelete below).  For mesh operations,
// all face (loop) and vertex pointers must not be NULL.  However, once
// mesh manipulation is finished, TESSmeshZapFace can be used to delete
// faces of the mesh, one at a time.  All external faces can be "zapped"
// before the mesh is returned to the client; then a NULL face indicates
// a region which is not part of the output polygon.
//

//  The mesh operations below have three motivations: completeness,
// convenience, and efficiency.  The basic mesh operations are MakeEdge,
// Splice, and Delete.  All the other edge operations can be implemented
// in terms of these.  The other operations are provided for convenience
// and/or efficiency.
//
// When a face is split or a vertex is added, they are inserted into the
// global list *before* the existing vertex or face (ie. e->Org or e->Lface).
// This makes it easier to process all vertices or faces in the global lists
// without worrying about processing the same data twice.  As a convenience,
// when a face is split, the "inside" flag is copied from the old face.
// Other internal data (v->data, v->activeRegion, f->data, f->marked,
// f->trail, e->winding) is set to zero.
//
// ********************** Basic Edge Operations **************************
//
// tessMeshMakeEdge( mesh ) creates one edge, two vertices, and a loop.
// The loop (face) consists of the two new half-edges.
//
// tessMeshSplice( eOrg, eDst ) is the basic operation for changing the
// mesh connectivity and topology.  It changes the mesh so that
//  eOrg->Onext <- OLD( eDst->Onext )
//  eDst->Onext <- OLD( eOrg->Onext )
// where OLD(...) means the value before the meshSplice operation.
//
// This can have two effects on the vertex structure:
//  - if eOrg->Org != eDst->Org, the two vertices are merged together
//  - if eOrg->Org == eDst->Org, the origin is split into two vertices
// In both cases, eDst->Org is changed and eOrg->Org is untouched.
//
// Similarly (and independently) for the face structure,
//  - if eOrg->Lface == eDst->Lface, one loop is split into two
//  - if eOrg->Lface != eDst->Lface, two distinct loops are joined into one
// In both cases, eDst->Lface is changed and eOrg->Lface is unaffected.
//
// tessMeshDelete( eDel ) removes the edge eDel.  There are several cases:
// if (eDel->Lface != eDel->Rface), we join two loops into one; the loop
// eDel->Lface is deleted.  Otherwise, we are splitting one loop into two;
// the newly created loop will contain eDel->Dst.  If the deletion of eDel
// would create isolated vertices, those are deleted as well.
//
// ********************** Other Edge Operations **************************
//
// tessMeshAddEdgeVertex( eOrg ) creates a new edge eNew such that
// eNew == eOrg->Lnext, and eNew->Dst is a newly created vertex.
// eOrg and eNew will have the same left face.
//
// tessMeshSplitEdge( eOrg ) splits eOrg into two edges eOrg and eNew,
// such that eNew == eOrg->Lnext.  The new vertex is eOrg->Dst == eNew->Org.
// eOrg and eNew will have the same left face.
//
// tessMeshConnect( eOrg, eDst ) creates a new edge from eOrg->Dst
// to eDst->Org, and returns the corresponding half-edge eNew.
// If eOrg->Lface == eDst->Lface, this splits one loop into two,
// and the newly created loop is eNew->Lface.  Otherwise, two disjoint
// loops are merged into one, and the loop eDst->Lface is destroyed.
//
// ************************ Other Operations *****************************
//
// tessMeshNewMesh() creates a new mesh with no edges, no vertices,
// and no loops (what we usually call a "face").
//
// tessMeshUnion( mesh1, mesh2 ) forms the union of all structures in
// both meshes, and returns the new mesh (the old meshes are destroyed).
//
// tessMeshDeleteMesh( mesh ) will free all storage for any valid mesh.
//
// tessMeshZapFace( fZap ) destroys a face and removes it from the
// global face list.  All edges of fZap will have a NULL pointer as their
// left face.  Any edges which also have a NULL pointer as their right face
// are deleted entirely (along with any isolated vertices this produces).
// An entire mesh can be deleted by zapping its faces, one at a time,
// in any order.  Zapped faces cannot be used in further mesh operations!
//
// tessMeshCheckMesh( mesh ) checks a mesh for self-consistency.
//

function tessMeshMakeEdge(mesh: PTESSmesh): PTESShalfEdge;
function tessMeshSplice(mesh: PTESSmesh; eOrg: PTESShalfEdge; eDst: PTESShalfEdge): Boolean;
function tessMeshDelete(mesh: PTESSmesh; eDel: PTESShalfEdge): Boolean;

function tessMeshAddEdgeVertex(mesh: PTESSmesh; eOrg: PTESShalfEdge): PTESShalfEdge;
function tessMeshSplitEdge(mesh: PTESSmesh; eOrg: PTESShalfEdge): PTESShalfEdge;
function tessMeshConnect(mesh: PTESSmesh; eOrg: PTESShalfEdge; eDst: PTESShalfEdge): PTESShalfEdge;

function tessMeshNewMesh(alloc: PTESSalloc): PTESSmesh;
function tessMeshUnion(alloc: PTESSalloc; mesh1, mesh2: PTESSmesh): PTESSmesh;
function tessMeshMergeConvexFaces(mesh: PTESSmesh; maxVertsPerFace: PtrInt): Boolean;
procedure tessMeshDeleteMesh(alloc: PTESSalloc; mesh: PTESSmesh);
procedure tessMeshZapFace(mesh: PTESSmesh; fZap: PTESSface);

procedure tessMeshFlipEdge(mesh: PTESSmesh; edge: PTESShalfEdge);

procedure tessMeshCheckMesh(mesh: PTESSmesh); inline;

implementation

//*********************** Utility Routines ************************

//  Allocate and free half-edges in pairs for efficiency.
// The *only* place that should use this fact is allocation/free.
//
type
PEdgePair = ^TEdgePair;
TEdgePair = record
  e, eSym: TESShalfEdge;
end;

//  MakeEdge creates a new pair of half-edges which form their own loop.
// No vertex or face structures are allocated, but these must be assigned
// before the current edge operation is completed.
//
function MakeEdge(mesh: PTESSmesh; eNext: PTESShalfEdge): PTESShalfEdge;
var
  e: PTESShalfEdge;
  eSym: PTESShalfEdge;
  ePrev: PTESShalfEdge;
  pair: PEdgePair;
begin
  pair := PEdgePair(bucketAlloc(mesh^.edgeBucket));
  if pair = nil then
    Exit(nil);

  e := @pair^.e;
  eSym := @pair^.eSym;

  // Make sure eNext points to the first edge of the edge pair
  if eNext^.Sym < eNext then begin
    eNext := eNext^.Sym;
  end;

  //  Insert in circular doubly-linked list before eNext.
  // Note that the prev pointer is stored in Sym^.next.
  //
  ePrev := eNext^.Sym^.next;
  eSym^.next := ePrev;
  ePrev^.Sym^.next := e;
  e^.next := eNext;
  eNext^.Sym^.next := eSym;

  e^.Sym := eSym;
  e^.Onext := e;
  e^.Lnext := eSym;
  e^.Org := nil;
  e^.Lface := nil;
  e^.winding := 0;
  e^.activeRegion := nil;
  e^.mark := False;

  eSym^.Sym := e;
  eSym^.Onext := eSym;
  eSym^.Lnext := e;
  eSym^.Org := nil;
  eSym^.Lface := nil;
  eSym^.winding := 0;
  eSym^.activeRegion := nil;
  eSym^.mark := False;

  Exit(e);
end;

//  Splice( a, b ) is best described by the Guibas/Stolfi paper or the
// CS348a notes (see mesh.h).  Basically it modifies the mesh so that
// a->Onext and b->Onext are exchanged.  This can have various effects
// depending on whether a and b belong to different face or vertex rings.
// For more explanation see tessMeshSplice() below.
//
procedure Splice(a, b: PTESShalfEdge);
var
  aOnext: PTESShalfEdge;
  bOnext: PTESShalfEdge;
begin
  aOnext := a^.Onext;
  bOnext := b^.Onext;

  aOnext^.Sym^.Lnext := b;
  bOnext^.Sym^.Lnext := a;
  a^.Onext := bOnext;
  b^.Onext := aOnext;
end;

//  MakeVertex( newVertex, eOrig, vNext ) attaches a new vertex and makes it the
// origin of all edges in the vertex loop to which eOrig belongs. "vNext" gives
// a place to insert the new vertex in the global vertex list.  We insert
// the new vertex *before* vNext so that algorithms which walk the vertex
// list will not see the newly created vertices.
//
procedure MakeVertex(newVertex: PTESSvertex;
                     eOrig: PTESShalfEdge; vNext: PTESSvertex);
var
  e: PTESShalfEdge;
  vPrev: PTESSvertex;
  vNew: PTESSvertex;
begin
  vNew := newVertex;

  Assert(vNew <> nil);

  // insert in circular doubly-linked list before vNext
  vPrev := vNext^.prev;
  vNew^.prev := vPrev;
  vPrev^.next := vNew;
  vNew^.next := vNext;
  vNext^.prev := vNew;

  vNew^.anEdge := eOrig;
  // leave coords, s, t undefined

  // fix other edges on this vertex loop
  e := eOrig;
  repeat
    e^.Org := vNew;
    e := e^.Onext;
  until e = eOrig;
end;

//  MakeFace( newFace, eOrig, fNext ) attaches a new face and makes it the left
// face of all edges in the face loop to which eOrig belongs.  "fNext" gives
// a place to insert the new face in the global face list.  We insert
// the new face *before* fNext so that algorithms which walk the face
// list will not see the newly created faces.
//
procedure MakeFace(newFace: PTESSface; eOrig: PTESShalfEdge; fNext: PTESSface);
var
  e: PTESShalfEdge;
  fPrev: PTESSface;
  fNew: PTESSface;
begin
  fNew := newFace;

  Assert(fNew <> nil);

  // insert in circular doubly-linked list before fNext
  fPrev := fNext^.prev;
  fNew^.prev := fPrev;
  fPrev^.next := fNew;
  fNew^.next := fNext;
  fNext^.prev := fNew;

  fNew^.anEdge := eOrig;
  fNew^.trail := nil;
  fNew^.marked := False;

  // The new face is marked "inside" if the old one was.  This is a
  // convenience for the common case where a face has been split in two.
  //
  fNew^.inside := fNext^.inside;

  // fix other edges on this face loop
  e := eOrig;
  repeat
    e^.Lface := fNew;
    e := e^.Lnext;
  until e = eOrig;
end;

//  KillEdge( eDel ) destroys an edge (the half-edges eDel and eDel->Sym),
// and removes from the global edge list.
//
procedure KillEdge(mesh: PTESSmesh; eDel: PTESShalfEdge);
var
  ePrev, eNext: PTESShalfEdge;
begin
  // Half-edges are allocated in pairs, see EdgePair above
  if eDel^.Sym < eDel then begin
    eDel := eDel^.Sym;
  end;

  // delete from circular doubly-linked list
  eNext := eDel^.next;
  ePrev := eDel^.Sym^.next;
  eNext^.Sym^.next := ePrev;
  ePrev^.Sym^.next := eNext;

  bucketFree(mesh^.edgeBucket, eDel);
end;


//  KillVertex( vDel ) destroys a vertex and removes it from the global
// vertex list.  It updates the vertex loop to point to a given new vertex.
//
procedure KillVertex(mesh: PTESSmesh; vDel: PTESSvertex; newOrg: PTESSvertex);
var
  e, eStart: PTESShalfEdge;
  vPrev, vNext: PTESSvertex;
begin
  eStart := vDel^.anEdge;

  // change the origin of all affected edges
  e := eStart;
  repeat
    e^.Org := newOrg;
    e := e^.Onext;
  until e = eStart;

  // delete from circular doubly-linked list
  vPrev := vDel^.prev;
  vNext := vDel^.next;
  vNext^.prev := vPrev;
  vPrev^.next := vNext;

  bucketFree(mesh^.vertexBucket, vDel);
end;

//  KillFace( fDel ) destroys a face and removes it from the global face
// list.  It updates the face loop to point to a given new face.
//
procedure KillFace(mesh: PTESSmesh; fDel: PTESSface; newLface: PTESSface);
var
  e, eStart: PTESShalfEdge;
  fPrev, fNext: PTESSface;
begin
  eStart := fDel^.anEdge;

  // change the left face of all affected edges
  e := eStart;
  repeat
    e^.Lface := newLface;
    e := e^.Lnext;
  until e = eStart;

  // delete from circular doubly-linked list
  fPrev := fDel^.prev;
  fNext := fDel^.next;
  fNext^.prev := fPrev;
  fPrev^.next := fNext;

  bucketFree(mesh^.faceBucket, fDel);
end;


//***************** Basic Edge Operations **********************

//  tessMeshMakeEdge creates one edge, two vertices, and a loop (face).
// The loop consists of the two new half-edges.
//
function tessMeshMakeEdge(mesh: PTESSmesh): PTESShalfEdge;
var
  newVertex1: PTESSvertex;
  newVertex2: PTESSvertex;
  newFace: PTESSface;
  e: PTESShalfEdge;
begin
  newVertex1 := PTESSvertex(bucketAlloc(mesh^.vertexBucket));
  newVertex2 := PTESSvertex(bucketAlloc(mesh^.vertexBucket));
  newFace := PTESSface(bucketAlloc(mesh^.faceBucket));

  // if any one is null then all get freed
  if (newVertex1 = nil) or (newVertex2 = nil) or (newFace = nil) then begin
    if newVertex1 <> nil then
      bucketFree(mesh^.vertexBucket, newVertex1);
    if newVertex2 <> nil then
      bucketFree(mesh^.vertexBucket, newVertex2);
    if newFace <> nil then
      bucketFree(mesh^.faceBucket, newFace);
    Exit(nil);
  end;

  e := MakeEdge(mesh, @mesh^.eHead);
  if e = nil then
    Exit(nil);

  MakeVertex(newVertex1, e, @mesh^.vHead);
  MakeVertex(newVertex2, e^.Sym, @mesh^.vHead);
  MakeFace(newFace, e, @mesh^.fHead);
  Exit(e);
end;


//  tessMeshSplice( eOrg, eDst ) is the basic operation for changing the
// mesh connectivity and topology.  It changes the mesh so that
//  eOrg->Onext <- OLD( eDst->Onext )
//  eDst->Onext <- OLD( eOrg->Onext )
// where OLD(...) means the value before the meshSplice operation.
//
// This can have two effects on the vertex structure:
//  - if eOrg->Org != eDst->Org, the two vertices are merged together
//  - if eOrg->Org == eDst->Org, the origin is split into two vertices
// In both cases, eDst->Org is changed and eOrg->Org is untouched.
//
// Similarly (and independently) for the face structure,
//  - if eOrg->Lface == eDst->Lface, one loop is split into two
//  - if eOrg->Lface != eDst->Lface, two distinct loops are joined into one
// In both cases, eDst->Lface is changed and eOrg->Lface is unaffected.
//
// Some special cases:
// If eDst == eOrg, the operation has no effect.
// If eDst == eOrg->Lnext, the new face will have a single edge.
// If eDst == eOrg->Lprev, the old face will have a single edge.
// If eDst == eOrg->Onext, the new vertex will have a single edge.
// If eDst == eOrg->Oprev, the old vertex will have a single edge.
//
function tessMeshSplice(mesh: PTESSmesh; eOrg: PTESShalfEdge; eDst: PTESShalfEdge): Boolean;
var
  joiningLoops: Boolean;
  joiningVertices: Boolean;
  newVertex: PTESSvertex;
  newFace: PTESSface;
begin
  joiningLoops := False;
  joiningVertices := False;

  if eOrg = eDst then
    Exit(True);

  if eDst^.Org <> eOrg^.Org then begin
    // We are merging two disjoint vertices -- destroy eDst^.Org
    joiningVertices := True;
    KillVertex(mesh, eDst^.Org, eOrg^.Org);
  end;
  if eDst^.Lface <> eOrg^.Lface then begin
    // We are connecting two disjoint loops -- destroy eDst->Lface
    joiningLoops := True;
    KillFace(mesh, eDst^.Lface, eOrg^.Lface);
  end;

  // Change the edge structure
  Splice(eDst, eOrg);

  if not joiningVertices then begin
    newVertex := PTESSvertex(bucketAlloc(mesh^.vertexBucket));
    if newVertex = nil then
      Exit(False);

    //  We split one vertex into two -- the new vertex is eDst->Org.
    // Make sure the old vertex points to a valid half-edge.
    //
    MakeVertex(newVertex, eDst, eOrg^.Org);
    eOrg^.Org^.anEdge := eOrg;
  end;
  if not joiningLoops then begin
    newFace := PTESSface(bucketAlloc(mesh^.faceBucket));
    if newFace = nil then
      Exit(False);

    //  We split one loop into two -- the new loop is eDst->Lface.
    // Make sure the old face points to a valid half-edge.
    //
    MakeFace(newFace, eDst, eOrg^.Lface);
    eOrg^.Lface^.anEdge := eOrg;
  end;

  Exit(True);
end;


//  tessMeshDelete( eDel ) removes the edge eDel.  There are several cases:
// if (eDel->Lface != eDel->Rface), we join two loops into one; the loop
// eDel->Lface is deleted.  Otherwise, we are splitting one loop into two;
// the newly created loop will contain eDel->Dst.  If the deletion of eDel
// would create isolated vertices, those are deleted as well.
//
// This function could be implemented as two calls to tessMeshSplice
// plus a few calls to memFree, but this would allocate and delete
// unnecessary vertices and faces.
//
function tessMeshDelete(mesh: PTESSmesh; eDel: PTESShalfEdge): Boolean;
var
  eDelSym: PTESShalfEdge;
  joiningLoops: Boolean;
  newFace: PTESSface;
begin
  eDelSym := eDel^.Sym;
  joiningLoops := False;

  //  First step: disconnect the origin vertex eDel->Org.  We make all
  // changes to get a consistent mesh in this "intermediate" state.
  //
  if eDel^.Lface <> eDel^.Rface then begin
    // We are joining two loops into one -- remove the left face
    joiningLoops := True;
    KillFace(mesh, eDel^.Lface, eDel^.Rface);
  end;

  if eDel^.Onext = eDel then begin
    KillVertex(mesh, eDel^.Org, nil);
  end else begin
    // Make sure that eDel->Org and eDel->Rface point to valid half-edges
    eDel^.Rface^.anEdge := eDel^.Oprev;
    eDel^.Org^.anEdge := eDel^.Onext;

    Splice(eDel, eDel^.Oprev);
    if not joiningLoops then begin
      newFace := PTESSface(bucketAlloc(mesh^.faceBucket));
      if newFace = nil then
        Exit(False);

      // We are splitting one loop into two -- create a new loop for eDel.
      MakeFace(newFace, eDel, eDel^.Lface);
    end;
  end;

  //  Claim: the mesh is now in a consistent state, except that eDel->Org
  // may have been deleted.  Now we disconnect eDel->Dst.
  //
  if eDelSym^.Onext = eDelSym then begin
    KillVertex(mesh, eDelSym^.Org, nil);
    KillFace(mesh, eDelSym^.Lface, nil);
  end else begin
    // Make sure that eDel->Dst and eDel->Lface point to valid half-edges
    eDel^.Lface^.anEdge := eDelSym^.Oprev;
    eDelSym^.Org^.anEdge := eDelSym^.Onext;
    Splice(eDelSym, eDelSym^.Oprev);
  end;

  // Any isolated vertices or faces have already been freed.
  KillEdge(mesh, eDel);

  Exit(True);
end;


//******************* Other Edge Operations **********************

//  All these routines can be implemented with the basic edge
// operations above.  They are provided for convenience and efficiency.
//


//  tessMeshAddEdgeVertex( eOrg ) creates a new edge eNew such that
// eNew == eOrg->Lnext, and eNew->Dst is a newly created vertex.
// eOrg and eNew will have the same left face.
//
function tessMeshAddEdgeVertex(mesh: PTESSmesh; eOrg: PTESShalfEdge): PTESShalfEdge;
var
  eNewSym: PTESShalfEdge;
  eNew: PTESShalfEdge;
  newVertex: PTESSvertex;
begin
  eNew := MakeEdge(mesh, eOrg);
  if eNew = nil then
    Exit(nil);

  eNewSym := eNew^.Sym;

  // Connect the new edge appropriately
  Splice(eNew, eOrg^.Lnext);

  // Set the vertex and face information
  eNew^.Org := eOrg^.Dst;
  begin
    newVertex := PTESSvertex(bucketAlloc(mesh^.vertexBucket));
    if newVertex = nil then
      Exit(nil);

    MakeVertex(newVertex, eNewSym, eNew^.Org);
  end;
  eNew^.Lface := eOrg^.Lface;
  eNewSym^.Lface := eOrg^.Lface;

  Exit(eNew);
end;


//  tessMeshSplitEdge( eOrg ) splits eOrg into two edges eOrg and eNew,
// such that eNew == eOrg->Lnext.  The new vertex is eOrg->Dst == eNew->Org.
// eOrg and eNew will have the same left face.
//
function tessMeshSplitEdge(mesh: PTESSmesh; eOrg: PTESShalfEdge): PTESShalfEdge;
var
  eNew: PTESShalfEdge;
  tempHalfEdge: PTESShalfEdge;
begin
  tempHalfEdge := tessMeshAddEdgeVertex(mesh, eOrg);
  if tempHalfEdge = nil then
    Exit(nil);

  eNew := tempHalfEdge^.Sym;

  // Disconnect eOrg from eOrg->Dst and connect it to eNew->Org
  Splice(eOrg^.Sym, eOrg^.Sym^.Oprev);
  Splice(eOrg^.Sym, eNew);

  // Set the vertex and face information
  eOrg^.Dst := eNew^.Org;
  eNew^.Dst^.anEdge := eNew^.Sym;  // may have pointed to eOrg->Sym
  eNew^.Rface := eOrg^.Rface;
  eNew^.winding := eOrg^.winding;  // copy old winding information
  eNew^.Sym^.winding := eOrg^.Sym^.winding;

  Exit(eNew);
end;


//  tessMeshConnect( eOrg, eDst ) creates a new edge from eOrg->Dst
// to eDst->Org, and returns the corresponding half-edge eNew.
// If eOrg->Lface == eDst->Lface, this splits one loop into two,
// and the newly created loop is eNew->Lface.  Otherwise, two disjoint
// loops are merged into one, and the loop eDst->Lface is destroyed.
//
// If (eOrg == eDst), the new face will have only two edges.
// If (eOrg->Lnext == eDst), the old face is reduced to a single edge.
// If (eOrg->Lnext->Lnext == eDst), the old face is reduced to two edges.
//
function tessMeshConnect(mesh: PTESSmesh; eOrg: PTESShalfEdge; eDst: PTESShalfEdge): PTESShalfEdge;
var
  eNewSym: PTESShalfEdge;
  joiningLoops: Boolean;
  eNew: PTESShalfEdge;
  newFace: PTESSface;
begin
  joiningLoops := False;
  eNew := MakeEdge(mesh, eOrg);
  if eNew = nil then
    Exit(nil);

  eNewSym := eNew^.Sym;

  if eDst^.Lface <> eOrg^.Lface then begin
    // We are connecting two disjoint loops -- destroy eDst^.Lface
    joiningLoops := True;
    KillFace(mesh, eDst^.Lface, eOrg^.Lface);
  end;

  // Connect the new edge appropriately
  Splice(eNew, eOrg^.Lnext);
  Splice(eNewSym, eDst);

  // Set the vertex and face information
  eNew^.Org := eOrg^.Dst;
  eNewSym^.Org := eDst^.Org;
  eNew^.Lface := eOrg^.Lface;
  eNewSym^.Lface := eOrg^.Lface;

  // Make sure the old face points to a valid half-edge
  eOrg^.Lface^.anEdge := eNewSym;

  if not joiningLoops then begin
    newFace := PTESSface(bucketAlloc(mesh^.faceBucket));
    if newFace = nil then
      Exit(nil);

    // We split one loop into two -- the new loop is eNew->Lface
    MakeFace(newFace, eNew, eOrg^.Lface);
  end;
  Exit(eNew);
end;


//******************* Other Operations **********************

//  tessMeshZapFace( fZap ) destroys a face and removes it from the
// global face list.  All edges of fZap will have a NULL pointer as their
// left face.  Any edges which also have a NULL pointer as their right face
// are deleted entirely (along with any isolated vertices this produces).
// An entire mesh can be deleted by zapping its faces, one at a time,
// in any order.  Zapped faces cannot be used in further mesh operations!
//
procedure tessMeshZapFace(mesh: PTESSmesh; fZap: PTESSface);
var
  eStart: PTESShalfEdge;
  e, eNext, eSym: PTESShalfEdge;
  fPrev, fNext: PTESSface;
begin
  eStart := fZap^.anEdge;

  // walk around face, deleting edges whose right face is also NULL
  eNext := eStart^.Lnext;
  repeat
    e := eNext;
    eNext := e^.Lnext;

    e^.Lface := nil;
    if e^.Rface = nil then begin
      // delete the edge -- see TESSmeshDelete above

      if e^.Onext = e then begin
        KillVertex(mesh, e^.Org, nil);
      end else begin
        // Make sure that e->Org points to a valid half-edge
        e^.Org^.anEdge := e^.Onext;
        Splice(e, e^.Oprev);
      end;
      eSym := e^.Sym;
      if eSym^.Onext = eSym then begin
        KillVertex(mesh, eSym^.Org, nil);
      end else begin
        // Make sure that eSym->Org points to a valid half-edge
        eSym^.Org^.anEdge := eSym^.Onext;
        Splice(eSym, eSym^.Oprev);
      end;
      KillEdge(mesh, e);
    end;
  until e = eStart;

  // delete from circular doubly-linked list
  fPrev := fZap^.prev;
  fNext := fZap^.next;
  fNext^.prev := fPrev;
  fPrev^.next := fNext;

  bucketFree(mesh^.faceBucket, fZap);
end;


//  tessMeshNewMesh() creates a new mesh with no edges, no vertices,
// and no loops (what we usually call a "face").
//
function tessMeshNewMesh(alloc: PTESSalloc): PTESSmesh;
var
  v: PTESSvertex;
  f: PTESSface;
  e: PTESShalfEdge;
  eSym: PTESShalfEdge;
  mesh: PTESSmesh;
begin
  mesh := PTESSmesh(alloc^.memalloc(alloc^.userData, SizeOf(TESSmesh)));
  if mesh = nil then begin
    Exit(nil);
  end;

  if alloc^.meshEdgeBucketSize < 16 then
    alloc^.meshEdgeBucketSize := 16;
  if alloc^.meshEdgeBucketSize > 4096 then
    alloc^.meshEdgeBucketSize := 4096;

  if alloc^.meshVertexBucketSize < 16 then
    alloc^.meshVertexBucketSize := 16;
  if alloc^.meshVertexBucketSize > 4096 then
    alloc^.meshVertexBucketSize := 4096;

  if alloc^.meshFaceBucketSize < 16 then
    alloc^.meshFaceBucketSize := 16;
  if alloc^.meshFaceBucketSize > 4096 then
    alloc^.meshFaceBucketSize := 4096;

  mesh^.edgeBucket := createBucketAlloc(alloc, 'Mesh Edges', SizeOf(TEdgePair), alloc^.meshEdgeBucketSize);
  mesh^.vertexBucket := createBucketAlloc(alloc, 'Mesh Vertices', SizeOf(TESSvertex), alloc^.meshVertexBucketSize);
  mesh^.faceBucket := createBucketAlloc(alloc, 'Mesh Faces', SizeOf(TESSface), alloc^.meshFaceBucketSize);

  v := @mesh^.vHead;
  f := @mesh^.fHead;
  e := @mesh^.eHead;
  eSym := @mesh^.eHeadSym;

  v^.next := v;
  v^.prev := v;
  v^.anEdge := nil;

  f^.next := f;
  f^.prev := f;
  f^.anEdge := nil;
  f^.trail := nil;
  f^.marked := False;
  f^.inside := False;

  e^.next := e;
  e^.Sym := eSym;
  e^.Onext := nil;
  e^.Lnext := nil;
  e^.Org := nil;
  e^.Lface := nil;
  e^.winding := 0;
  e^.activeRegion := nil;

  eSym^.next := eSym;
  eSym^.Sym := e;
  eSym^.Onext := nil;
  eSym^.Lnext := nil;
  eSym^.Org := nil;
  eSym^.Lface := nil;
  eSym^.winding := 0;
  eSym^.activeRegion := nil;

  Exit(mesh);
end;


//  tessMeshUnion( mesh1, mesh2 ) forms the union of all structures in
// both meshes, and returns the new mesh (the old meshes are destroyed).
//
function tessMeshUnion(alloc: PTESSalloc; mesh1, mesh2: PTESSmesh): PTESSmesh;
var
  f1: PTESSface;
  v1: PTESSvertex;
  e1: PTESShalfEdge;
  f2: PTESSface;
  v2: PTESSvertex;
  e2: PTESShalfEdge;
begin
  f1 := @mesh1^.fHead;
  v1 := @mesh1^.vHead;
  e1 := @mesh1^.eHead;
  f2 := @mesh2^.fHead;
  v2 := @mesh2^.vHead;
  e2 := @mesh2^.eHead;

  // Add the faces, vertices, and edges of mesh2 to those of mesh1
  if f2^.next <> f2  then begin
    f1^.prev^.next := f2^.next;
    f2^.next^.prev := f1^.prev;
    f2^.prev^.next := f1;
    f1^.prev := f2^.prev;
  end;

  if v2^.next <> v2 then begin
    v1^.prev^.next := v2^.next;
    v2^.next^.prev := v1^.prev;
    v2^.prev^.next := v1;
    v1^.prev := v2^.prev;
  end;

  if e2^.next <> e2 then begin
    e1^.Sym^.next^.Sym^.next := e2^.next;
    e2^.next^.Sym^.next := e1^.Sym^.next;
    e2^.Sym^.next^.Sym^.next := e1;
    e1^.Sym^.next := e2^.Sym^.next;
  end;

  alloc^.memfree(alloc^.userData, mesh2);
  Exit(mesh1);
end;

function CountFaceVerts(f: PTESSface): PtrInt;
var
  eCur: PTESShalfEdge;
  n: PtrInt;
begin
  eCur := f^.anEdge;
  n := 0;
  repeat
    Inc(n);
    eCur := eCur^.Lnext;
  until eCur = f^.anEdge;
  Exit(n);
end;

function tessMeshMergeConvexFaces(mesh: PTESSmesh; maxVertsPerFace: PtrInt): Boolean;
label
  LContinue;
var
  e, eNext, eSym: PTESShalfEdge;
  eHead: PTESShalfEdge;
  va, vb, vc, vd, ve, vf: PTESSvertex;
  leftNv, rightNv: PtrInt;
begin
  eHead := @mesh^.eHead;

  e := eHead^.next;
  while e <> eHead do begin
    eNext := e^.next;
    eSym := e^.Sym;
    if eSym = nil then
      goto LContinue;

    // Both faces must be inside
    if (e^.Lface = nil) or not e^.Lface^.inside then
      goto LContinue;
    if (eSym^.Lface = nil) or not eSym^.Lface^.inside then
      goto LContinue;

    leftNv := CountFaceVerts(e^.Lface);
    rightNv := CountFaceVerts(eSym^.Lface);
    if (leftNv + rightNv - 2) > maxVertsPerFace then
      goto LContinue;

    // Merge if the resulting poly is convex.
    //
    //      vf--ve--vd
    //          ^|
    // left   e ||   right
    //          |v
    //      va--vb--vc

    va := e^.Lprev^.Org;
    vb := e^.Org;
    vc := e^.Sym^.Lnext^.Dst;

    vd := e^.Sym^.Lprev^.Org;
    ve := e^.Sym^.Org;
    vf := e^.Lnext^.Dst;

    if VertCCW(va, vb, vc) and VertCCW(vd, ve, vf) then begin
      if (e = eNext) or (e = eNext^.Sym) then begin
        eNext := eNext^.next;
      end;
      if not tessMeshDelete(mesh, e) then
        Exit(False);
    end;
  LContinue:
    e := eNext;
  end;

  Exit(True);
end;

procedure tessMeshFlipEdge(mesh: PTESSmesh; edge: PTESShalfEdge);
var
  a0: PTESShalfEdge;
  a1: PTESShalfEdge;
  a2: PTESShalfEdge;
  b0: PTESShalfEdge;
  b1: PTESShalfEdge;
  b2: PTESShalfEdge;
  aOrg: PTESSvertex;
  aOpp: PTESSvertex;
  bOrg: PTESSvertex;
  bOpp: PTESSvertex;
  fa: PTESSface;
  fb: PTESSface;
begin
  a0 := edge;
  a1 := a0^.Lnext;
  a2 := a1^.Lnext;
  b0 := edge^.Sym;
  b1 := b0^.Lnext;
  b2 := b1^.Lnext;

  aOrg := a0^.Org;
  aOpp := a2^.Org;
  bOrg := b0^.Org;
  bOpp := b2^.Org;

  fa := a0^.Lface;
  fb := b0^.Lface;

  Assert(EdgeIsInternal(edge));
  Assert(a2^.Lnext = a0);
  Assert(b2^.Lnext = b0);

  a0^.Org := bOpp;
  a0^.Onext := b1^.Sym;
  b0^.Org := aOpp;
  b0^.Onext := a1^.Sym;
  a2^.Onext := b0;
  b2^.Onext := a0;
  b1^.Onext := a2^.Sym;
  a1^.Onext := b2^.Sym;

  a0^.Lnext := a2;
  a2^.Lnext := b1;
  b1^.Lnext := a0;

  b0^.Lnext := b2;
  b2^.Lnext := a1;
  a1^.Lnext := b0;

  a1^.Lface := fb;
  b1^.Lface := fa;

  fa^.anEdge := a0;
  fb^.anEdge := b0;

  if aOrg^.anEdge = a0 then
    aOrg^.anEdge := b1;
  if bOrg^.anEdge = b0 then
    bOrg^.anEdge := a1;

  Assert(a0^.Lnext^.Onext^.Sym = a0);
  Assert(a0^.Onext^.Sym^.Lnext = a0);
  Assert(a0^.Org^.anEdge^.Org = a0^.Org);


  Assert(a1^.Lnext^.Onext^.Sym = a1);
  Assert(a1^.Onext^.Sym^.Lnext = a1);
  Assert(a1^.Org^.anEdge^.Org = a1^.Org);

  Assert(a2^.Lnext^.Onext^.Sym = a2);
  Assert(a2^.Onext^.Sym^.Lnext = a2);
  Assert(a2^.Org^.anEdge^.Org = a2^.Org);

  Assert(b0^.Lnext^.Onext^.Sym = b0);
  Assert(b0^.Onext^.Sym^.Lnext = b0);
  Assert(b0^.Org^.anEdge^.Org = b0^.Org);

  Assert(b1^.Lnext^.Onext^.Sym = b1);
  Assert(b1^.Onext^.Sym^.Lnext = b1);
  Assert(b1^.Org^.anEdge^.Org = b1^.Org);

  Assert(b2^.Lnext^.Onext^.Sym = b2);
  Assert(b2^.Onext^.Sym^.Lnext = b2);
  Assert(b2^.Org^.anEdge^.Org = b2^.Org);

  Assert(aOrg^.anEdge^.Org = aOrg);
  Assert(bOrg^.anEdge^.Org = bOrg);

  Assert(a0^.Oprev^.Onext^.Org = a0^.Org);
end;

{$IF Defined(DELETE_BY_ZAPPING)}

// tessMeshDeleteMesh( mesh ) will free all storage for any valid mesh.
//
procedure tessMeshDeleteMesh(alloc: PTESSalloc; mesh: PTESSmesh);
var
  fHead: PTESSface;
begin
  fHead := @mesh^.fHead;

  while fHead^.next <> fHead do begin
    tessMeshZapFace(fHead^.next);
  end;
  Assert(mesh^.vHead.next = @mesh^.vHead);

  alloc^.memfree(alloc^.userData, mesh);
end;

{$ELSE}

// tessMeshDeleteMesh( mesh ) will free all storage for any valid mesh.
//
procedure tessMeshDeleteMesh(alloc: PTESSalloc; mesh: PTESSmesh);
begin
  deleteBucketAlloc(mesh^.edgeBucket);
  deleteBucketAlloc(mesh^.vertexBucket);
  deleteBucketAlloc(mesh^.faceBucket);

  alloc^.memfree( alloc^.userData, mesh );
end;

{$ENDIF}

// tessMeshCheckMesh( mesh ) checks a mesh for self-consistency.
procedure tessMeshCheckMesh(mesh: PTESSmesh);
{$IFDEF NDEBUG}
begin
end;
{$ELSE}
var
  fHead: PTESSface;
  vHead: PTESSvertex;
  eHead: PTESShalfEdge;
  f, fPrev: PTESSface;
  v, vPrev: PTESSvertex;
  e, ePrev: PTESShalfEdge;
begin
  fHead := @mesh^.fHead;
  vHead := @mesh^.vHead;
  eHead := @mesh^.eHead;

  fPrev := fHead;
  f := fPrev^.next;
  while f <> fHead do begin
    Assert(f^.prev = fPrev);
    e := f^.anEdge;
    repeat
      Assert(e^.Sym <> e);
      Assert(e^.Sym^.Sym = e);
      Assert(e^.Lnext^.Onext^.Sym = e);
      Assert(e^.Onext^.Sym^.Lnext = e);
      Assert(e^.Lface = f);
      e := e^.Lnext;
    until e = f^.anEdge;
    fPrev := f;
    f := fPrev^.next;
  end;
  Assert((f^.prev = fPrev) and (f^.anEdge = nil));

  vPrev := vHead;
  v := vPrev^.next;
  while v <> vHead do begin
    Assert(v^.prev = vPrev);
    e := v^.anEdge;
    repeat
      Assert(e^.Sym <> e);
      Assert(e^.Sym^.Sym = e);
      Assert(e^.Lnext^.Onext^.Sym = e);
      Assert(e^.Onext^.Sym^.Lnext = e);
      Assert(e^.Org = v);
      e := e^.Onext;
    until e = v^.anEdge;
    vPrev := v;
    v := vPrev^.next
  end;
  Assert((v^.prev = vPrev) and (v^.anEdge = nil));

  ePrev := eHead;
  e := ePrev^.next;
  while e <> eHead do begin
    Assert(e^.Sym^.next = ePrev^.Sym);
    Assert(e^.Sym <> e);
    Assert(e^.Sym^.Sym = e);
    Assert(e^.Org <> nil);
    Assert(e^.Dst <> nil);
    Assert(e^.Lnext^.Onext^.Sym = e);
    Assert(e^.Onext^.Sym^.Lnext = e);
    ePrev := e;
    e := ePrev^.next;
  end;
  Assert((e^.Sym^.next = ePrev^.Sym)
    and (e^.Sym = @mesh^.eHeadSym)
    and (e^.Sym^.Sym = e)
    and (e^.Org = nil) and (e^.Dst = nil)
    and (e^.Lface = nil) and (e^.Rface = nil));
end;
{$ENDIF}

end.
