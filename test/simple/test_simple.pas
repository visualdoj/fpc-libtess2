program test_simple;

{
  Simple unit tests for libtess2 Pascal port.

  Test 1: Triangulate a unit square (4 vertices)
          Expected: 2 triangles, 4 vertices

  Test 2: Triangulate a simple triangle (3 vertices)
          Expected: 1 triangle, 3 vertices

  Test 3: Triangulate two overlapping triangles with ODD winding rule
          Expected: triangulation of the XOR region
}

uses
  libtess2_tesselator;

var
  TestsPassed: Integer = 0;
  TestsFailed: Integer = 0;

procedure Check(Condition: Boolean; const TestName: String);
begin
  if Condition then begin
    Writeln('[PASS] ', TestName);
    Inc(TestsPassed);
  end else begin
    Writeln('[FAIL] ', TestName);
    Inc(TestsFailed);
  end;
end;

procedure TestSquareTriangulation;
const
  // Unit square: (0,0) -> (1,0) -> (1,1) -> (0,1)
  Square: array[0..7] of TESSreal = (
    0.0, 0.0,
    1.0, 0.0,
    1.0, 1.0,
    0.0, 1.0
  );
var
  Tess: PTESStesselator;
  VertexCount, ElementCount: Integer;
begin
  Writeln;
  Writeln('Test 1: Square triangulation');
  Writeln('  Input: Unit square with 4 vertices');
  Writeln('  Expected: 2 triangles');

  Tess := tessNewTess(nil);
  Check(Tess <> nil, 'Tesselator created');

  if Tess = nil then Exit;

  tessAddContour(Tess, 2, @Square[0], 2 * SizeOf(TESSreal), 4);

  Check(
    tessTesselate(Tess, TESS_WINDING_ODD, TESS_POLYGONS, 3, 2, nil),
    'Tesselation succeeded'
  );

  VertexCount := tessGetVertexCount(Tess);
  ElementCount := tessGetElementCount(Tess);

  Writeln('  Result: ', ElementCount, ' triangles, ', VertexCount, ' vertices');

  Check(VertexCount = 4, 'Vertex count = 4');
  Check(ElementCount = 2, 'Triangle count = 2');

  tessDeleteTess(Tess);
end;

procedure TestTriangleTriangulation;
const
  // Simple triangle
  Triangle: array[0..5] of TESSreal = (
    0.0, 0.0,
    2.0, 0.0,
    1.0, 2.0
  );
var
  Tess: PTESStesselator;
  VertexCount, ElementCount: Integer;
begin
  Writeln;
  Writeln('Test 2: Triangle triangulation');
  Writeln('  Input: Simple triangle with 3 vertices');
  Writeln('  Expected: 1 triangle (unchanged)');

  Tess := tessNewTess(nil);
  Check(Tess <> nil, 'Tesselator created');

  if Tess = nil then Exit;

  tessAddContour(Tess, 2, @Triangle[0], 2 * SizeOf(TESSreal), 3);

  Check(
    tessTesselate(Tess, TESS_WINDING_ODD, TESS_POLYGONS, 3, 2, nil),
    'Tesselation succeeded'
  );

  VertexCount := tessGetVertexCount(Tess);
  ElementCount := tessGetElementCount(Tess);

  Writeln('  Result: ', ElementCount, ' triangles, ', VertexCount, ' vertices');

  Check(VertexCount = 3, 'Vertex count = 3');
  Check(ElementCount = 1, 'Triangle count = 1');

  tessDeleteTess(Tess);
end;

procedure TestOverlappingTriangles;
const
  // Two overlapping triangles - tests the ODD winding rule
  // Triangle 1: pointing up
  Triangle1: array[0..5] of TESSreal = (
    0.0, 0.0,
    4.0, 0.0,
    2.0, 3.0
  );
  // Triangle 2: pointing down, overlapping with Triangle 1
  Triangle2: array[0..5] of TESSreal = (
    0.0, 2.0,
    4.0, 2.0,
    2.0, -1.0
  );
var
  Tess: PTESStesselator;
  VertexCount, ElementCount: Integer;
begin
  Writeln;
  Writeln('Test 3: Overlapping triangles (ODD winding)');
  Writeln('  Input: Two overlapping triangles');
  Writeln('  Expected: XOR region triangulated (overlap excluded)');

  Tess := tessNewTess(nil);
  Check(Tess <> nil, 'Tesselator created');

  if Tess = nil then Exit;

  tessAddContour(Tess, 2, @Triangle1[0], 2 * SizeOf(TESSreal), 3);
  tessAddContour(Tess, 2, @Triangle2[0], 2 * SizeOf(TESSreal), 3);

  Check(
    tessTesselate(Tess, TESS_WINDING_ODD, TESS_POLYGONS, 3, 2, nil),
    'Tesselation succeeded'
  );

  VertexCount := tessGetVertexCount(Tess);
  ElementCount := tessGetElementCount(Tess);

  Writeln('  Result: ', ElementCount, ' triangles, ', VertexCount, ' vertices');

  // With ODD winding, overlapping region is excluded
  // The result should have more triangles than 2 due to intersection points
  Check(ElementCount > 2, 'Triangle count > 2 (intersections create new triangles)');
  Check(VertexCount > 6, 'Vertex count > 6 (intersection points added)');

  tessDeleteTess(Tess);
end;

begin
  Writeln('===========================================');
  Writeln('libtess2 Pascal Port - Simple Unit Tests');
  Writeln('===========================================');

  TestSquareTriangulation;
  TestTriangleTriangulation;
  TestOverlappingTriangles;

  Writeln;
  Writeln('===========================================');
  Writeln('Results: ', TestsPassed, ' passed, ', TestsFailed, ' failed');
  Writeln('===========================================');

  if TestsFailed > 0 then
    Halt(1);
end.
