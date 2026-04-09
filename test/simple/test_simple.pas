program test_simple;

{
  Simple unit tests for libtess2 Pascal port.

  Test 1: Triangulate a unit square (4 vertices)
          Expected: 2 triangles, 4 vertices

  Test 2: Triangulate a simple triangle (3 vertices)
          Expected: 1 triangle, 3 vertices

  Test 3: Triangulate two overlapping triangles with ODD winding rule
          Expected: triangulation of the XOR region

  Test 4: Winding rules comparison
          NONZERO fills union, ODD fills XOR

  Test 5: Polygon with hole (donut shape)
          Outer square CCW + inner square CW

  Test 6: Concave polygon (L-shape)
          Tests handling of non-convex polygons

  Test 7: Boundary contours element type
          Returns contour outlines instead of triangles

  Test 8: Quad output (polySize=4)
          Request quads instead of triangles

  Test 9: Vertex index mapping
          tessGetVertexIndices maps output to input vertices

  Test 10: Constrained Delaunay option
           TESS_CONSTRAINED_DELAUNAY_TRIANGULATION option

  Test 11: Empty and degenerate cases
           No contours, collinear points

  Test 12: Multiple separate polygons
           Two non-overlapping shapes
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

procedure TestWindingRules;
const
  // Two overlapping squares - same area covered twice
  // Square 1: (0,0) to (2,2)
  Square1: array[0..7] of TESSreal = (
    0.0, 0.0,
    2.0, 0.0,
    2.0, 2.0,
    0.0, 2.0
  );
  // Square 2: (1,1) to (3,3) - overlaps with Square1
  Square2: array[0..7] of TESSreal = (
    1.0, 1.0,
    3.0, 1.0,
    3.0, 3.0,
    1.0, 3.0
  );
var
  Tess: PTESStesselator;
  CountOdd, CountNonzero: Integer;
begin
  Writeln;
  Writeln('Test 4: Winding rules (NONZERO vs ODD)');
  Writeln('  Input: Two overlapping squares');

  // Test with ODD winding - excludes overlap (XOR)
  Tess := tessNewTess(nil);
  Check(Tess <> nil, 'Tesselator created (ODD)');
  if Tess = nil then Exit;

  tessAddContour(Tess, 2, @Square1[0], 2 * SizeOf(TESSreal), 4);
  tessAddContour(Tess, 2, @Square2[0], 2 * SizeOf(TESSreal), 4);
  tessTesselate(Tess, TESS_WINDING_ODD, TESS_POLYGONS, 3, 2, nil);
  CountOdd := tessGetElementCount(Tess);
  Writeln('  ODD winding (XOR): ', CountOdd, ' triangles');
  tessDeleteTess(Tess);

  // Test with NONZERO winding - includes overlap (union)
  Tess := tessNewTess(nil);
  Check(Tess <> nil, 'Tesselator created (NONZERO)');
  if Tess = nil then Exit;

  tessAddContour(Tess, 2, @Square1[0], 2 * SizeOf(TESSreal), 4);
  tessAddContour(Tess, 2, @Square2[0], 2 * SizeOf(TESSreal), 4);
  tessTesselate(Tess, TESS_WINDING_NONZERO, TESS_POLYGONS, 3, 2, nil);
  CountNonzero := tessGetElementCount(Tess);
  Writeln('  NONZERO winding (union): ', CountNonzero, ' triangles');
  tessDeleteTess(Tess);

  // ODD excludes overlap (XOR), NONZERO includes it (union)
  // They should produce different results
  Check(CountOdd > 0, 'ODD produces triangles');
  Check(CountNonzero > 0, 'NONZERO produces triangles');
  Check(CountOdd <> CountNonzero, 'ODD and NONZERO produce different triangle counts');
end;

procedure TestPolygonWithHole;
const
  // Outer square (CCW winding) - 10x10
  Outer: array[0..7] of TESSreal = (
    0.0, 0.0,
   10.0, 0.0,
   10.0, 10.0,
    0.0, 10.0
  );
  // Inner square (CW winding - hole) - 4x4 centered
  Inner: array[0..7] of TESSreal = (
    3.0, 3.0,
    3.0, 7.0,
    7.0, 7.0,
    7.0, 3.0
  );
var
  Tess: PTESStesselator;
  VertexCount, ElementCount: Integer;
begin
  Writeln;
  Writeln('Test 5: Polygon with hole (donut)');
  Writeln('  Input: Outer square CCW + inner square CW');
  Writeln('  Expected: Donut shape (hole excluded)');

  Tess := tessNewTess(nil);
  Check(Tess <> nil, 'Tesselator created');
  if Tess = nil then Exit;

  tessAddContour(Tess, 2, @Outer[0], 2 * SizeOf(TESSreal), 4);
  tessAddContour(Tess, 2, @Inner[0], 2 * SizeOf(TESSreal), 4);

  Check(
    tessTesselate(Tess, TESS_WINDING_ODD, TESS_POLYGONS, 3, 2, nil),
    'Tesselation succeeded'
  );

  VertexCount := tessGetVertexCount(Tess);
  ElementCount := tessGetElementCount(Tess);

  Writeln('  Result: ', ElementCount, ' triangles, ', VertexCount, ' vertices');

  // Donut needs more than 2 triangles (a simple square would be 2)
  Check(VertexCount = 8, 'Vertex count = 8 (outer + inner)');
  Check(ElementCount > 2, 'Triangle count > 2 (donut shape)');

  tessDeleteTess(Tess);
end;

procedure TestConcavePolygon;
const
  // L-shaped polygon (6 vertices)
  //   ##
  //   #
  //   ##
  LShape: array[0..11] of TESSreal = (
    0.0, 0.0,
    2.0, 0.0,
    2.0, 1.0,
    1.0, 1.0,
    1.0, 2.0,
    0.0, 2.0
  );
var
  Tess: PTESStesselator;
  VertexCount, ElementCount: Integer;
begin
  Writeln;
  Writeln('Test 6: Concave polygon (L-shape)');
  Writeln('  Input: L-shaped polygon with 6 vertices');
  Writeln('  Expected: Multiple triangles for concave shape');

  Tess := tessNewTess(nil);
  Check(Tess <> nil, 'Tesselator created');
  if Tess = nil then Exit;

  tessAddContour(Tess, 2, @LShape[0], 2 * SizeOf(TESSreal), 6);

  Check(
    tessTesselate(Tess, TESS_WINDING_ODD, TESS_POLYGONS, 3, 2, nil),
    'Tesselation succeeded'
  );

  VertexCount := tessGetVertexCount(Tess);
  ElementCount := tessGetElementCount(Tess);

  Writeln('  Result: ', ElementCount, ' triangles, ', VertexCount, ' vertices');

  Check(VertexCount = 6, 'Vertex count = 6');
  // L-shape needs at least 4 triangles (area = 3 units, each triangle ~ 0.5-1 unit)
  Check(ElementCount >= 4, 'Triangle count >= 4 (L-shape decomposition)');

  tessDeleteTess(Tess);
end;

procedure TestBoundaryContours;
const
  // Square with a hole - should produce 2 boundary contours
  Outer: array[0..7] of TESSreal = (
    0.0, 0.0,
   10.0, 0.0,
   10.0, 10.0,
    0.0, 10.0
  );
  Inner: array[0..7] of TESSreal = (
    3.0, 3.0,
    3.0, 7.0,
    7.0, 7.0,
    7.0, 3.0
  );
var
  Tess: PTESStesselator;
  ElementCount: Integer;
  Elements: PTESSindex;
begin
  Writeln;
  Writeln('Test 7: Boundary contours element type');
  Writeln('  Input: Square with hole');
  Writeln('  Expected: 2 boundary contours (outer + inner)');

  Tess := tessNewTess(nil);
  Check(Tess <> nil, 'Tesselator created');
  if Tess = nil then Exit;

  tessAddContour(Tess, 2, @Outer[0], 2 * SizeOf(TESSreal), 4);
  tessAddContour(Tess, 2, @Inner[0], 2 * SizeOf(TESSreal), 4);

  Check(
    tessTesselate(Tess, TESS_WINDING_ODD, TESS_BOUNDARY_CONTOURS, 0, 2, nil),
    'Tesselation succeeded'
  );

  ElementCount := tessGetElementCount(Tess);
  Elements := tessGetElements(Tess);

  Writeln('  Result: ', ElementCount, ' contours');

  Check(ElementCount = 2, 'Contour count = 2 (outer + inner boundary)');
  // Each contour element is [base, count] pair
  Check(Elements[1] = 4, 'First contour has 4 vertices');
  Check(Elements[3] = 4, 'Second contour has 4 vertices');

  tessDeleteTess(Tess);
end;

procedure TestQuadOutput;
const
  // Simple square - should produce 1 quad (not 2 triangles)
  Square: array[0..7] of TESSreal = (
    0.0, 0.0,
    1.0, 0.0,
    1.0, 1.0,
    0.0, 1.0
  );
var
  Tess: PTESStesselator;
  ElementCount: Integer;
  Elements: PTESSindex;
  HasUndefPadding: Boolean;
begin
  Writeln;
  Writeln('Test 8: Quad output (polySize=4)');
  Writeln('  Input: Simple square');
  Writeln('  Expected: 1 quad or triangles with TESS_UNDEF padding');

  Tess := tessNewTess(nil);
  Check(Tess <> nil, 'Tesselator created');
  if Tess = nil then Exit;

  tessAddContour(Tess, 2, @Square[0], 2 * SizeOf(TESSreal), 4);

  Check(
    tessTesselate(Tess, TESS_WINDING_ODD, TESS_POLYGONS, 4, 2, nil),
    'Tesselation succeeded'
  );

  ElementCount := tessGetElementCount(Tess);
  Elements := tessGetElements(Tess);

  Writeln('  Result: ', ElementCount, ' polygons (polySize=4)');

  Check(ElementCount >= 1, 'At least 1 polygon');

  // Check if triangles are padded with TESS_UNDEF
  // For a triangle in quad mode, index 3 would be TESS_UNDEF
  HasUndefPadding := (ElementCount > 1) and (Elements[3] = TESSindex(TESS_UNDEF));
  if HasUndefPadding then
    Writeln('  Note: Triangles padded with TESS_UNDEF as expected');

  tessDeleteTess(Tess);
end;

procedure TestVertexIndices;
const
  // Simple square - no intersections, all vertices should map to originals
  Square: array[0..7] of TESSreal = (
    0.0, 0.0,
    1.0, 0.0,
    1.0, 1.0,
    0.0, 1.0
  );
  // Two intersecting lines - will create new vertices
  Line1: array[0..3] of TESSreal = (0.0, 0.0, 2.0, 2.0);
  Line2: array[0..3] of TESSreal = (0.0, 2.0, 2.0, 0.0);
var
  Tess: PTESStesselator;
  VertexCount: Integer;
  Indices: PTESSindex;
  I: Integer;
  AllValid: Boolean;
begin
  Writeln;
  Writeln('Test 9: Vertex index mapping');
  Writeln('  Input: Simple square (no intersections)');
  Writeln('  Expected: All indices map to original vertices (0-3)');

  Tess := tessNewTess(nil);
  Check(Tess <> nil, 'Tesselator created');
  if Tess = nil then Exit;

  tessAddContour(Tess, 2, @Square[0], 2 * SizeOf(TESSreal), 4);
  tessTesselate(Tess, TESS_WINDING_ODD, TESS_POLYGONS, 3, 2, nil);

  VertexCount := tessGetVertexCount(Tess);
  Indices := tessGetVertexIndices(Tess);

  AllValid := True;
  for I := 0 to VertexCount - 1 do begin
    if (Indices[I] < 0) or (Indices[I] > 3) then begin
      AllValid := False;
      Break;
    end;
  end;

  Check(AllValid, 'All vertex indices map to original vertices (0-3)');
  Check(Indices <> nil, 'Vertex indices array returned');

  tessDeleteTess(Tess);
end;

procedure TestConstrainedDelaunay;
const
  // Rectangle - CDT may produce different triangulation than default
  Rect: array[0..7] of TESSreal = (
    0.0, 0.0,
    4.0, 0.0,
    4.0, 1.0,
    0.0, 1.0
  );
var
  Tess: PTESStesselator;
  CountDefault, CountCDT: Integer;
begin
  Writeln;
  Writeln('Test 10: Constrained Delaunay triangulation option');
  Writeln('  Input: Rectangle (4x1)');

  // Without CDT
  Tess := tessNewTess(nil);
  Check(Tess <> nil, 'Tesselator created (default)');
  if Tess = nil then Exit;

  tessAddContour(Tess, 2, @Rect[0], 2 * SizeOf(TESSreal), 4);
  tessTesselate(Tess, TESS_WINDING_ODD, TESS_POLYGONS, 3, 2, nil);
  CountDefault := tessGetElementCount(Tess);
  Writeln('  Default: ', CountDefault, ' triangles');
  tessDeleteTess(Tess);

  // With CDT enabled
  Tess := tessNewTess(nil);
  Check(Tess <> nil, 'Tesselator created (CDT)');
  if Tess = nil then Exit;

  tessSetOption(Tess, TESS_CONSTRAINED_DELAUNAY_TRIANGULATION, 1);
  tessAddContour(Tess, 2, @Rect[0], 2 * SizeOf(TESSreal), 4);
  Check(
    tessTesselate(Tess, TESS_WINDING_ODD, TESS_POLYGONS, 3, 2, nil),
    'CDT tesselation succeeded'
  );
  CountCDT := tessGetElementCount(Tess);
  Writeln('  CDT: ', CountCDT, ' triangles');
  tessDeleteTess(Tess);

  // Both should produce valid triangulations
  Check(CountDefault = 2, 'Default produces 2 triangles');
  Check(CountCDT = 2, 'CDT produces 2 triangles');
end;

procedure TestDegenerateCases;
const
  // Collinear points (a line, not a polygon)
  Line: array[0..5] of TESSreal = (
    0.0, 0.0,
    1.0, 1.0,
    2.0, 2.0
  );
  // Two points (degenerate)
  TwoPoints: array[0..3] of TESSreal = (
    0.0, 0.0,
    1.0, 1.0
  );
var
  Tess: PTESStesselator;
  ElementCount: Integer;
begin
  Writeln;
  Writeln('Test 11: Degenerate cases');

  // Test collinear points
  Writeln('  Input: 3 collinear points (a line)');
  Tess := tessNewTess(nil);
  Check(Tess <> nil, 'Tesselator created (collinear)');
  if Tess = nil then Exit;

  tessAddContour(Tess, 2, @Line[0], 2 * SizeOf(TESSreal), 3);
  tessTesselate(Tess, TESS_WINDING_ODD, TESS_POLYGONS, 3, 2, nil);
  ElementCount := tessGetElementCount(Tess);
  Writeln('  Result: ', ElementCount, ' triangles');
  Check(ElementCount = 0, 'Collinear points produce 0 triangles');
  tessDeleteTess(Tess);

  // Test two points
  Writeln('  Input: 2 points (degenerate polygon)');
  Tess := tessNewTess(nil);
  Check(Tess <> nil, 'Tesselator created (two points)');
  if Tess = nil then Exit;

  tessAddContour(Tess, 2, @TwoPoints[0], 2 * SizeOf(TESSreal), 2);
  tessTesselate(Tess, TESS_WINDING_ODD, TESS_POLYGONS, 3, 2, nil);
  ElementCount := tessGetElementCount(Tess);
  Writeln('  Result: ', ElementCount, ' triangles');
  Check(ElementCount = 0, 'Two points produce 0 triangles');
  tessDeleteTess(Tess);
end;

procedure TestMultipleSeparatePolygons;
const
  // Two non-overlapping squares
  Square1: array[0..7] of TESSreal = (
    0.0, 0.0,
    1.0, 0.0,
    1.0, 1.0,
    0.0, 1.0
  );
  Square2: array[0..7] of TESSreal = (
    5.0, 5.0,
    6.0, 5.0,
    6.0, 6.0,
    5.0, 6.0
  );
var
  Tess: PTESStesselator;
  VertexCount, ElementCount: Integer;
begin
  Writeln;
  Writeln('Test 12: Multiple separate polygons');
  Writeln('  Input: Two non-overlapping unit squares');
  Writeln('  Expected: 4 triangles (2 per square), 8 vertices');

  Tess := tessNewTess(nil);
  Check(Tess <> nil, 'Tesselator created');
  if Tess = nil then Exit;

  tessAddContour(Tess, 2, @Square1[0], 2 * SizeOf(TESSreal), 4);
  tessAddContour(Tess, 2, @Square2[0], 2 * SizeOf(TESSreal), 4);

  Check(
    tessTesselate(Tess, TESS_WINDING_ODD, TESS_POLYGONS, 3, 2, nil),
    'Tesselation succeeded'
  );

  VertexCount := tessGetVertexCount(Tess);
  ElementCount := tessGetElementCount(Tess);

  Writeln('  Result: ', ElementCount, ' triangles, ', VertexCount, ' vertices');

  Check(VertexCount = 8, 'Vertex count = 8 (4 + 4)');
  Check(ElementCount = 4, 'Triangle count = 4 (2 + 2)');

  tessDeleteTess(Tess);
end;

begin
  Writeln('===========================================');
  Writeln('libtess2 Pascal Port - Simple Unit Tests');
  Writeln('===========================================');

  TestSquareTriangulation;
  TestTriangleTriangulation;
  TestOverlappingTriangles;
  TestWindingRules;
  TestPolygonWithHole;
  TestConcavePolygon;
  TestBoundaryContours;
  TestQuadOutput;
  TestVertexIndices;
  TestConstrainedDelaunay;
  TestDegenerateCases;
  TestMultipleSeparatePolygons;

  Writeln;
  Writeln('===========================================');
  Writeln('Results: ', TestsPassed, ' passed, ', TestsFailed, ' failed');
  Writeln('===========================================');

  if TestsFailed > 0 then
    Halt(1);
end.
