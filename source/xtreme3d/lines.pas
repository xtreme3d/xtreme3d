function LinesCreate(parent: real): real; cdecl;
var
  li: TGLLines;
begin
  if not (parent=0) then
    li := TGLLines.CreateAsChild(TGLBaseSceneObject(RealToPtr(parent)))
  else
    li := TGLLines.CreateAsChild(scene.Objects);
  result := Integer(li);
end;

function LinesAddNode(lines, x, y, z: real): real; cdecl;
var
  li: TGLLines;
begin
  li := TGLLines(RealToPtr(lines));
  li.AddNode(x, y, z);
  result := 1.0;
end;

function LinesDeleteNode(lines, ind: real): real; cdecl;
var
  li: TGLLines;
begin
  li := TGLLines(RealToPtr(lines));
  li.Nodes.Delete(trunc(ind));
  result := 1.0;
end;

function LinesSetNode(lines, ind, x, y, z: real): real; cdecl;
var
  li: TGLLines;
begin
  li := TGLLines(RealToPtr(lines));
  li.Nodes[trunc(ind)].X := x;
  li.Nodes[trunc(ind)].Y := y;
  li.Nodes[trunc(ind)].Y := z;
  result := 1.0;
end;

function LinesSetColors(lines,
  linecolor, linealpha,
  nodecolor, nodealpha : real): real; cdecl;
var
  li: TGLLines;
begin
  li := TGLLines(RealToPtr(lines));
  li.LineColor.AsWinColor := TColor(trunc(linecolor));
  li.LineColor.Alpha := linealpha;
  li.NodeColor.AsWinColor := TColor(trunc(nodecolor));
  li.NodeColor.Alpha := nodealpha;
  result := 1.0;
end;

function LinesSetSize(lines, linewidth, nodesize: real): real; cdecl;
var
  li: TGLLines;
begin
  li := TGLLines(RealToPtr(lines));
  li.LineWidth := linewidth;
  li.NodeSize := nodesize;
  result := 1.0;
end;

function LinesSetSplineMode(lines, lsm: real): real; cdecl;
var
  li: TGLLines;
begin
  li := TGLLines(RealToPtr(lines));
  if lsm = 0 then li.SplineMode := lsmLines;
  if lsm = 1 then li.SplineMode := lsmCubicSpline;
  if lsm = 2 then li.SplineMode := lsmBezierSpline;
  if lsm = 3 then li.SplineMode := lsmNURBSCurve;
  if lsm = 4 then li.SplineMode := lsmSegments;
  result := 1.0;
end;

function LinesSetNodesAspect(lines, lna: real): real; cdecl;
var
  li: TGLLines;
begin
  li := TGLLines(RealToPtr(lines));
  if lna = 0 then li.NodesAspect := lnaInvisible;
  if lna = 1 then li.NodesAspect := lnaAxes;
  if lna = 2 then li.NodesAspect := lnaCube;
  result := 1.0;
end;

function LinesSetDivision(lines, division: real): real; cdecl;
var
  li: TGLLines;
begin
  li := TGLLines(RealToPtr(lines));
  li.Division := trunc(division);
  result := 1.0;
end;

