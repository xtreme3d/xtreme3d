function PipeCreate(divs,slic, parent: real): real; stdcall;
var
  pipe: TGLPipe;
begin
    if not (parent=0) then
    pipe:=TGLPipe.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
  else
    pipe:=TGLPipe.CreateAsChild(scene.Objects);
	
	pipe.Division:=trunc64(divs);
	pipe.Slices:=trunc64(slic);
	//pipe.NodesColorMode:=pncmAmbientAndDiffuse;
  result:=Integer(pipe);
end;

function PipeAddNode(pipe, x,y,z: real): real; stdcall;
var
  pipeObj: TGLPipe;
begin
  pipeObj:=TGLPipe(trunc64(pipe));
  pipeObj.AddNode(x,y,z);
  result := 1;
end;

function PipeSetDivision(pipe, divs: real): real; stdcall;
var
  pipeObj: TGLPipe;
begin
  pipeObj:=TGLPipe(trunc64(pipe));
  pipeObj.Division:=trunc64(divs);
  result := 1;
end;

function PipeSetSplineMode(pipe, mode: real): real; stdcall;
var
  pipeObj: TGLPipe;
begin
  pipeObj:=TGLPipe(trunc64(pipe));
  if mode = 0 then pipeObj.SplineMode := lsmLines;
  if mode = 1 then pipeObj.SplineMode := lsmCubicSpline;
  if mode = 2 then pipeObj.SplineMode := lsmBezierSpline;
  if mode = 3 then pipeObj.SplineMode := lsmNURBSCurve;
  if mode = 4 then pipeObj.SplineMode := lsmSegments;
  result := 1;
end;

function PipeDeleteNode(pipe, ind: real): real; stdcall;
var
  pipeObj: TGLPipe;
begin
  pipeObj:=TGLPipe(trunc64(pipe));
  pipeObj.Nodes.Delete(trunc64(ind));
  result := 1;
end;

function PipeSetRadius(pipe, rad: real): real; stdcall;
var
  pipeObj: TGLPipe;
begin
  pipeObj:=TGLPipe(trunc64(pipe));
  pipeObj.Radius:=rad;
  result := 1;
end;

function PipeSetNode(pipe,ind, x,y,z: real): real; stdcall;
var
  pipeObj: TGLPipe;
begin
  pipeObj:=TGLPipe(trunc64(pipe));
  pipeObj.Nodes[trunc64(ind)].X:=x;
  pipeObj.Nodes[trunc64(ind)].Y:=y;
  pipeObj.Nodes[trunc64(ind)].Z:=z;
  result := 1;
end;

function PipeSetSlices(pipe,slic: real): real; stdcall;
var
  pipeObj: TGLPipe;
begin
  pipeObj:=TGLPipe(trunc64(pipe));
  pipeObj.Slices:=trunc64(slic);
  result := 1;
end;
