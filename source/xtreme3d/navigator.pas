function NavigatorCreate: real; cdecl;
var
  nav: TGLNavigator;
begin
  nav := TGLNavigator.Create(scene);
  Result := Integer(nav);
end;

function NavigatorSetObject(navigator, obj: real): real; cdecl;
var
  nav: TGLNavigator;
  ob: TGLBaseSceneObject;
begin
  nav := TGLNavigator(trunc64(navigator));
  ob := TGLBaseSceneObject(trunc64(obj));
  nav.SetObject(ob);
  Result := 1.0;
end;

function NavigatorSetUseVirtualUp(navigator, mode: real): real; cdecl;
var
  nav: TGLNavigator;
begin
  nav := TGLNavigator(trunc64(navigator));
  nav.SetUseVirtualUp(Boolean(trunc64(mode)));
  Result := 1.0;
end;

function NavigatorSetVirtualUp(navigator, x, y, z: real): real; cdecl;
var
  nav: TGLNavigator;
begin
  nav := TGLNavigator(trunc64(navigator));
  nav.VirtualUp.AsAffineVector := AffineVectorMake(x, y, z);
  Result := 1.0;
end;

function NavigatorTurnHorizontal(navigator, angle: real): real; cdecl;
var
  nav: TGLNavigator;
begin
  nav := TGLNavigator(trunc64(navigator));
  nav.TurnHorizontal(angle);
  Result := 1.0;
end;

function NavigatorTurnVertical(navigator, angle: real): real; cdecl;
var
  nav: TGLNavigator;
begin
  nav := TGLNavigator(trunc64(navigator));
  nav.TurnVertical(angle);
  Result := 1.0;
end;

function NavigatorMoveForward(navigator, spd: real): real; cdecl;
var
  nav: TGLNavigator;
begin
  nav := TGLNavigator(trunc64(navigator));
  nav.MoveForward(spd);
  Result := 1.0;
end;

function NavigatorStrafeHorizontal(navigator, spd: real): real; cdecl;
var
  nav: TGLNavigator;
begin
  nav := TGLNavigator(trunc64(navigator));
  nav.StrafeHorizontal(spd);
  Result := 1.0;
end;

function NavigatorStrafeVertical(navigator, spd: real): real; cdecl;
var
  nav: TGLNavigator;
begin
  nav := TGLNavigator(trunc64(navigator));
  nav.StrafeVertical(spd);
  Result := 1.0;
end;

function NavigatorStraighten(navigator: real): real; cdecl;
var
  nav: TGLNavigator;
begin
  nav := TGLNavigator(trunc64(navigator));
  nav.Straighten;
  Result := 1.0;
end;

function NavigatorFlyForward(navigator, spd: real): real; cdecl;
var
  nav: TGLNavigator;
begin
  nav := TGLNavigator(trunc64(navigator));
  nav.FlyForward(spd);
  Result := 1.0;
end;

function NavigatorMoveUpWhenMovingForward(navigator, mode: real): real; cdecl;
var
  nav: TGLNavigator;
begin
  nav := TGLNavigator(trunc64(navigator));
  nav.MoveUpWhenMovingForward := Boolean(trunc64(mode));
  Result := 1.0;
end;

function NavigatorInvertHorizontalWhenUpsideDown(navigator, mode: real): real; cdecl;
var
  nav: TGLNavigator;
begin
  nav := TGLNavigator(trunc64(navigator));
  nav.InvertHorizontalSteeringWhenUpsideDown := Boolean(trunc64(mode));
  Result := 1.0;
end;

function NavigatorSetAngleLock(navigator, mode: real): real; cdecl;
var
  nav: TGLNavigator;
begin
  nav := TGLNavigator(trunc64(navigator));
  nav.AngleLock := Boolean(trunc64(mode));
  Result := 1.0;
end;

function NavigatorSetAngles(navigator, minangle, maxangle: real): real; cdecl;
var
  nav: TGLNavigator;
begin
  nav := TGLNavigator(trunc64(navigator));
  nav.MinAngle := minangle;
  nav.MaxAngle := maxangle;
  Result := 1.0;
end;
