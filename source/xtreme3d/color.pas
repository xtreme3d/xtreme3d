function MakeColorRGB(r, g, b: real): real; cdecl;
var
  rb, gb, bb: Byte;
begin
  rb := trunc(r);
  gb := trunc(g);
  bb := trunc(b);
  result := Integer(RGB(rb, gb, bb));
end;

function MakeColorRGBFloat(r, g, b: real): real; cdecl;
var
  rb, gb, bb: Byte;
begin
  rb := trunc(r * 255.0);
  gb := trunc(g * 255.0);
  bb := trunc(b * 255.0);
  result := Integer(RGB(rb, gb, bb));
end;
