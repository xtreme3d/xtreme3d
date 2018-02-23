function MakeColorRGB(r, g, b: real): real; stdcall;
var
  rb, gb, bb: Byte;
begin
  rb := trunc64(r);
  gb := trunc64(g);
  bb := trunc64(b);
  result := Integer(RGB(rb, gb, bb));
end;

function MakeColorRGBFloat(r, g, b: real): real; stdcall;
var
  rb, gb, bb: Byte;
begin
  rb := trunc64(r * 255.0);
  gb := trunc64(g * 255.0);
  bb := trunc64(b * 255.0);
  result := Integer(RGB(rb, gb, bb));
end;
