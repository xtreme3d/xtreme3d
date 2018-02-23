varying vec3 position;
varying vec3 n, e;

void main()
{
    gl_TexCoord[0] = gl_MultiTexCoord0;
    n = normalize(gl_NormalMatrix * gl_Normal);
    position = (gl_ModelViewMatrix * gl_Vertex).xyz;
    e = -normalize(position);  
    gl_Position = ftransform();
}
