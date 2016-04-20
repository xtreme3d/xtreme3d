varying vec3 position;
varying vec3 n, t, b, e;

void main()
{
    gl_TexCoord[0] = gl_MultiTexCoord0;
    
    n = normalize(gl_NormalMatrix * gl_Normal);
    t = normalize(gl_NormalMatrix * gl_MultiTexCoord1.xyz);
    b = cross(n, t);

    position = (gl_ModelViewMatrix * gl_Vertex).xyz;
    
    e = position;
    e.x = dot(position, t);
    e.y = dot(position, b);
    e.z = dot(position, n);
    e = -e;
        
    gl_Position = ftransform();
}
