varying vec3 position;
varying vec3 n, e;

varying vec4 shadowCoord1;
varying vec4 shadowCoord2;
varying vec4 shadowCoord3;

uniform mat4 shadowMatrix1;
uniform mat4 shadowMatrix2;
uniform mat4 shadowMatrix3;

void main()
{
    gl_TexCoord[0] = gl_MultiTexCoord0;
    vec4 eyeVertex = gl_ModelViewMatrix * gl_Vertex;
    shadowCoord1 = shadowMatrix1 * eyeVertex;
    shadowCoord2 = shadowMatrix2 * eyeVertex;
    shadowCoord3 = shadowMatrix3 * eyeVertex;
    position = eyeVertex.xyz;
    
    e = position;
    e = -normalize(e);
    
    n = normalize(gl_NormalMatrix * gl_Normal);
   
    gl_Position = ftransform();
}