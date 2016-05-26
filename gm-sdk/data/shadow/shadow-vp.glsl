varying vec4 shadowCoord;
varying vec3 normal;

uniform mat4 shadowMatrix;

void main()
{
    gl_TexCoord[0] = gl_MultiTexCoord0;
    shadowCoord = shadowMatrix * (gl_ModelViewMatrix * gl_Vertex);    
    normal = normalize(gl_NormalMatrix * gl_Normal);   
    gl_Position = ftransform();
}
