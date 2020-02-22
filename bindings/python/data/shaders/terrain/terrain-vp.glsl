varying vec3 position;
varying vec3 positionWorld;

uniform mat4 invViewMatrix;

void main()
{
    gl_TexCoord[0] = gl_MultiTexCoord0;
    vec4 eyePos = gl_ModelViewMatrix * gl_Vertex;
    position = eyePos.xyz;
    positionWorld = invViewMatrix * eyePos;
    gl_Position = ftransform();
}
