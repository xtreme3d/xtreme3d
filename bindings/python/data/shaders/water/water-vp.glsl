varying vec3 position;
varying vec3 worldPos;
varying vec3 normal;
varying vec3 tangent;
varying vec3 binormal;
varying vec3 eye;
varying vec3 eyeTan;

varying vec3 tNormal;

uniform mat4 invViewMatrix;

void main()
{
    vec4 eyeVertex = gl_ModelViewMatrix * gl_Vertex;
    position = eyeVertex.xyz;
    eye = normalize(-position);
    
    mat4 modelMatrix = invViewMatrix * gl_ModelViewMatrix;
    worldPos = vec3(modelMatrix * gl_Vertex);
    
    normal = normalize(gl_NormalMatrix * gl_Normal);
    tangent = normalize(gl_NormalMatrix * vec3(1, 0, 0));
    binormal = cross(normal, tangent);
    
    eyeTan = position;
    eyeTan.x = dot(position, tangent);
    eyeTan.y = dot(position, binormal);
    eyeTan.z = dot(position, normal);
    eyeTan = -eyeTan;
 
	gl_Position = gl_ProjectionMatrix * eyeVertex;
    gl_TexCoord[0] = gl_MultiTexCoord0;
}

