#version 120
varying vec3 position;
varying vec3 normal;
varying vec3 eye;
varying vec3 refl;

uniform mat4 viewMatrix;
uniform mat4 invViewMatrix;

void main()
{  
   vec4 eyeVertex = gl_ModelViewMatrix * gl_Vertex;
   position = eyeVertex.xyz;
   eye = normalize(-position);
   
   vec3 worldVertex = (invViewMatrix * eyeVertex).xyz;
   vec3 worldCamPos = (invViewMatrix[3]).xyz; // translation part of the matrix
   vec3 worldView = normalize(worldVertex - worldCamPos); // vector from camera to vertex
   
   normal = normalize(gl_NormalMatrix * gl_Normal);
   vec3 worldNormal = normalize(transpose(mat3(viewMatrix)) * gl_Normal);
   
   refl = reflect(worldView, worldNormal);
   refl = vec3(-refl.x, refl.y, -refl.z);
   
   gl_Position = ftransform();
}