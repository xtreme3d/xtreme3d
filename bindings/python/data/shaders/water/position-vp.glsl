varying vec3 eyePos;

void main()
{
    vec4 eyeVertex = gl_ModelViewMatrix * gl_Vertex;
    eyePos = eyeVertex.xyz; 
	gl_Position = ftransform();	
}
