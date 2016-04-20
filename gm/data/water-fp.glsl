#version 120
varying vec3 position;
varying vec3 normal;
varying vec3 eye;
varying vec3 refl;

uniform samplerCube texture0;
const vec4 waterColor = vec4(0.4, 0.4, 0.2, 1.0);
    
void main()
{
   vec3 L = normalize(gl_LightSource[0].position.xyz - position);                    
   vec3 N = normalize(normal);
   vec3 E = normalize(eye);
   vec3 R = normalize(refl);
   vec3 H = normalize(L + E);
   
   float fresnel = 1.0 - max(dot(E, N), 0.0);

   vec4 tex = textureCube(texture0, R); 
   vec4 reflectionTerm = tex * fresnel + waterColor * (1.0 - fresnel);

   float diffuse = clamp(dot(N, L), 0.0, 1.0);
   float specular = pow(max(dot(H, N), 0.0), 3.0 * gl_FrontMaterial.shininess);
   
    vec4 col =
     reflectionTerm * 0.6 +
     diffuse * reflectionTerm +
     specular * gl_FrontMaterial.specular;
     
    col.a = max(0.8, fresnel);

    gl_FragColor = col;
}