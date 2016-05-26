varying vec4 shadowCoord;
varying vec3 normal;

uniform sampler2D diffuseMap;
uniform sampler2DShadow shadowMap;

float lookup(sampler2DShadow depths, vec4 coord, vec2 offset)
{
    vec2 texelSize = vec2(1.0) / vec2(1024.0, 1024.0); //dgl_ShadowMapSize
    vec2 v = offset * texelSize * coord.w;
    //coord.z *= 1.001;
    float z = shadow2DProj(depths, coord + vec4(v.x, v.y, 0, 0.0), 2.0).z;
    return z;
}

void main()
{    
    float shadow = 1.0;
    
    vec3 directionToLight = normalize(gl_LightSource[0].position.xyz);
    vec3 N = normalize(normal);
    float diffuse = clamp(dot(N, directionToLight), 0.0, 1.0);
    
    vec4 diffuseCol = texture2D(diffuseMap, gl_TexCoord[0].st);

    if (shadowCoord.w > 0.0)
    {
        shadow = 0.0;
        float x, y;
        const float size = 2.0;
		for (y = -size ; y < size ; y += 1.0)
		for (x = -size ; x < size ; x += 1.0)
        {
			shadow += lookup(shadowMap, shadowCoord, vec2(x, y));
        }
			
		shadow /= size * size * 4.0;
    }

    gl_FragColor = diffuseCol * 0.3 + diffuseCol * diffuse * shadow;
    gl_FragColor.a = 1.0;
}
