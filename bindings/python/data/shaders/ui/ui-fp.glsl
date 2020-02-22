uniform sampler2D screenTexture;

void main()
{
    gl_FragColor = texture2D(screenTexture, gl_TexCoord[0].st);
}
