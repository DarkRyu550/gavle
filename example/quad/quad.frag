#version 300 es
precision mediump float;

uniform sampler2D goat;

in vec4 pos;
in vec2 uv;

layout(location = 0) out vec4 color;

float near = 0.1;
float far  = 100.0;

float LinearizeDepth(float depth)
{
    float z = depth * 2.0 - 1.0; // back to NDC
    return (2.0 * near * far) / (far + near - z * (far - near));
}


void main()
{
    color = texture(goat, uv);
    color = vec4((1.0 - clamp(LinearizeDepth(pos.z), 0.0, 1.0)) * color.xyz, 1.0);
}
