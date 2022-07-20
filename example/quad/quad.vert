#version 300 es
precision mediump float;

layout(std140) uniform matrices
{
    mat4 modelview;
    mat4 projection;
};

in vec4 position;
in vec2 texture_uv;

out vec4 pos;
out vec2 uv;

void main() {
    pos = projection * modelview * position;

    uv = texture_uv;
    gl_Position = pos;
}