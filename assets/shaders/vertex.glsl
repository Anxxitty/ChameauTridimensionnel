#version 330 core
layout (location = 0) in vec4 pos;
out vec3 o_color;
uniform mat4 model_matrix;
uniform mat4 view_matrix;
uniform mat4 projection_matrix;
uniform vec3 color;
void main() {
    gl_Position = projection_matrix *  view_matrix * model_matrix * pos;
    o_color = color;
}