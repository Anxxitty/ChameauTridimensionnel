#version 330 core
layout (location = 0) in vec3 pos;
layout (location = 1) in vec4 col;
out vec4 color;
uniform mat4 model_matrix;
uniform mat4 view_matrix;
uniform mat4 projection_matrix;
void main() {
    gl_Position = projection_matrix *  view_matrix * model_matrix * vec4(pos, 1.0);
    color = col;
}