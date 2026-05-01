#version 330 core
layout (location = 0) in vec4 pos;
out vec4 color;
uniform mat4 model_matrix;
uniform mat4 view_matrix;
uniform mat4 projection_matrix;
void main() {
    gl_Position = projection_matrix *  view_matrix * model_matrix * pos;
    color = vec4(0.0,0.5,0.1,1.0);
}