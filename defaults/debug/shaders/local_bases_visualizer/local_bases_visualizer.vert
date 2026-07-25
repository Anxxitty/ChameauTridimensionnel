#version 330 core
layout (location = 0) in vec3 pos;
layout (location = 1) in vec3 x_axis;
layout (location = 2) in vec3 y_axis;
layout (location = 3) in vec3 z_axis;

out Vertex {
    vec3 x_axis;
    vec3 y_axis;
    vec3 z_axis;
} vertex;

uniform mat4 view_matrix;

void main() {
    vertex.x_axis = mat3(view_matrix) * x_axis;
    vertex.y_axis = mat3(view_matrix) * y_axis;
    vertex.z_axis = mat3(view_matrix) * z_axis;
    gl_Position = view_matrix * vec4(pos, 1.0);
}