#version 330 core

layout (points) in;
layout (line_strip, max_vertices = 6) out;

in Vertex {
    vec3 x_axis;
    vec3 y_axis;
    vec3 z_axis;
} vertex[];

out vec4 color;

uniform mat4 projection_matrix;

uniform float vector_length;

void main() {
    gl_Position = projection_matrix * gl_in[0].gl_Position;
    color = vec4(1.0, 0.0, 0.0, 1.0);
    EmitVertex();
    gl_Position = projection_matrix * (gl_in[0].gl_Position + vec4(vector_length * vertex[0].x_axis, 0.0));
    color = vec4(1.0, 0.0, 0.0, 1.0);
    EmitVertex();
    EndPrimitive();

    gl_Position = projection_matrix * gl_in[0].gl_Position;
    color = vec4(0.0, 0.0, 1.0, 1.0);
    EmitVertex();
    gl_Position = projection_matrix * (gl_in[0].gl_Position + vec4(vector_length * vertex[0].y_axis, 0.0));
    color = vec4(0.0, 0.0, 1.0, 1.0);
    EmitVertex();
    EndPrimitive();

    gl_Position = projection_matrix * gl_in[0].gl_Position;
    color = vec4(0.0, 1.0, 0.0, 1.0);
    EmitVertex();
    gl_Position = projection_matrix * (gl_in[0].gl_Position + vec4(vector_length * vertex[0].z_axis, 0.0));
    color = vec4(0.0, 1.0, 0.0, 1.0);
    EmitVertex();
    EndPrimitive();
}