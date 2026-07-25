#version 330 core

layout (triangles) in;
layout (line_strip, max_vertices = 6) out;

in Vertex {
    vec3 normal;
} vertex[];

uniform mat4 projection_matrix;

uniform float normal_length;

void main() {
    gl_Position = projection_matrix * gl_in[0].gl_Position;
    EmitVertex();
    gl_Position = projection_matrix * (gl_in[0].gl_Position + vec4(normal_length * vertex[0].normal, 0.0));
    EmitVertex();
    EndPrimitive();

    gl_Position = projection_matrix * gl_in[1].gl_Position;
    EmitVertex();
    gl_Position = projection_matrix * (gl_in[1].gl_Position + vec4(normal_length * vertex[1].normal, 0.0));
    EmitVertex();
    EndPrimitive();

    gl_Position = projection_matrix * gl_in[2].gl_Position;
    EmitVertex();
    gl_Position = projection_matrix * (gl_in[2].gl_Position + vec4(normal_length * vertex[2].normal, 0.0));
    EmitVertex();
    EndPrimitive();
}