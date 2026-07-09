#version 330 core
in vec2 tex_coord;
in vec3 norm;
in vec3 frag_pos;
in vec3 o_light_pos;
out vec4 frag_color;

uniform sampler2D texture1;
uniform float time;

uniform vec3 light_col;
uniform float light_intensity;
uniform float ambient;
uniform int specular_expo;
uniform float alpha;

void main() {
    vec3 light_dir = normalize(o_light_pos - frag_pos);
    vec3 reflection_dir = normalize(reflect(-light_dir, norm));
    vec3 cam_obj = normalize(-frag_pos);
    vec4 obj_col = texture(texture1, tex_coord);
    vec3 diffuse = obj_col.xyz * light_col * pow(max(0.0, dot(light_dir, norm)), 1/light_intensity);
    vec3 ambient_light = obj_col.xyz * ambient * light_col;
    vec3 specular = obj_col.xyz * light_col * pow(max(0.0, dot(reflection_dir, cam_obj)), float(specular_expo));
    frag_color = vec4(diffuse + ambient_light + specular, alpha);
}