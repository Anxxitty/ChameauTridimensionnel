#version 330 core

struct Point_Light {
    vec3 position;
    vec3 ambient;
    vec3 diffuse;
    vec3 specular;
    float linear_attenuation;
    float quadratic_attenuation;
    float intensity;
};

struct Directional_Light {
    vec3 direction;
    vec3 ambient;
    vec3 diffuse;
    vec3 specular;
    float intensity;
};

struct Spot_Light {
    vec3 position;
    vec3 direction;
    vec3 ambient;
    vec3 diffuse;
    vec3 specular;
    float outer_cut_off_angle;
    float inner_cut_off_angle;
    float linear_attenuation;
    float quadratic_attenuation;
    float intensity;
};

struct Material {
    sampler2D ambient;
    sampler2D diffuse;
    sampler2D specular;
    float shininess;
};

uniform Material material;

in Vertex {
    vec2 texture_coordinates;
    vec3 normal;
    vec3 fragment_position;
} vertex;

uniform int number_of_point_lights;
uniform Point_Light point_lights[ENGINE_SETTINGS_MAX_NUMBER_OF_LIGHTS];
uniform int number_of_directional_lights;
uniform Directional_Light directional_lights[ENGINE_SETTINGS_MAX_NUMBER_OF_LIGHTS];
uniform int number_of_spot_lights;
uniform Spot_Light spot_lights[ENGINE_SETTINGS_MAX_NUMBER_OF_LIGHTS];

out vec4 fragment_color;

vec3 ambient(vec3 light_ambient) {
    return light_ambient * vec3(texture(material.ambient, vertex.texture_coordinates));
}

vec3 diffuse(vec3 light_direction, vec3 light_diffuse) {
    return vec3(texture(material.diffuse, vertex.texture_coordinates)) * light_diffuse * max(0.0, dot(light_direction, vertex.normal));
}

vec3 specular(vec3 light_direction, vec3 light_specular) {
    vec3 camera_direction = normalize(-vertex.fragment_position);
    vec3 halfway_vector = normalize(light_direction + camera_direction);
    return vec3(texture(material.specular, vertex.texture_coordinates)) * light_specular * pow(max(0.0, dot(halfway_vector, vertex.normal)), float(material.shininess));
}

void main() {
    vec3 result = vec3(0.0, 0.0, 0.0);
    for (int i = 0; i < number_of_directional_lights; i++) {
        vec3 light_direction = normalize(-directional_lights[i].direction);
        result += directional_lights[i].intensity * (
            ambient(directional_lights[i].ambient)
          + diffuse(light_direction, directional_lights[i].diffuse) 
          + specular(light_direction, directional_lights[i].specular)
        );
    }
    for (int i = 0; i < number_of_point_lights; i++) {
        vec3 light_fragment = point_lights[i].position - vertex.fragment_position;
        vec3 light_direction = normalize(light_fragment);
        float light_fragment_distance = length(light_fragment);
        float attenuation = 1.0/(1.0 + point_lights[i].linear_attenuation*light_fragment_distance + point_lights[i].quadratic_attenuation*light_fragment_distance*light_fragment_distance);
        result += attenuation * point_lights[i].intensity * (
            ambient(point_lights[i].ambient)
          + diffuse(light_direction, point_lights[i].diffuse) 
          + specular(light_direction, point_lights[i].specular)
        );
    }
    for (int i = 0; i < number_of_spot_lights; i++) {
        vec3 light_fragment = spot_lights[i].position - vertex.fragment_position;
        float light_fragment_distance = length(light_fragment);
        vec3 light_direction = light_fragment/light_fragment_distance;
        float light_fragment_angle = dot(light_direction, -spot_lights[i].direction);
        if (light_fragment_angle > spot_lights[i].outer_cut_off_angle) {
            float attenuation = 1.0/(1.0 + spot_lights[i].linear_attenuation*light_fragment_distance + spot_lights[i].quadratic_attenuation*light_fragment_distance*light_fragment_distance);
            float border_attenuation = min(1.0, (light_fragment_angle - spot_lights[i].outer_cut_off_angle)/(spot_lights[i].inner_cut_off_angle - spot_lights[i].outer_cut_off_angle));
            result += attenuation * border_attenuation * spot_lights[i].intensity * (
                ambient(spot_lights[i].ambient) 
              + diffuse(light_direction, spot_lights[i].diffuse) 
              + specular(light_direction, spot_lights[i].specular)
            );
        }
    }
    fragment_color = vec4(result, texture(material.diffuse, vertex.texture_coordinates).a);
}