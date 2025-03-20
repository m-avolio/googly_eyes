#version 300 es
precision highp float;

uniform vec3 u_cameraPos;
uniform float u_focalLength;
uniform vec3 u_lightPos;
uniform vec2 u_resolution;
uniform float u_lightRadiance;
uniform float u_alpha;

// New texture uniform for the light.
uniform sampler2D u_lightTexture;

out vec4 fragColor;

const float PI = 3.14159265359;
const float epsilon = 1e-6;

// A simple random function.
float random(vec2 st) {
    return fract(sin(dot(st.xy, vec2(12.9898,78.233)))*43758.5453123);
}

struct Camera {
    vec2 sensorSize;  // in millimeters
    float focalLength;
    vec3 position;
};

struct Light {
    vec2 size;        // in millimeters
    vec3 position;
    vec3 normal;
};

mat4 lightTransform(Light light) {
    vec3 zBasis = light.normal; // assumed normalized
    vec3 xBasis = normalize(cross(vec3(0,1,0), zBasis));
    if(length(xBasis) < epsilon) {
        xBasis = vec3(1,0,0);
    }
    vec3 yBasis = cross(zBasis, xBasis);
    xBasis *= light.size.x;
    yBasis *= light.size.y;
    return mat4(
        vec4(xBasis, 0.0),
        vec4(yBasis, 0.0),
        vec4(0.0, 0.0, 0.0, 0.0),
        vec4(light.position, 1.0)
    );
}

mat4 viewMatrix(Camera camera) {
    vec3 zBasis = normalize(camera.position);
    vec3 xBasis = normalize(cross(vec3(0,1,0), zBasis));
    if (xBasis == vec3(0, 0, 0)) {
        xBasis = vec3(1, 0, 0);
    }
    vec3 yBasis = cross(zBasis, xBasis);
    return mat4(
        vec4(xBasis, 0.0),
        vec4(yBasis, 0.0),
        vec4(zBasis, 0.0),
        vec4(camera.position, 1.0)
    );
}

// Helper: Ray-plane intersection.
float intersectRayPlane(vec3 rayOrigin, vec3 rayDir, vec3 planePoint, vec3 planeNormal) {
    float denom = dot(planeNormal, rayDir);
    if(abs(denom) < epsilon) return -1.0;
    return dot(planePoint - rayOrigin, planeNormal) / denom;
}

vec3 fresnelSchlick(float cosTheta, vec3 F0) {
    return F0 + (1.0 - F0) * pow(1.0 - cosTheta, 5.0);
}

float intersectCylinder(vec3 rayOrigin, vec3 rayDir, float R, float cylinderHeight) {
    float a = rayDir.x * rayDir.x + rayDir.z * rayDir.z;
    float b = 2.0 * (rayOrigin.x * rayDir.x + rayOrigin.z * rayDir.z);
    float c = rayOrigin.x * rayOrigin.x + rayOrigin.z * rayOrigin.z - R * R;
    float disc = b * b - 4.0 * a * c;
    if(disc < 0.0) return -1.0;
    float sqrtDisc = sqrt(disc);
    float t1 = (-b - sqrtDisc) / (2.0 * a);
    float t2 = (-b + sqrtDisc) / (2.0 * a);
    float tCylinder = -1.0;
    if(t1 > epsilon) {
        float y = rayOrigin.y + t1 * rayDir.y;
        if(y >= 0.0 && y <= cylinderHeight)
            tCylinder = t1;
    }
    if(tCylinder < 0.0 && t2 > epsilon) {
        float y = rayOrigin.y + t2 * rayDir.y;
        if(y >= 0.0 && y <= cylinderHeight)
            tCylinder = t2;
    }
    return tCylinder;
}

float intersectCap(vec3 rayOrigin, vec3 rayDir, float R, float cylinderHeight, float flatten) {
    vec3 center = vec3(0.0, cylinderHeight, 0.0);
    vec3 p = rayOrigin - center;
    vec3 pPrime = vec3(p.x, p.y/flatten, p.z);
    vec3 dPrime = vec3(rayDir.x, rayDir.y/flatten, rayDir.z);
    float A = dot(dPrime, dPrime);
    float B = 2.0 * dot(pPrime, dPrime);
    float C = dot(pPrime, pPrime) - R * R;
    float disc = B * B - 4.0 * A * C;
    if(disc < 0.0) return -1.0;
    float sqrtDisc = sqrt(disc);
    float t1 = (-B - sqrtDisc) / (2.0 * A);
    float t2 = (-B + sqrtDisc) / (2.0 * A);
    float tCap = -1.0;
    if(t1 > epsilon) {
        vec3 pos = rayOrigin + t1 * rayDir;
        if(pos.y >= cylinderHeight)
            tCap = t1;
    }
    if(tCap < 0.0 && t2 > epsilon) {
        vec3 pos = rayOrigin + t2 * rayDir;
        if(pos.y >= cylinderHeight)
            tCap = t2;
    }
    return tCap;
}

float lambertianReflection() {
    return 1.0/PI;
}

//
// A lightweight 3D "Perlin-like" noise implementation,
// using a fade function and corner randoms.
//
// -----------------------------------------------------

// A utility random function for 3D:
float random3(vec3 c) 
{
    // Dot against large, somewhat 'random' constants:
    // (12.9898, 78.233, 37.425) is a classic choice.
    return fract(sin(dot(c, vec3(12.9898, 78.233, 37.425))) * 43758.5453123);
}

// Smooth interpolation curve for Perlin noise.
float fade(float t) 
{
    // 6t^5 - 15t^4 + 10t^3
    return t * t * t * (t * (t * 6.0 - 15.0) + 10.0);
}

// A simple 3D "Perlin-like" noise function:
float perlinNoise(vec3 p) 
{
    // "Grid cell" coordinates (integer) and local position (fractional).
    vec3 i = floor(p);
    vec3 f = fract(p);

    // 8 random values at the corners of the cell:
    float n000 = random3(i + vec3(0.0, 0.0, 0.0));
    float n001 = random3(i + vec3(0.0, 0.0, 1.0));
    float n010 = random3(i + vec3(0.0, 1.0, 0.0));
    float n011 = random3(i + vec3(0.0, 1.0, 1.0));
    float n100 = random3(i + vec3(1.0, 0.0, 0.0));
    float n101 = random3(i + vec3(1.0, 0.0, 1.0));
    float n110 = random3(i + vec3(1.0, 1.0, 0.0));
    float n111 = random3(i + vec3(1.0, 1.0, 1.0));

    // Fade the fractional position to smooth the interpolation.
    vec3 u = vec3(fade(f.x), fade(f.y), fade(f.z));

    // Trilinear interpolation of the corner values.
    // Mix in x, then y, then z dimensions.
    float nx00 = mix(n000, n100, u.x);
    float nx01 = mix(n001, n101, u.x);
    float nx10 = mix(n010, n110, u.x);
    float nx11 = mix(n011, n111, u.x);
    float nxy0 = mix(nx00, nx10, u.y);
    float nxy1 = mix(nx01, nx11, u.y);
    return mix(nxy0, nxy1, u.z);
}

vec3 perturb_normal(vec3 normal, vec3 worldPos, float scale, float frequency)
{
    // Scale the intersectionWorld to control how quickly noise varies:
    vec3 pos = worldPos * frequency;

    // Sample 3D noise in each direction:
    float nx = perlinNoise(pos + vec3(0.1, 0.0, 0.0));
    float ny = perlinNoise(pos + vec3(0.0, 0.1, 0.0));
    float nz = perlinNoise(pos + vec3(0.0, 0.0, 0.1));

    // Move each noise sample into [-0.5, +0.5], then scale:
    vec3 perturb = (vec3(nx, ny, nz) - 0.5) * scale;

    // Add to the original normal and re-normalize:
    return normalize(normal + perturb);
}

void main() {
    // Shape parameters.
    float cylinderHeight = 2.0;
    float radius = 10.0;
    float flatten = 0.3;  // Values < 1 produce a more squashed cap.

    vec3 objectF0 = vec3(0.041808, 0.041808, 0.041808);

    // Set up camera and light.
    Camera camera = Camera(vec2(36.0, 24.0), u_focalLength, u_cameraPos);
    Light light = Light(vec2(3000.0, 2000.0), u_lightPos, normalize(vec3(0.0, -1.0, 0.0)));

    mat4 view = viewMatrix(camera);
    vec2 uv = gl_FragCoord.xy / u_resolution.xy;
    vec2 imagePlane = camera.sensorSize * (uv - 0.5);
    vec3 rayCamera = normalize(vec3(imagePlane.x, imagePlane.y, -camera.focalLength));
    vec4 rayWorldHomogeneous = view * vec4(rayCamera, 0.0);
    vec3 rayWorld = normalize(rayWorldHomogeneous.xyz);

    // Ground plane (y = 0).
    vec3 planePointWorld = vec3(0.0, 0.0, 0.0);
    vec3 planeNormalWorld = vec3(0.0, 1.0, 0.0);
    float tPlane = intersectRayPlane(camera.position, rayWorld, planePointWorld, planeNormalWorld);

    // Intersection with the cylinder and the cap.
    float tCyl = intersectCylinder(camera.position, rayWorld, radius, cylinderHeight);
    float tCap = intersectCap(camera.position, rayWorld, radius, cylinderHeight, flatten);

    // Determine the object's hit.
    float tObj = -1.0;
    int hitType = -1; // 1: cylinder; 2: cap; 0: ground.
    if(tCyl > epsilon && tCap > epsilon) {
        if(tCyl < tCap) { tObj = tCyl; hitType = 1; }
        else { tObj = tCap; hitType = 2; }
    } else if(tCyl > epsilon) {
        tObj = tCyl; hitType = 1;
    } else if(tCap > epsilon) {
        tObj = tCap; hitType = 2;
    }

    // Compare with the ground plane.
    float tHit = -1.0;
    if(tPlane > epsilon && (tObj < epsilon || tPlane < tObj)) {
        tHit = tPlane;
        hitType = 0;
        fragColor = vec4(0.5, 0.5, 0.5, 1.0);
        return;
    } else {
        tHit = tObj;
    }

    if(tHit < 0.0) {
        fragColor = vec4(0.5, 0.5, 0.5, 1.0);
        return;
    }

    vec3 intersectionWorld = camera.position + tHit * rayWorld;
    vec3 hitNormal;
    if(hitType == 1) {
        // Cylinder side: normal is horizontal.
        hitNormal = normalize(vec3(intersectionWorld.x, 0.0, intersectionWorld.z));
    } else if(hitType == 2) {
        // Cap: blend the ellipsoidal cap normal and the cylinder normal.
        vec3 local = intersectionWorld - vec3(0.0, cylinderHeight, 0.0);
        vec3 capNormal = normalize(vec3(
            local.x / (radius * radius),
            local.y / ((radius * flatten) * (radius * flatten)),
            local.z / (radius * radius)
        ));
        vec3 cylNormal = normalize(vec3(intersectionWorld.x, 0.0, intersectionWorld.z));
        float blend = smoothstep(cylinderHeight, cylinderHeight + radius/10.0, intersectionWorld.y);
        hitNormal = normalize(mix(cylNormal, capNormal, blend));
    } else {
        // Ground plane.
        hitNormal = planeNormalWorld;
    }
    // Example: perturb the normal by 5% (tweak to taste)
    hitNormal = perturb_normal(hitNormal, intersectionWorld, 0.1, 0.2);

    // --- Direct Reflection Approach with Texture Projection ---
    // Compute the perfect reflection direction.
    vec3 reflectionDir = reflect(rayWorld, hitNormal);

    // Intersect the reflection ray with the light's plane.
    float tLight = intersectRayPlane(intersectionWorld, reflectionDir, u_lightPos, light.normal);
    vec3 color = vec3(0.0);
    if(tLight > epsilon) {
        vec3 lightIntersection = intersectionWorld + tLight * reflectionDir;
        
        // Compute the light's local coordinate system.
        vec3 zBasis = light.normal;
        vec3 xBasis = normalize(cross(vec3(0.0, 1.0, 0.0), zBasis));
        if(length(xBasis) < epsilon) {
            xBasis = vec3(1.0, 0.0, 0.0);
        }
        vec3 yBasis = cross(zBasis, xBasis);
        
        // Check if the intersection is within the rectangular light area.
        float halfWidth = light.size.x * 0.5;
        float halfHeight = light.size.y * 0.5;
        vec3 v = lightIntersection - u_lightPos;
        float localX = dot(v, normalize(xBasis));
        float localY = dot(v, normalize(yBasis));
        
        if(abs(localX) <= halfWidth && abs(localY) <= halfHeight) {
            // Convert local coordinates to UVs in [0,1].
            vec2 uvLight = vec2(localX/halfWidth, localY/halfHeight);
            uvLight = uvLight * 0.5 + 0.5;
            uvLight.x = 1.0 - uvLight.x;
            vec3 textureColor = texture(u_lightTexture, uvLight).rgb;

            // Compute luminance (brightness) using a standard luminance formula.
            float lum = dot(textureColor, vec3(0.299, 0.587, 0.114));

            // Apply gamma correction to just the luminance.
            float correctedLum = pow(lum, 1.0 / 0.1);

            // Compute a ratio (avoiding division by zero).
            float ratio = (lum > 0.0) ? correctedLum / lum : 1.0;

            // Scale the original color by this ratio so that only brightness is affected.
            textureColor *= ratio;            
            // Compute lighting contribution.
            float distance = length(lightIntersection - intersectionWorld) / 1000.0;
            float attenuation = 1.0 / (distance * distance);
            float NdotR = max(dot(hitNormal, reflectionDir), 0.0);
            float cosTheta = max(dot(-rayWorld, hitNormal), 0.0);
            vec3 F = fresnelSchlick(cosTheta, objectF0);
            
            color = F * u_lightRadiance * attenuation * NdotR * textureColor;
        }
    }
    fragColor = vec4(color, 1.0);
}