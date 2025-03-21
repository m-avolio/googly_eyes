#version 300 es
precision mediump float;

// ============================================================================
// UNIFORMS
// ============================================================================
uniform vec3  u_cameraPos;
uniform float u_focalLength;
uniform vec3  u_lightPos;
uniform vec2  u_resolution;
uniform float u_lightRadiance;
uniform float u_alpha;
uniform sampler2D u_lightTexture;  // Light source texture

// ============================================================================
// CONSTANTS & DEFINES
// ============================================================================
#define PI 3.14159265359
#define EPSILON 1e-6

// Number of samples for the diffuse "Lambertian" portion
#define SAMPLES 256

// ============================================================================
// STRUCTS
// ============================================================================
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

struct Intersection {
    bool  hit;
    float t;
    vec3  point;
    vec3  normal;
};

// ============================================================================
// RANDOM FUNCTIONS
// ============================================================================
float random(vec2 st) {
    // A simple 2D→float hash
    return fract(sin(dot(st.xy, vec2(12.9898,78.233))) * 43758.5453123);
}

// ============================================================================
// NOISE (for normal perturb if desired)
// ============================================================================
float random3(vec3 c) {
    return fract(sin(dot(c, vec3(12.9898, 78.233, 37.425))) * 43758.5453123);
}
float fade(float t) {
    return t*t*t*(t*(t*6.0-15.0)+10.0);
}
float perlinNoise(vec3 p) {
    vec3 i = floor(p);
    vec3 f = fract(p);

    float n000 = random3(i + vec3(0.0, 0.0, 0.0));
    float n001 = random3(i + vec3(0.0, 0.0, 1.0));
    float n010 = random3(i + vec3(0.0, 1.0, 0.0));
    float n011 = random3(i + vec3(0.0, 1.0, 1.0));
    float n100 = random3(i + vec3(1.0, 0.0, 0.0));
    float n101 = random3(i + vec3(1.0, 0.0, 1.0));
    float n110 = random3(i + vec3(1.0, 1.0, 0.0));
    float n111 = random3(i + vec3(1.0, 1.0, 1.0));

    vec3 u = vec3(fade(f.x), fade(f.y), fade(f.z));

    float nx00 = mix(n000, n100, u.x);
    float nx01 = mix(n001, n101, u.x);
    float nx10 = mix(n010, n110, u.x);
    float nx11 = mix(n011, n111, u.x);
    float nxy0 = mix(nx00, nx10, u.y);
    float nxy1 = mix(nx01, nx11, u.y);
    return mix(nxy0, nxy1, u.z);
}

vec3 perturb_normal(vec3 normal, vec3 worldPos, float scale, float frequency) {
    vec3 pos = worldPos * frequency;

    float nx = perlinNoise(pos + vec3(0.1, 0.0, 0.0));
    float ny = perlinNoise(pos + vec3(0.0, 0.1, 0.0));
    float nz = perlinNoise(pos + vec3(0.0, 0.0, 0.1));

    vec3 perturb = (vec3(nx, ny, nz) - 0.5) * scale;
    return normalize(normal + perturb);
}

// ============================================================================
// MATRIX HELPERS
// ============================================================================
mat4 viewMatrix(Camera camera) {
    // We'll assume the camera is looking towards the origin
    vec3 zBasis = normalize(camera.position);
    vec3 xBasis = normalize(cross(vec3(0,1,0), zBasis));
    if (length(xBasis) < EPSILON) {
        xBasis = vec3(1,0,0);
    }
    vec3 yBasis = cross(zBasis, xBasis);

    return mat4(
        vec4(xBasis, 0.0),
        vec4(yBasis, 0.0),
        vec4(zBasis, 0.0),
        vec4(camera.position, 1.0)
    );
}

// ============================================================================
// INTERSECTIONS
// ============================================================================

// Ray-plane
float intersectRayPlane(vec3 rayOrigin, vec3 rayDir, vec3 planePoint, vec3 planeNormal) {
    float denom = dot(planeNormal, rayDir);
    if(abs(denom) < EPSILON) return -1.0;
    float t = dot(planePoint - rayOrigin, planeNormal) / denom;
    if(t < 0.0) return -1.0;
    return t;
}

// Sclera: disc at y=0
Intersection intersectSclera(vec3 rayOrigin, vec3 rayDir, float R) {
    Intersection result;
    result.hit = false;
    result.t   = -1.0;

    if(abs(rayDir.y) < EPSILON) return result;
    float tPlane = -rayOrigin.y / rayDir.y;
    if(tPlane < EPSILON) return result;

    vec3 pos = rayOrigin + tPlane * rayDir;
    if(dot(pos.xz, pos.xz) > R*R) return result;

    result.hit    = true;
    result.t      = tPlane;
    result.point  = pos;
    result.normal = vec3(0.0, 1.0, 0.0);
    return result;
}

// Pupil: cylinder side + top disc
float intersectSide(vec3 ro, vec3 rd, vec2 cylPos, float R, float h) {
    vec2 o = vec2(ro.x, ro.z) - cylPos;
    float a = rd.x*rd.x + rd.z*rd.z;
    float b = 2.0 * (o.x*rd.x + o.y*rd.z);
    float c = dot(o,o) - R*R;
    float disc = b*b - 4.0*a*c;
    if(disc < 0.0) return -1.0;
    float sd = sqrt(disc);
    float t1 = (-b - sd)/(2.0*a);
    float t2 = (-b + sd)/(2.0*a);
    float tSide = -1.0;
    if(t1 > EPSILON) {
        float y = ro.y + t1*rd.y;
        if(y >= 0.0 && y <= h) tSide = t1;
    }
    if(tSide < 0.0 && t2 > EPSILON) {
        float y = ro.y + t2*rd.y;
        if(y >= 0.0 && y <= h) tSide = t2;
    }
    return tSide;
}
float intersectDisc(vec3 ro, vec3 rd, vec2 cylPos, float R, float h) {
    float denom = dot(rd, vec3(0,1,0));
    if(abs(denom) < EPSILON) return -1.0;
    float t = (h - ro.y)/rd.y;
    if(t < EPSILON) return -1.0;
    vec3 pos = ro + t*rd;
    vec2 d = vec2(pos.x, pos.z) - cylPos;
    if(dot(d,d) <= R*R) return t;
    return -1.0;
}
Intersection intersectPupil(vec3 ro, vec3 rd, vec2 xz, float R, float h) {
    Intersection result;
    result.hit = false;
    result.t   = -1.0;

    float tSide = intersectSide(ro, rd, xz, R, h);
    float tTop  = intersectDisc(ro, rd, xz, R, h);

    if(tSide < EPSILON && tTop < EPSILON) return result;
    bool hitSide = false;
    float t      = -1.0;

    if(tSide > 0.0 && tTop > 0.0) {
        if(tSide <= tTop) { t = tSide; hitSide = true; }
        else              { t = tTop;  hitSide = false;}
    } else if(tSide > 0.0) {
        t = tSide;
        hitSide = true;
    } else {
        t = tTop;
        hitSide = false;
    }
    if(t < 0.0) return result;

    result.hit   = true;
    result.t     = t;
    result.point = ro + t*rd;
    if(hitSide) {
        vec3 localPoint = result.point;
        localPoint.x -= xz.x;
        localPoint.z -= xz.y;
        result.normal = normalize(vec3(localPoint.x, 0.0, localPoint.z));
    } else {
        result.normal = vec3(0.0, 1.0, 0.0);
    }
    return result;
}

// ============================================================================
// LIGHT SAMPLING
// ============================================================================
vec3 sampleLight(Light light, vec3 lightIntersection, float LOD, float gamma) {
    // For a rectangular area light in plane (light.normal, light.position)
    vec3 zBasis = light.normal;
    vec3 xBasis = normalize(cross(vec3(0.0,1.0,0.0), zBasis));
    if(length(xBasis) < EPSILON) {
        xBasis = vec3(1.0, 0.0, 0.0);
    }
    vec3 yBasis = cross(zBasis, xBasis);

    float halfW = 0.5 * light.size.x;
    float halfH = 0.5 * light.size.y;

    vec3  v      = (lightIntersection - light.position);
    float localX = dot(v, normalize(xBasis));
    float localY = dot(v, normalize(yBasis));

    if(abs(localX) <= halfW && abs(localY) <= halfH) {
        // Convert to [0..1] for the texture
        vec2 uvLight = vec2(localX/halfW, localY/halfH);
        uvLight = uvLight*0.5 + 0.5;
        uvLight.x = 1.0 - uvLight.x; // flip x if needed

        // Sample the texture
        vec3 texColor = textureLod(u_lightTexture, uvLight, LOD).rgb;
        float lum = dot(texColor, vec3(0.299, 0.587, 0.114));
        float corrected = pow(lum, 1.0/gamma);
        float ratio = (lum>0.0) ? (corrected/lum) : 1.0;
        return texColor * ratio;
    }
    return vec3(0.0);
}

// ============================================================================
// FRESNEL
// ============================================================================
vec3 fresnelSchlick(float cosTheta, vec3 F0) {
    return F0 + (1.0 - F0)*pow(1.0 - cosTheta, 5.0);
}

// ============================================================================
// COSINE-WEIGHTED SAMPLING (for diffuse only)
// ============================================================================
vec3 lambertNoTangent(in vec3 normal, in vec2 uv) {
    float theta = 2.0 * PI * uv.x; 
    float y     = 2.0 * uv.y - 1.0;

    float r = sqrt(1.0 - y*y);
    vec3 sphereDir = vec3(
        r * cos(theta),
        r * sin(theta),
        y
    );
    return normalize(normal + sphereDir);
}

// ============================================================================
// MAIN
// ============================================================================
out vec4 fragColor;

void main() {
    // Eye geometry
    float pupilRadius    = 8.0;
    float cylinderHeight = 0.5;
    float scleraRadius   = 15.0;

    vec3  objectF0 = vec3(0.041808); // example Fresnel reflectance

    // Camera & Light
    Camera camera = Camera(
        vec2(36.0, 24.0),
        u_focalLength,
        u_cameraPos
    );
    Light light = Light(
        vec2(3000.0, 2000.0),
        u_lightPos,
        normalize(vec3(0.0, -1.0, 0.0))
    );

    mat4 view = viewMatrix(camera);

    // Build ray
    vec2 uv = gl_FragCoord.xy / u_resolution.xy;
    vec2 imagePlane = camera.sensorSize*(uv - 0.5);
    vec3 rayCamera = normalize(vec3(imagePlane.x, imagePlane.y, -camera.focalLength));
    vec4 rayWorldH = view * vec4(rayCamera, 0.0);
    vec3 rayWorld  = normalize(rayWorldH.xyz);

    // Intersect with pupil & sclera
    Intersection pupil  = intersectPupil(camera.position, rayWorld, vec2(0.0), pupilRadius, cylinderHeight);
    Intersection sclera = intersectSclera(camera.position, rayWorld, scleraRadius);

    vec3 intersectionWorld;
    vec3 hitNormal;
    vec3 baseColor;

    if(pupil.hit) {
        intersectionWorld = pupil.point;
        hitNormal         = pupil.normal;
        baseColor         = vec3(0.0, 0.0, 0.0); // black pupil
    } else if(sclera.hit) {
        intersectionWorld = sclera.point;
        hitNormal         = sclera.normal;
        // optional normal perturb
        hitNormal         = perturb_normal(hitNormal, intersectionWorld, 0.1, 0.2);
        baseColor         = vec3(0.9, 0.9, 0.9); // white sclera
    } else {
        // Missed both
        fragColor = vec4(0.5, 0.5, 0.5, 1.0);
        return;
    }

    // ------------------------------------------------------------------------
    // 1) DIFFUSE term from Lambertian sampling (for the base color)
    // ------------------------------------------------------------------------
    vec3 diffuseLight = vec3(0.0);
    for(int i = 0; i < SAMPLES; i++) {
        vec2 randSeed = gl_FragCoord.xy + float(i)*0.1234;
        float r1 = random(randSeed + 12.345);
        float r2 = random(randSeed + 98.765);

        // direction ~ cos-weighted about hitNormal
        vec3 sampleDir = lambertNoTangent(hitNormal, vec2(r1, r2));

        // offset to avoid self-shadow
        vec3 origin = intersectionWorld + hitNormal*1e-4;

        // Check pupil intersection
        if (!pupil.hit) {
            Intersection pupilShadow = intersectPupil(origin, sampleDir, vec2(0.0), pupilRadius, cylinderHeight);

            if (pupilShadow.hit) {
                continue;
            }
        }

        float tLight = intersectRayPlane(origin, sampleDir, u_lightPos, light.normal);
        if(tLight > 0.0) {
            vec3 Lpos = origin + tLight*sampleDir;
            vec3 lightSample = sampleLight(light, Lpos, 10.0, 0.5) * 10.0;
            float nDotL = max(dot(hitNormal, sampleDir), 0.0);
            // accumulate
            diffuseLight += (lightSample * nDotL);
        }
    }
    // average & 1/pi factor
    diffuseLight /= float(SAMPLES);
    diffuseLight *= (1.0/PI);

    // The final base/diffuse contribution
    vec3 diffuseColor = baseColor * diffuseLight;

    // ------------------------------------------------------------------------
    // 2) SPECULAR reflection (same as your original single reflection approach)
    // ------------------------------------------------------------------------
    // Reflection direction & intersection with the light plane:
    vec3 reflectionDir = reflect(rayWorld, hitNormal);
    float tRefl = intersectRayPlane(intersectionWorld, reflectionDir, u_lightPos, light.normal);
    vec3 reflectionColor = vec3(0.0);
    if(tRefl > 0.0) {
        vec3 lightHit = intersectionWorld + tRefl*reflectionDir;
        reflectionColor = sampleLight(light, lightHit, 0.0, 0.1) * u_lightRadiance;
    }

    // ------------------------------------------------------------------------
    // 3) Fresnel & Combine
    // ------------------------------------------------------------------------
    float cosTheta = max(dot(-rayWorld, hitNormal), 0.0);
    vec3 F = fresnelSchlick(cosTheta, objectF0);

    // final color: mix diffuse & reflection with Fresnel
    vec3 colorLinear = (1.0 - F)*diffuseColor + F*reflectionColor;
    colorLinear = clamp(colorLinear, 0.0, 1.0);

    fragColor = vec4(colorLinear, 1.0);
}