#version 300 es
precision mediump float;

// ============================================================================
// UNIFORMS
// ============================================================================
uniform vec3  u_cameraPos;
uniform vec3  u_cameraNormal;
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
#define SAMPLES 64

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
// float random(vec2 st) {
//     // A simple 2D→float hash
//     return fract(sin(dot(st.xy, vec2(12.9898,78.233))) * 43758.5453123);
// }
float hash12(vec2 p) {
    // "Hash without sine" by Inigo Quilez
    vec3 p3 = fract(vec3(p.x, p.y, p.x) * 0.1031);
    p3 += dot(p3, p3.yzx + 31.32);
    return fract((p3.x + p3.y) * p3.z);
}

float random(vec2 st) {
    return hash12(st);
}

float random3(vec3 c) {
    return fract(sin(dot(c, vec3(12.9898, 78.233, 37.425))) * 43758.5453123);
}
// ============================================================================
// NOISE (for normal perturb if desired)
// ============================================================================

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
    vec3 zBasis = normalize(camera.position);
    vec3 xBasis;
    vec3 yBasis;
    if (true) {
        xBasis = vec3(1, 0, 0);
        yBasis = cross(zBasis, xBasis);
    } else {
        xBasis = normalize(cross(vec3(0,1,0), zBasis));
        yBasis = cross(zBasis, xBasis);
    }

    return mat4(
        vec4(xBasis, 0.0),
        vec4(yBasis, 0.0),
        vec4(zBasis, 0.0),
        vec4(camera.position, 1.0)
    );
}

mat4 lookAt(vec3 camera, vec3 target, vec3 up) {
    vec3 f = normalize(target - camera);      // forward
    vec3 s = normalize(cross(f, up));        // right
    vec3 u = cross(s, f);                    // recalculated up

    return mat4(
        vec4(s, 0.0),
        vec4(u, 0.0),
        vec4(-f, 0.0),
        vec4(-dot(s, camera), -dot(u, camera), dot(f, camera), 1.0)
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

// ----------------------------------------------------------------------------
// CORNEA INTERSECTION
// ----------------------------------------------------------------------------

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
    if(t1 > EPSILON) {
        vec3 pos = rayOrigin + t1 * rayDir;
        if(pos.y >= cylinderHeight)
            tCap = t1;
    }
    if(tCap < 0.0 && t2 > EPSILON) {
        vec3 pos = rayOrigin + t2 * rayDir;
        if(pos.y >= cylinderHeight)
            tCap = t2;
    }
    return tCap;
}

Intersection intersectCornea(
    vec3 ro,            // Ray origin
    vec3 rd,            // Ray direction
    float radius,       // Horizontal radius of cornea
    float cylinderH,    // Height of the cornea's cylindrical part (e.g. 2.0)
    float flatten       // Flatten factor for the cap portion (e.g. 0.3)
)
{
    Intersection result;
    result.hit    = false;
    result.t      = -1.0;
    result.point  = vec3(0.0);
    result.normal = vec3(0.0);

    // Intersect the cylindrical side and the ellipsoidal cap.
    float tCyl = intersectSide(ro, rd, vec2(0.0f), radius, cylinderH);
    float tCap = intersectCap(ro, rd, radius, cylinderH, flatten);

    // If both are invalid or negative, exit immediately.
    if(tCyl < EPSILON && tCap < EPSILON) {
        return result;
    }

    // Decide which intersection is closer and valid.
    float tHit       = -1.0;
    bool hitCylinder = false;

    if(tCyl > EPSILON && tCap > EPSILON) {
        // Both hits are valid; pick the nearer one.
        if(tCyl <= tCap) {
            tHit       = tCyl;
            hitCylinder = true;
        } else {
            tHit       = tCap;
            hitCylinder = false;
        }
    }
    else if(tCyl > EPSILON) {
        tHit       = tCyl;
        hitCylinder = true;
    }
    else {
        tHit       = tCap;
        hitCylinder = false;
    }

    // If still invalid, no hit.
    if(tHit < EPSILON) {
        return result;
    }

    // Populate Intersection data.
    result.hit   = true;
    result.t     = tHit;
    result.point = ro + tHit * rd;

    if(hitCylinder) {
        // Cylinder side normal: points radially outward in XZ.
        // (y-component is 0, so it’s “horizontal.”)
        result.normal = normalize(vec3(result.point.x, 0.0, result.point.z));
    } 
    else {
        // We hit the cap. Compute an ellipsoidal normal,
        // then blend with the cylinder normal near the seam.
        
        // 1) Cap normal
        vec3 capCenter  = vec3(0.0, cylinderH, 0.0);
        vec3 capLocal   = result.point - capCenter;
        float scaleY    = radius * flatten;  // flatten in the Y-direction
        vec3 capNormal  = normalize(vec3(
            capLocal.x / (radius * radius),
            capLocal.y / (scaleY * scaleY),
            capLocal.z / (radius * radius)
        ));

        // 2) Cylinder normal
        vec3 cylNormal  = normalize(vec3(result.point.x, 0.0, result.point.z));

        // 3) Blending factor: from 'cylinderH' (pure cylinder)
        //    to 'cylinderH + (radius/10)' (pure cap).
        float blend = smoothstep(
            cylinderH, 
            cylinderH + radius*0.1, 
            result.point.y
        );

        // 4) Blend and normalize
        result.normal = normalize(mix(cylNormal, capNormal, blend));
    }

    return result;
}
// ============================================================================
// LIGHT SAMPLING
// ============================================================================


vec3 sampleLight(Light light, Intersection hit, vec3 sampleDir, float tLight, vec3 lightColor) {
    vec3 lightIntersection = hit.point + tLight*sampleDir;
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
        float distance = length(lightIntersection - hit.point) / 1000.0;
        float attenuation = 1.0 / (distance * distance);
        float NdotL = max(dot(hit.normal, sampleDir), 0.0);
        
        return lightColor * attenuation * NdotL;
    }
    return vec3(0.0);
}

vec3 sampleTextureLight(Light light, Intersection hit, vec3 sampleDir, float tLight, float LOD, float gamma) {
    vec3 lightIntersection = hit.point + tLight*sampleDir;
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
        float distance = length(lightIntersection - hit.point) / 1000.0;
        float attenuation = 1.0 / (distance * distance);
        float NdotL = max(dot(hit.normal, sampleDir), 0.0);
        return texColor * ratio * attenuation * NdotL;
    }
    return vec3(0);
}

// ============================================================================
// FRESNEL
// ============================================================================
float fresnelSchlick(float cosTheta, float F0) {
    return F0 + (1.0 - F0)*pow(1.0 - cosTheta, 5.0);
}

float fresnelThinDialectric(float cosTheta, float F0) { 
    float R = fresnelSchlick(cosTheta, F0);
    float T = 1.0 - R;
    if (R < 1.0) {
        R += sqrt(T) * R / (1.0 - sqrt(R));
    }
    return R;
}

// ============================================================================
// COSINE-WEIGHTED SAMPLING (for diffuse only)
// ============================================================================
vec3 lambertNoTangent(vec3 normal, vec2 uv) {
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

vec3 refractRay(vec3 I, vec3 N, float eta)
{
    float cosi  = dot(-I, N);
    float cost = sqrt(1.0 - eta*eta*(1.0 - cosi*cosi));
    // Snell's law refraction
    vec3 T = eta * I + (eta * cosi - cost) * N;
    T = normalize(T);
    return T;
}
// ============================================================================
// MAIN
// ============================================================================
out vec4 fragColor;

void main() {
    // Eye geometry
    float pupilRadius    = 8.0;
    float cylinderHeight = 2.5;
    float scleraRadius   = 15.0;
    float corneaHeight = 3.0f;
    float corneaRadius   = 14.5;
    float flatten = 0.4f;

    float  objectF0 = 0.041808;

    // Camera & Light
    Camera camera = Camera(
        vec2(36.0, 24.0),
        u_focalLength,
        u_cameraPos
    );

    vec3 lightColor = vec3(1.0f);
    Light light = Light(
        // vec2(3000.0, 2000.0),
        u_resolution,
        u_lightPos,
        normalize(vec3(0.0, -1.0, 0.0))
    );

    mat4 view = viewMatrix(camera);
    // mat4 view = lookAt(camera.position, vec3(0), vec3(EPSILON, 1, EPSILON));

    // Build ray
    vec2 uv = gl_FragCoord.xy / u_resolution.xy;
    vec2 imagePlane = camera.sensorSize*(uv - 0.5);
    vec3 rayCamera = normalize(vec3(imagePlane.x, imagePlane.y, -camera.focalLength));
    vec4 rayWorldH = view * vec4(rayCamera, 0.0);
    vec3 rayWorld  = normalize(rayWorldH.xyz);

    // Intersections
    Intersection pupil  = intersectPupil(camera.position, rayWorld, vec2(0.0), pupilRadius, cylinderHeight);
    Intersection sclera = intersectSclera(camera.position, rayWorld, scleraRadius);
    Intersection cornea  = intersectCornea(camera.position, rayWorld, corneaRadius, corneaHeight, flatten);

    vec3 baseColor;

    vec3 colorClear = vec3(0.93, 0.97, 1.0);
    float F_clear = 0.0;
    if (cornea.hit) {
        // Cornea relfection
        vec3 reflectionDir = reflect(rayWorld, cornea.normal);
        float tRefl = intersectRayPlane(cornea.point, reflectionDir, u_lightPos, light.normal);
        vec3 reflectionColor = 0.2 * colorClear * u_lightRadiance;
        if(tRefl > 0.0) {
            // reflectionColor = sampleTextureLight(light, cornea, reflectionDir, tRefl, 0.0, 0.05) * u_lightRadiance;
            vec3 lightSampleTexture = sampleTextureLight(light, cornea, reflectionDir, tRefl, 0.0, 0.1) * 10.0 * u_lightRadiance;
            vec3 lightSample = sampleLight(light, cornea, reflectionDir, tRefl, lightColor);
            // Balance both types of lights
            float bias = 0.9;
            reflectionColor += bias * lightSampleTexture + (1.0 - bias) * lightSample;
        }
        float cosTheta = max(dot(-rayWorld, cornea.normal), 0.0);
        F_clear = fresnelThinDialectric(cosTheta, objectF0);
        colorClear = F_clear*reflectionColor + 0.2*colorClear;
    }

    Intersection hit;
    if(pupil.hit) {
        hit = pupil;
        baseColor = vec3(0.05f);
    } else if(sclera.hit) {
        hit = sclera;
        hit.normal = perturb_normal(hit.normal, hit.point, 0.05, 0.05);
        baseColor = vec3(0.95f);
    }

    // ------------------------------------------------------------------------
    // 1) DIFFUSE term from Lambertian sampling
    // ------------------------------------------------------------------------
    vec3 diffuseColor = vec3(0);
    if (pupil.hit || sclera.hit) {
        // If sclera hit and cornea hit are close in x and z, lots of light is lost
        vec3 refracted = refractRay(rayWorld, cornea.normal, 1.5);
        if (true) {
            vec3 diffuseLight = vec3(0.0);
            for(int i = 0; i < SAMPLES; i++) {
                vec2 randSeed = gl_FragCoord.xy + float(i)*0.1234;
                float r1 = random(randSeed + 12.345);
                float r2 = random(randSeed + 98.765);
                vec3 sampleDir = lambertNoTangent(hit.normal, vec2(r1, r2));

                vec3 origin = hit.point + hit.normal*EPSILON;

                // Shadow from pupil
                if (!pupil.hit) {
                    Intersection pupilShadow = intersectPupil(origin, sampleDir, vec2(0.0), pupilRadius, cylinderHeight);

                    if (pupilShadow.hit) {
                        continue;
                    }
                }

                float tLight = intersectRayPlane(origin, sampleDir, u_lightPos, light.normal);
                if(tLight > 0.0) {
                    vec3 lightSampleTexture = sampleTextureLight(light, hit, sampleDir, tLight, 0.0, 0.1);
                    vec3 lightSample = sampleLight(light, hit, sampleDir, tLight, lightColor);
                    // Balance both types of lights
                    float bias = 0.7;
                    diffuseLight += bias * lightSampleTexture + (1.0 - bias) * lightSample;
                }
            }

            diffuseLight /= float(SAMPLES);
            diffuseLight *= u_lightRadiance;
            diffuseLight *= (1.0/PI);

            diffuseColor = baseColor * diffuseLight;
        }
    }

    // ------------------------------------------------------------------------
    // 2) SPECULAR reflection
    // ------------------------------------------------------------------------
    vec3 reflectionDir = reflect(rayWorld, hit.normal);
    float tRefl = intersectRayPlane(hit.point, reflectionDir, u_lightPos, light.normal);
    vec3 reflectionColor = vec3(0.0);
    if(tRefl > 0.0) {
        reflectionColor = sampleTextureLight(light, hit, reflectionDir, tRefl, 0.0, 0.1)*u_lightRadiance;
    }

    // ------------------------------------------------------------------------
    // 3) Fresnel & Combine
    // ------------------------------------------------------------------------
    float cosTheta = max(dot(-rayWorld, hit.normal), 0.0);
    float F_opaque = fresnelSchlick(cosTheta, objectF0);

    vec3 colorOpaque = (1.0 - F_opaque)*diffuseColor + F_opaque*reflectionColor;
    vec3 ambient = 0.1 * baseColor;
    colorOpaque += ambient;
     
    vec3 finalColor = F_clear*colorClear + (1.0 - F_clear)*colorOpaque;
    fragColor = vec4(finalColor, 1.0);
}