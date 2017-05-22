#define SHADOW_BIAS 0.85

#define WAVING_TERRAIN

//#define POM
  #define POM_MAP_RES 64. //[16. 32. 64. 128. 256. 512.]
  #define POM_DEPTH 4. //actually inverse POM depth

#define VPSS
#define SSAO
  #define SSAO_METHOD 1 //[1 2]

  #define SSAO_STRENGTH 0.2 //[0.2 0.5 1.0 2.0 3.0 4.0 5.0]

#define AMBIENT_LIGHTING 2 //[1 2 3] 1: Ambient light approximation, no AO. 2: Ambient light approximation with AO. 3: Full ambient diffuse

#define VOLUMETRIC_LIGHT
  #define COLORED_VL
  #define VL_DISTANCE 256.0
  #define VL_STEPS 3

#define REFLECTIONS
  #define ROUGH_REFLECTIONS
  #define R_QUALITY 5 //[1 5 10 15 20 30 40 60 80 100] Quality of the raytracer. Higher numbers mean more accurate reflections
  #define R_STEPS 1 //[1 2 4 8 12 20] Quality of raytracer. Higher numbers mean less speckly rough reflections

#define ROUGH_WATER

//#define WATER_PARALLAX

//#define CLOUDS
  #define CLOUD_STYLE 2 //[1 2]
  #define CLOUD_HEIGHT_2D   512  // [384 512 640 768] For cloud style 1
  #define CLOUD_COVERAGE_2D 0.5
  #define CLOUD_SPEED_2D    1.00 // [0.25 0.50 1.00 2.00 4.00] For cloud style 1

#define VOLUMETRIC_CLOUDS
  #define VOLUMETRIC_CLOUDS_DENSITY 20.0 //[10.0 20.0 40.0 50.0 80.0 100.0 150.0 200.0 250.0]
  #define VOLUMETRIC_CLOUDS_COVERAGE 1.25 //[0.5 0.75 1.0 1.25 1.5 1.75 2.0]
  #define VOLUMETRIC_CLOUDS_HEIGHT 160.0 //[60.0 80.0 100.0 120.0 140.0 160.0 180.0 200.0 220.0 240.0 260.0]

#define SKY_MODEL 1 //[1 2] 1: JOOOODIE, faster but still realistic. 2: BROOOOCE, slow but beautiful
#define HORIZON_HEIGHT 5 // [5 62 72 80 128 192 208]

#define BLOOM

#define MOTIONBLUR
  #define LQ_MOTIONBLUR //Low quality motion blur. Requires MOTIONBLUR. ENABLE ONE!
  //#define HQ_MOTIONBLUR //High quality motion blur. Requires MOTIONBLUR. ENABLE ONE!
  #define MOTIONBLUR_AMOUNT 1.0

//#define DYN_LENS

//#define DOF
  //#define HQ_DOF
  //#define TILT_SHIFT
  #define FSTOP 2.8 //[1.0 1.4 2.0 2.8 4.0 5.6 8.0 11.0 16.0] //The ratio of focal length to diaphram size, for more precise numbers go to lib/settings.glsl and change this number manually.
  #define ITERATIONS 128
  #define DISTORTION_ANAMORPHIC	1.0
  #define DISTORTION_BARREL 0.6

  #define BLADES 5 //[3 4 5 6 7 8] //Number of blades in the aperture. This number is not limited, for more numbers go to lib/settings.glsl and change this number manually.
  #define ROTATION 0.8 //[0.0 0.2 0.4 0.6 0.8 1.0] //How much the aperture is rotated. 0.0 has no rotation.
  #define ROUNDING 0.6 //[0.0 0.2 0.4 0.6 0.8 1.0] //How much the aperture is rounded. 0.0 is a perfect polygon, 1.0 is a perfect circle.
  #define BIAS 0.2 //[0.0 0.2 0.4 0.6 0.8 1.0] //How much light is spread to the outer points of the aperture. 0.0 is a solid aperture, 1.0 is a catadioptric aperture.
  #define LENS_SHIFT_AMOUNT 0.1 //[0.0 0.1 0.2 0.3 0.4 0.5] //The factor for how much light is split outwards. 0.0 has no splitting mimicking a very high quality glass, 0.5 is an unrealistic ammount of splitting.
  #define GRUNGE 0.0 //[0.0 0.2 0.4 0.6 0.8 1.0] //Ammount of variation in the bokeh. 0.0 is no grunge, 1.0 is default grunge.

//#define LENS_DISTORTION
  #define LDIST_AMOUNT 0.8

#define AVERAGE_EXPOSURE_STRENGTH 1.15

#define FILM_GRAIN
	#define FILM_GRAIN_STRENGTH 0.2 //[0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 0.1]

//#define EDGE_SHARPENING

#define TONEMAP_PRESET 5 //[1 2 3 4 5 6]
