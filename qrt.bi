 /'*********************************************************
             Header file for Quick Ray Trace
                     Steve Koren
 *********************************************************'/


#ifndef NULL
  #define NULL cptr(any ptr,0)
#EndIf

' mis propias definiciones (dado que FALSE lo prefiero como "0")
#Undef BOOL
#Define BOOL UByte

#Undef TRUE
#Define TRUE 1

#Undef FALSE
#Define FALSE 0




/'*********************************************************

                   MISC MATH CONSTANTS

 *********************************************************'/
#Ifndef M_PI
	#Define M_PI 3.14159265359
	#Define PI M_PI
#EndIf
#define PI2 PI/2


/'*********************************************************
                     OBJECT  NUMBERS
 *********************************************************'/
#define _LINE               1
#define _SPHERE             2
#define _PARALLELOGRAM      3
#define _TRIANGLE           4
#define _LAMP               5
#define _OBSERVER           6
'#define _GROUND             7 ' no se usa
#define _SKY                8
#define _BBOX               9
#define _RING               10
#define _QUADRATIC          11


/'*********************************************************
                   PROGRAM CONSTANTS
 *********************************************************'/
#define SMALL    1.0e-2      /' a sorta small number  1.0e-3  '/
#define BIG      1.0e10      /' a sorta big number    1.0e13  '/
#define CNUMB    64          /' this many shades/colors  '/
#define SLEN     64          /' max string length       '/
#define MAX_IX   8           /' 4 maximum x interpolation '/
#define MAX_IY   8           /' 4 maximum y interpolation '/
#define MAX_XRES 2500        /' antes 800 - maximum X resolution '/
#define XYZZY    42          /' this is here for no reason whatsoever       '/

Dim Shared As long CNUM


/'*********************************************************
                      VECTOR STRUCTURES
 *********************************************************'/
Type VECTOR         /' As a vector in 3 space   '/
  As Single x,y,z 
End Type 
type as VECTOR ptr VECT_PTR


Type SVECTOR        /' As an r,g,b colors vector '/
  As Short r,g,b 
End Type 
type as SVECTOR ptr SVECT_PTR


Type CINFOS   /' As colors information     '/
	As SVECTOR amb           /' ambient lighting      '/
	As SVECTOR diff          /' diffuse lighting      '/
	As SVECTOR mirror        /' % light reflected     '/
	As SVECTOR transp        /' % light transmitted   '/

	As VECTOR density        /' density '/

	As Single sreflect       /' specular refl coefficient '/
	As Single index          /' index if refraction '/

	As Short fuzz            /' currently unused '/
	As Short reflect         /' percent specularly reflected '/
	As Short dither          /' colors dithering. 3..6 look ok '/
End Type 
type as CINFOS ptr CINFO_PTR


/'*********************************************************
                PRECOMPUTED INFO FOR OBJECTS

 These fields can be used by objects routines however
 they wish.  They just make objects/lines intersections
 faster.
 *********************************************************'/
Type PRECOMPS  
	As Single sin1, cos1        /' sin and cos '/
	As Single sin2, cos2
	As Single n1                /' misc number '/
	As Single len1, len2        /' lengths of vectors '/

	As VECTOR vect1             /' misc vector '/
	As VECTOR norm              /' norm for planar objs '/
End Type 
type as PRECOMPS PRECOMP_PTR


/'*********************************************************
                      PATTERN STRUCTURE
 *********************************************************'/
Type PATTERNS
	As Short  types              /' type of pattern '/
	
	As Single xsize              /' pattern size '/
	As Single ysize
	As Single startx
	As Single starty             /' x,y positions '/
	As Single endx
	As Single endy
	As Single radius             /' rad for circles '/

	As CINFOS cinfo              /' colors information '/

	As string names              /' pattern name '/

	As PATTERNS PTR child, sibling, link 
End Type 
TYPE AS PATTERNS PTR PATTERN_PTR


/'*********************************************************
                    OBJECT STRUCTURE
 *********************************************************'/
Type OBJ_STRUCT
	As Short types         /' objects type '/
	As SHORT flag          /' misc boolean flag '/

	As string names        /' objects name '/

	As VECTOR locs         /' objects location '/
	As VECTOR vect1        /' three vectors '/
	As VECTOR vect2
	As VECTOR vect3
	As VECTOR lower        /' lower and upper bounds '/
	As VECTOR upper 

	As Single cterm        /' for quadratic surfaces only '/
	As Single xmult        /' x and y multipliers for patterns '/
	As Single ymult 

	As CINFOS cinfo        /' colors information '/

	As PRECOMPS precomp     /' precomputed information '/

	As OBJ_STRUCT PTR nextobj  /' next obj in list '/
	As OBJ_STRUCT PTR child    /' child for bounding boxes only '/

	As PATTERN_PTR pattern     /' pointer to pattern structure '/
	As PATTERN_PTR remove      /' remove section of objects '/
End Type 
TYPE AS OBJ_STRUCT PTR OBJ_PTR


/'*********************************************************
                   Plane Bbox structure
 *********************************************************'/
Type PLANE_BBOX
  As Integer min_x, min_y
  As Integer max_x, max_y
  As OBJ_PTR objects 
  'As PLANE_BBOX PTR next_ 
End Type 
TYPE AS PLANE_BBOX PTR PLANE_BBOX_PTR


/'*********************************************************
                       WORLD STRUCTURE
 *********************************************************'/
Type WORLD 
	As OBJ_PTR stack            /' here are the objects '/
	As OBJ_PTR observer         /' the observer '/
	As OBJ_PTR sky              /' sky '/
	As OBJ_PTR lamps            /' a lamp list '/
	As OBJ_PTR instances        /' instance list '/

	As Integer objcount         /' # objects and lamps '/
	As Integer lampcount 

	As Long ray_intersects      /' statistics '/
	As Long primary_traced
	As Long to_lamp
	As Long refl_trans
	As Long bbox_intersects
	As Long intersect_tests
	As Long pixels_hit
	As Long pattern_matches 

	As VECTOR obsright          /' obs up dir '/
	As VECTOR obsup 

	As SVECTOR skycolor_horiz   /' skycolors '/
	As SVECTOR skycolor_zenith 

	As PATTERN_PTR patlist      /' the pattern stack '/

	As Single flength           /' focal length '/
	As Single x_divisor         /' used to find ray direction '/
	As Single y_divisor
	As Single globindex         /' global index of refraction '/

	As string outfile           /' output file name '/
	As integer filept           /' output file pointer '/
End Type 
Dim Shared As WORLD THEWORLD  /' THE WORLD in the mind of a computer '/


/'*********************************************************
                  FUNCTIONS FOR OBJECT TYPES
 *********************************************************'/
Type OBJ_DATA 
  ColTest  As Function(lines As OBJ_PTR , quad As OBJ_PTR ,byref t As Single ) As Integer' collision test function ptr
  FindNorm As Sub(norm As VECT_PTR , objects As OBJ_PTR , position As VECT_PTR) ' normal finding function ptr
  FindBbox As Sub(v1 As VECT_PTR , v2 As VECT_PTR , sphere As OBJ_PTR) ' objects bound function ptr
  RelPos   As Sub(obj As OBJ_PTR , locs As VECT_PTR , byref pos1 As Single ,byref pos2 As Single , normalize as integer = 0) ' objects relative position
  PreComp  As Sub(obj As OBJ_PTR) ' info pre-computing routine
  Offset   As Sub(obj As OBJ_PTR , offset As VECT_PTR) ' offset objects by dx, dy, dz
  Resize   As Sub(obj As OBJ_PTR , mult As VECT_PTR) ' resize objects by a multiple
End Type 


/'*********************************************************
                      MATH DEFINES
 *********************************************************'/
#define POW(x) ((x)*(x))
#define DOTPROD(v1,v2) ((v1).x*(v2).x+(v1).y*(v2).y+(v1).z*(v2).z)
#define MIN(x,y) (iif((x)<(y),(x),(y)))
#define MAX(x,y) (iif((x)>(y),(x),(y)))


/'*********************************************************
                      Default structure
 *********************************************************'/
Type DEFS 
	As CINFOS cinfo            /' default colorinfo '/

	As Short shadow            /' shadows ? '/
	As Short vlamp             /' lamps visible (not yet implimented) '/
	As Short int_x             /' interpolate (def=1) '/
	As Short int_y

	As Short x_res             /' X resolution of image '/
	As Short y_res             /' Y resolution of image '/
	As Short x_center          /' X center of image '/
	As Short y_center          /' Y center of image '/

	As Single threshold        /' cutoff pt for min refl, refl rays '/
	As Single aspect           /' aspect ratio for image            '/

	As Short ithreshold        /' integer version of above          '/
End Type 
TYPE AS DEFS PTR DEF_PTR
Dim Shared As DEFS def 



/'*********************************************************
                       ERROR CODES
 *********************************************************'/
#define ILLEGAL_PARAMETER 1
#define TOO_FEW_PARMS     2
#define ILLEGAL_OBJECT    3
#define MALLOC_FAILURE    4
#define SYNTAX_ERROR      5
#define INTERNAL_ERROR    6
#define FILE_ERROR        7
#define PATTERN_NOT_FOUND 8
#define PATTERN_EXISTS    9
#define NO_OBSERVER       10
#define UNDEFINED_PARAM   11
#define NON_HOMOGENIOUS   12
#define ZERO_INDEX        13
#define COLOR_VALUE_ERR   14
#define LESS_THAN_ZERO    15
#define ZERO_MULTIPLIER   16
#define UNDEFINED_NAME    17
#define LPAREN_EXPECTED   18
#define RPAREN_EXPECTED   19
#define ILLEGAL_VECTOR    20
#define ILLEGAL_SVECTOR   21
#define ILLEGAL_OPTION    22


/'*********************************************************
                      WARNING CODES
 *********************************************************'/
#define OBSOLETE_OPTION   1


/'*********************************************************
 Define this flag for more robust code (HIGHLY recommended)
 *********************************************************'/
#define ROBUST TRUE


