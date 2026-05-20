 

 ' -------------------------------------------------
 ' FICHERO --> BBOX.C
 ' -------------------------------------------------
Declare Sub BboxBbox(v1 As VECT_PTR , v2 As VECT_PTR , bbox As OBJ_PTR) 
Declare Sub BboxSphere(v1 As VECT_PTR , v2 As VECT_PTR , sphere As OBJ_PTR) 
Declare Sub BboxParallelogram(v1 As VECT_PTR , v2 As VECT_PTR , para As OBJ_PTR) 
Declare Sub BboxTriangle(v1 As VECT_PTR , v2 As VECT_PTR , obj As OBJ_PTR) 
Declare Sub BboxRing(v1 As VECT_PTR , v2 As VECT_PTR , ring As OBJ_PTR) 
Declare Sub BboxQuadratic(v1 As VECT_PTR , v2 As VECT_PTR , quad As OBJ_PTR) 


 ' -------------------------------------------------
 ' FICHERO --> ERROR.C
 ' -------------------------------------------------
Declare Sub Warning(num As Integer , messg As string) 
Declare Sub Errors(num As Integer , code As Integer) 
Declare Sub RayErr()


 ' -------------------------------------------------
 ' FICHERO --> INOUT.C
 ' -------------------------------------------------
Declare Sub def_colorinfo(cinfo As CINFO_PTR) 
Declare Sub copy_colorinfo(c1 As CINFO_PTR , c2 As CINFO_PTR) 
Declare Function GetOpt(strs As string , cinfo As CINFO_PTR) As Integer 
Declare Function Get_Primitive(strs As string) As OBJ_PTR 
Declare Function GetAttrib(strs As string) As Integer 


 ' -------------------------------------------------
 ' FICHERO --> INSTANCE.C
 ' -------------------------------------------------
Declare Function Name_Find(obj As OBJ_PTR , names As string) As OBJ_PTR 
Declare Function Subtree_Copy(obj As OBJ_PTR , fflag As Integer) As OBJ_PTR 
Declare Sub Subtree_Offset(obj As OBJ_PTR , offset As VECT_PTR , fflag As Integer) 
Declare Sub Subtree_Scale(obj As OBJ_PTR , mult As VECT_PTR , fflag As Integer) 


 ' -------------------------------------------------
 ' FICHERO --> INTERSEC.C
 ' -------------------------------------------------
Declare Function LineBbox         (lines As OBJ_PTR , bbox As OBJ_PTR ,byref t As Single ) As Integer 
Declare Function LineRing         (lines As OBJ_PTR , ring As OBJ_PTR ,byref t As Single ) As Integer 
Declare Function LineParallelogram(lines As OBJ_PTR , para As OBJ_PTR ,byref t As Single ) As Integer 
Declare Function LineTriangle     (lines As OBJ_PTR , obj  As OBJ_PTR ,byref t As Single ) As Integer 
Declare Function LineSphere       (lines As OBJ_PTR , sph  As OBJ_PTR ,byref t As Single ) As Integer 
Declare Function LineQuadratic    (lines As OBJ_PTR , quad As OBJ_PTR ,byref t As Single ) As Integer 


 ' -------------------------------------------------
 ' FICHERO --> LEXER.C
 ' -------------------------------------------------
Declare Function towhite(c As string) As string 
Declare Sub GetToken(s As string)
Declare Function InRange(cnum As Single) As Single 
Declare Function IsPos(val As Single) As Single 
Declare Sub GetVector(vector As VECT_PTR) 
Declare Sub GetSVector(svector As SVECT_PTR) 
Declare Sub GetLeftParen() 


 ' -------------------------------------------------
 ' FICHERO --> MTH.C
 ' -------------------------------------------------
Declare Sub VectEqZero(v As VECT_PTR) 
Declare Sub VectAddMult(v1 As VECT_PTR , k2 As Single , v2 As VECT_PTR , k3 As Single , v3 As VECT_PTR) 
Declare Sub VectNegate(v1 As VECT_PTR , v2 As VECT_PTR) 
Declare Sub VectorAdd(v1 As VECT_PTR , v2 As VECT_PTR , v3 As VECT_PTR) 
Declare Sub VectorMult(v1 As VECT_PTR , v2 As VECT_PTR , v3 As VECT_PTR) 
Declare Sub VecSubtract(v1 As VECT_PTR , v2 As VECT_PTR , v3 As VECT_PTR) 
Declare Sub VectScale(v1 As VECT_PTR , k As Single) 
Declare Sub SVectEQ(v1 As SVECT_PTR , v2 As SVECT_PTR) 
Declare Sub VectEQ(v1 As VECT_PTR , v2 As VECT_PTR) 
Declare Sub Normalize(v As VECT_PTR) 
Declare Sub CrossProd(v1 As VECT_PTR , v2 As VECT_PTR , v3 As VECT_PTR) 
Declare Sub FindPos(pos As VECT_PTR , line2 As OBJ_PTR , t As Single) 
Declare Sub Reflect(out As VECT_PTR , in As VECT_PTR , norm As VECT_PTR) 
Declare Sub Rot12(invect As VECT_PTR , outvect As VECT_PTR , cos1 As Single , sin1 As Single , cos2 As Single , sin2 As Single) 
Declare Sub Rot21(invect As VECT_PTR , outvect As VECT_PTR , cos1 As Single , sin1 As Single , cos2 As Single , sin2 As Single) 
Declare Function atan2w(x As Single , y As Single) As Single 


 ' -------------------------------------------------
 ' FICHERO --> NORM.C
 ' -------------------------------------------------
Declare Sub SphereNorm(norm As VECT_PTR , objects As OBJ_PTR , position As VECT_PTR) 
Declare Sub PlaneNorm(norm As VECT_PTR , objects As OBJ_PTR , position As VECT_PTR) 
Declare Sub QuadraticNorm(norm As VECT_PTR , objects As OBJ_PTR , position As VECT_PTR) 


 ' -------------------------------------------------
 ' FICHERO --> OFFSET.C
 ' -------------------------------------------------
Declare Sub Standard_Offset(obj As OBJ_PTR , offset As VECT_PTR) 
Declare Sub Offset_Bbox(obj As OBJ_PTR , offset As VECT_PTR) 


 ' -------------------------------------------------
 ' FICHERO --> PATTERN.C
 ' -------------------------------------------------
Declare Function Find_Color( obj As OBJ_PTR , pattern As PATTERN_PTR , locs As VECT_PTR , cinfo As CINFO_PTR , xmult As Single , ymult As Single) As Integer 
Declare Function Rect_Hit(pos1 As Single , pos2 As Single , patt As PATTERN_PTR) As Integer 
Declare Function Circle_Hit(pos1 As Single , pos2 As Single , patt As PATTERN_PTR) As Integer 
Declare Function line_intersect(px As Single , py As Single , x1 As Single , y1 As Single , x2 As Single , y2 As Single) As Integer 
Declare Function Poly_Hit(pos1 As Single , pos2 As Single , patt As PATTERN_PTR) As Integer 


 ' -------------------------------------------------
 ' FICHERO --> PATTERNI.C
 ' -------------------------------------------------
Declare Function Get_SubPattern( str As string) As PATTERN_PTR 
Declare Function GetPattern() As Integer


 ' -------------------------------------------------
 ' FICHERO --> PRECOMP.C
 ' -------------------------------------------------
Declare Sub PreCompSphere(obj As OBJ_PTR) 
Declare Sub PreCompPlane(obj As OBJ_PTR) 
Declare Sub PreCompQuadratic(obj As OBJ_PTR) 
Declare Sub PreCompNull(obj As OBJ_PTR) 


 ' -------------------------------------------------
 ' FICHERO --> QRT.C
 ' -------------------------------------------------
Declare Sub Tree_Walker(obj As OBJ_PTR , num As Integer) 
Declare Sub Parse_CL_Args(argv As string) 
Declare sub read_QRT_Scene(filein as string)


 ' -------------------------------------------------
 ' FICHERO --> RAY.C
 ' -------------------------------------------------
Declare Sub DiffColor(colors As SVECT_PTR , cinfo As CINFO_PTR , norm As VECT_PTR , locs As VECT_PTR , oline As OBJ_PTR) 
Declare Sub AmbColor(colors As SVECT_PTR , cinfo As CINFO_PTR) 
Declare Sub TransColor(colors As SVECT_PTR , cinfo As CINFO_PTR , norm As VECT_PTR , locs As VECT_PTR , lines As OBJ_PTR , inmult As Single) 
Declare Sub ReflectColor(colors As SVECT_PTR , cinfo As CINFO_PTR , norm As VECT_PTR , locs As VECT_PTR , lines As OBJ_PTR , inmult As Single) 
Declare Sub SkyColor(lines As OBJ_PTR , colors As SVECT_PTR) 
Declare Sub Dither(colors As SVECT_PTR , cinfo As CINFO_PTR) 
Declare Function Ray_Hit(CurrObj As OBJ_PTR , lines As OBJ_PTR ,byref MinT As Single , sflag As Short, fflag As Short , atten As VECT_PTR) As OBJ_PTR 
Declare Function Ray_Trace(lines As OBJ_PTR , colors As SVECT_PTR , multiplier As Single) As Integer 
Declare Sub PixelLine(x As Integer , y As Integer , lines As OBJ_PTR) 


 ' -------------------------------------------------
 ' FICHERO --> RELPOS.C
 ' -------------------------------------------------
Declare Sub Find_Plane_Coords(v1 As VECT_PTR , v2 As VECT_PTR , delta As VECT_PTR ,byref pos1 As Single  ,byref pos2 As Single  , t1 As Single , t2 As Single , t3 As Single) 
Declare Sub Plane_Pos(obj As OBJ_PTR , locs As VECT_PTR ,byref pos1 As Single ,byref pos2 As Single  , normalizes As Integer) 
Declare Sub Sphere_Pos(obj As OBJ_PTR , locs As VECT_PTR ,byref pos1 As Single ,byref pos2 As Single , normalizes As Integer=0) ' ultimo param NO se usa
Declare Sub Quadratic_Pos(obj As OBJ_PTR , locs As VECT_PTR ,byref pos1 As Single ,byref pos2 As Single , normalizes As Integer=0) ' ultimo param NO se usa


 ' -------------------------------------------------
 ' FICHERO --> RESIZE.C
 ' -------------------------------------------------
Declare Sub Resize_Sphere(obj As OBJ_PTR , mult As VECT_PTR) 
Declare Sub Resize_Plane(obj As OBJ_PTR , mult As VECT_PTR) 
Declare Sub Resize_Quadratic(obj As OBJ_PTR , mult As VECT_PTR) 
Declare Sub Resize_Bbox(obj As OBJ_PTR , mult As VECT_PTR) 


 ' -------------------------------------------------
 ' FICHERO --> STACK.C
 ' -------------------------------------------------
Declare Sub Do_Precomp(node As OBJ_PTR) 
Declare Sub Do_Precomp_Tree(node As OBJ_PTR) 
Declare Sub Make_Bbox(node As OBJ_PTR) 
Declare Sub add_lamp(objects As OBJ_PTR) 
Declare Sub Print_Obj(obj As OBJ_PTR) ' debug OBJ only 
Declare Function new_pat() As PATTERN_PTR
Declare Function new_line() As OBJ_PTR
Declare Function find_pat( names As string) As PATTERN_PTR 
Declare Function new_obj( types As Short , locs As VECT_PTR , v1 As VECT_PTR , v2 As VECT_PTR , v3 As VECT_PTR , _
							cinfo As CINFO_PTR , pattern As PATTERN_PTR , remove As PATTERN_PTR , names As string , _
							upper As VECT_PTR , lower As VECT_PTR , cterm As Single , xmult As Single , ymult As Single) As OBJ_PTR 

