 
' #Include "qrt.bi"

/'*********************************************************

            Generates bounds for bounding box
            v1 = min x,y,z; v2 = max x,y,z

 *********************************************************'/

Sub BboxBbox(v1 As VECT_PTR , v2 As VECT_PTR , bbox As OBJ_PTR)
# ifdef ROBUST
    if (bbox->types<>_BBOX) Then Errors(INTERNAL_ERROR,101) 
# endif
  VectEQ(v1, @bbox->lower) 
  VectEQ(v2, @bbox->upper)
End Sub


/'*********************************************************

               Generates bounds for sphere
               v1 = min x,y,z; v2 = max x,y,z

 *********************************************************'/

Sub BboxSphere(v1 As VECT_PTR , v2 As VECT_PTR , sphere As OBJ_PTR)
# ifdef ROBUST
    if ( 0= (sphere->types=_SPHERE OrElse sphere->types=_LAMP)) Then 
      Errors(INTERNAL_ERROR,102)
    EndIf
# endif

  v1->x=sphere->locs.x-sphere->vect1.x 
  v2->x=sphere->locs.x+sphere->vect1.x 

  v1->y=sphere->locs.y-sphere->vect1.x 
  v2->y=sphere->locs.y+sphere->vect1.x 

  v1->z=sphere->locs.z-sphere->vect1.x 
  v2->z=sphere->locs.z+sphere->vect1.x 
End Sub


/'*********************************************************

               Generates bounds for parallelogram
               v1 = min x,y,z; v2 = max x,y,z

 *********************************************************'/

Sub BboxParallelogram(v1 As VECT_PTR , v2 As VECT_PTR , para As OBJ_PTR)
  dim as VECTOR point2,point3,point4 

# ifdef ROBUST
    if (para->types<>_PARALLELOGRAM) Then Errors(INTERNAL_ERROR,103) 
# endif

  VectorAdd(@point2, @para->locs, @para->vect1) 
  VectorAdd(@point3, @para->locs, @para->vect2) 
  VectorAdd(@point4, @point3    , @para->vect1) 

  v1->x=MIN(point2.x,point3.x) 
  v1->x=MIN(v1->x,point4.x) 
  v1->x=MIN(v1->x,para->locs.x) 

  v1->y=MIN(point2.y,point3.y) 
  v1->y=MIN(v1->y,point4.y) 
  v1->y=MIN(v1->y,para->locs.y) 

  v1->z=MIN(point2.z,point3.z) 
  v1->z=MIN(v1->z,point4.z) 
  v1->z=MIN(v1->z,para->locs.z) 

  v2->x=MAX(point2.x,point3.x) 
  v2->x=MAX(v2->x,point4.x) 
  v2->x=MAX(v2->x,para->locs.x) 

  v2->y=MAX(point2.y,point3.y) 
  v2->y=MAX(v2->y,point4.y) 
  v2->y=MAX(v2->y,para->locs.y) 

  v2->z=MAX(point2.z,point3.z) 
  v2->z=MAX(v2->z,point4.z) 
  v2->z=MAX(v2->z,para->locs.z) 
End Sub


/'*********************************************************

               Generates bounds for Triangle
               v1 = min x,y,z; v2 = max x,y,z

 *********************************************************'/

Sub BboxTriangle(v1 As VECT_PTR , v2 As VECT_PTR , obj As OBJ_PTR)
  dim as VECTOR point2,point3 

# ifdef ROBUST
    if (obj->types<>_TRIANGLE) Then Errors(INTERNAL_ERROR,104) 
# endif

  VectorAdd(@point2, @obj->locs, @obj->vect1) 
  VectorAdd(@point3, @obj->locs, @obj->vect2) 

  v1->x=MIN(point2.x,point3.x) 
  v1->x=MIN(v1->x,obj->locs.x) 

  v1->y=MIN(point2.y,point3.y) 
  v1->y=MIN(v1->y,obj->locs.y) 

  v1->z=MIN(point2.z,point3.z) 
  v1->z=MIN(v1->z,obj->locs.z) 

  v2->x=MAX(point2.x,point3.x) 
  v2->x=MAX(v2->x,obj->locs.x) 

  v2->y=MAX(point2.y,point3.y) 
  v2->y=MAX(v2->y,obj->locs.y) 

  v2->z=MAX(point2.z,point3.z) 
  v2->z=MAX(v2->z,obj->locs.z) 
End Sub

/'*********************************************************

  Generates bounds for ring. This routine is not quite
  right, and will generate a bounding box bigger than is
  really needed, but it will have to do until i have time
  to do it right.

               v1 = min x,y,z; v2 = max x,y,z

 *********************************************************'/

Sub BboxRing(v1 As VECT_PTR , v2 As VECT_PTR , ring As OBJ_PTR)
# ifdef ROBUST
    if (ring->types<>_RING) Then Errors(INTERNAL_ERROR,105) 
# endif

  v1->x = ring->locs.x-ring->vect3.y 
  v2->x = ring->locs.x+ring->vect3.y 

  v1->y = ring->locs.y-ring->vect3.y 
  v2->y = ring->locs.y+ring->vect3.y 

  v1->z = ring->locs.z-ring->vect3.y 
  v2->z = ring->locs.z+ring->vect3.y 
End Sub

/'*********************************************************

               Generates bounds for Quadratic
               v1 = min x,y,z; v2 = max x,y,z

     *** THIS IS NOT RIGHT FOR ROTATED QUADRATICS ***

 *********************************************************'/

Sub BboxQuadratic(v1 As VECT_PTR , v2 As VECT_PTR , quad As OBJ_PTR)
# ifdef ROBUST
    if (quad->types<>_QUADRATIC) Then Errors(INTERNAL_ERROR,106) 
# endif

  VectEQ(v1, @quad->lower) 
  VectEQ(v2, @quad->upper) 

  VectorAdd(v1,v1, @quad->locs) 
  VectorAdd(v2,v2, @quad->locs) 
End Sub


