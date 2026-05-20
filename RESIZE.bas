 
/'*********************************************************

      Routines to resize primitives by mx, my, mz
      (used for user defined primitive types).  The
      routines must also scale the position vector by
      that amount.  The scale factors are passed in as
      a vector (mult).

 *********************************************************'/

/'*********************************************************

   Resizes sphere.  If !(mult.x==mult.y==mult.z) will resize
   by smallest amount.

 *********************************************************'/

Sub Resize_Sphere(obj As OBJ_PTR , mult As VECT_PTR)
  Dim As Single size 

# ifdef ROBUST
    if (obj->types<>_SPHERE) Then Errors(INTERNAL_ERROR,1401) 
# endif

  size = MIN(MIN(mult->x,mult->y),mult->z) 

  VectorMult(@obj->locs, @obj->locs,mult) 
  obj->vect1.x *= size 
End Sub


/'*********************************************************

                 Resizes Planar objects

 *********************************************************'/

Sub Resize_Plane(obj As OBJ_PTR , mult As VECT_PTR)
  Dim As Single size 

# ifdef ROBUST
    if ((obj->types <> _PARALLELOGRAM) OrElse (obj->types <> _TRIANGLE) OrElse (obj->types <> _RING))=0 Then 
       Errors(INTERNAL_ERROR,1402)
    EndIf
# endif

  size = MIN(MIN(mult->x,mult->y),mult->z) 

  VectorMult(@obj->vect1, @obj->vect1,mult) 
  VectorMult(@obj->vect2, @obj->vect2,mult) 
  VectorMult(@obj->locs,  @obj->locs, mult) 

  obj->vect3.x *= size       /' for ring '/
  obj->vect3.y *= size 
End Sub


/'*********************************************************

                 Resizes Quadratic objects


   16 Mar 89 - fixed bug where mult was declared as float
               instead of VECT_PTR.
 *********************************************************'/

Sub Resize_Quadratic(obj As OBJ_PTR , mult As VECT_PTR)
# ifdef ROBUST
    if (obj->types<>_QUADRATIC) Then 
       Errors(INTERNAL_ERROR,1403)
    EndIf
# endif

  VectorMult(@obj->upper, @obj->upper,mult) 
  VectorMult(@obj->lower, @obj->lower,mult) 
  VectorMult(@obj->locs,  @obj->locs,  mult) 
End Sub


/'*********************************************************

    Resizes Bbox - doesn´t actually do anything, since
    bbox values are filled after the tree is created.

 *********************************************************'/

Sub Resize_Bbox(obj As OBJ_PTR , mult As VECT_PTR)
# ifdef ROBUST
    if (obj->types<>_BBOX) Then 
       Errors(INTERNAL_ERROR,1404)
    EndIf
# endif
End Sub


