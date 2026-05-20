 
/'*********************************************************

  Relative position module.  This is used for patterns:
  when an objects intersection is found, its location is
  passed to one of these routines, which returns two
  objects-relative coordinates.

 *********************************************************'/


/'*********************************************************

   Finds the coordinates of a point on a given plane.

   Added 11 Mar 89 to fix bug in finding plane coordinates.

 *********************************************************'/

Sub Find_Plane_Coords(v1 As VECT_PTR, v2 As VECT_PTR, delta As VECT_PTR, byref pos1 As Single, _
				byref pos2 As Single, t1 As Single, t2 As Single, t3 As Single)
  if (t1<> 0) Then 
    pos1 = (delta->x*v2->y-v2->x*delta->y)/t1 
  else
    if (t2<>0) Then 
      pos1 = (delta->x*v2->z-v2->x*delta->z)/t2 
    else
      if (t3<>0) Then 
        pos1 = (delta->y*v2->z-v2->y*delta->z)/t3 
      else
         Errors(ILLEGAL_OBJECT,700)
      EndIf
    EndIf
  EndIf
  
  if (v2->x <> 0) Then 
    pos2 = (delta->x-pos1*v1->x)/v2->x 
    exit sub 
  EndIf
  
  if (v2->y <> 0) Then 
    pos2 = (delta->y-pos1*v1->y)/v2->y 
    exit sub 
  EndIf
  
  if (v2->z <> 0) Then 
    pos2 = (delta->z-pos1*v1->z)/v2->z 
    exit sub 
  EndIf
 
  Errors(ILLEGAL_OBJECT,701) 
End Sub


/'*********************************************************

   Finds relative coords on plane given position in space.
   objects should be parallelogram or ring.
   loc is point in space.
   pos1, pos2 are set to relative coords.

   Changed 11 Mar 88 to use Find_Plane_Coords().

 *********************************************************'/

Sub Plane_Pos(obj As OBJ_PTR, locs As VECT_PTR, byref pos1 As Single, byref pos2 As Single, normalizes As Integer)
    Dim As VECTOR delta 

#ifdef ROBUST
      if ( ((obj->types = _RING) OrElse (obj->types = _PARALLELOGRAM) OrElse (obj->types = _RING) OrElse (obj->types = _TRIANGLE))=0) Then 
        Errors(INTERNAL_ERROR,702)
      EndIf
#endif

    VecSubtract(@delta,locs, @obj->locs) 

    Find_Plane_Coords(@obj->vect1,_
                       @obj->vect2,_
                       @delta,_
                       pos1,_
                       pos2,_
                       obj->precomp.sin1,_
                       obj->precomp.cos1,_
                       obj->precomp.sin2 _
                     ) 

    if (normalizes=0) Then 
      pos1 *= sqr(obj->precomp.len1) 
      pos2 *= sqr(obj->precomp.len2) 
    EndIf
End Sub


/'*********************************************************

   Finds relative coords on sphere given position in space
     obj->vect1.x = radius of sphere

 *********************************************************'/
' ultimo param no se usa, es por compatibilidad con llamadas punteros
Sub Sphere_Pos(obj As OBJ_PTR, locs As VECT_PTR, byref pos1 As Single, byref pos2 As Single, normalizes as integer = 0)
    Dim As VECTOR delta 

#ifdef ROBUST
      if (obj->types<>_SPHERE) Then Errors(INTERNAL_ERROR,703) 
#endif

    VecSubtract(@delta,locs, @obj->locs) 

    pos1 = atan2w(delta.x,delta.y) * obj->vect1.x 
    pos2 = atan2w(sqr(POW(delta.x)+POW(delta.y)),delta.z) * obj->vect1.x 
End Sub


/'*********************************************************

 Finds relative coords on quadratic given position in space

 *********************************************************'/
' ultimo param no se usa, es por compatibilidad con llamadas punteros
Sub Quadratic_Pos(obj As OBJ_PTR, locs As VECT_PTR, byref pos1 As Single, byref pos2 As Single, normalizes as integer = 0)
   Dim As VECTOR newpos 

   VecSubtract(@newpos,locs, @obj->locs) 

   pos1 = newpos.x /'* This isn´t right! *'/
   pos2 = newpos.y /'* fix it later      *'/
End Sub


