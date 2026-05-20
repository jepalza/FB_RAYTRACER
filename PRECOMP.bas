 /'*********************************************************

   Contains pre-computing routines for objects.  This
   is to get as much as possible out of the way so that
   the lines/objects intersection routines don´t have to
   do it.  Pre-computed stuff is stored in the objects
   precomp sub-structure.

 *********************************************************'/


/'*********************************************************

                   Precompute sphere

  Computes : n1 = POW(radius);

 *********************************************************'/

Sub PreCompSphere(obj As OBJ_PTR)
# ifdef ROBUST
    if (obj->types <> _SPHERE) Then Errors(INTERNAL_ERROR,901) 
# endif

  obj->precomp.n1 = POW(obj->vect1.x) 
End Sub


/'*********************************************************

                   Precompute Planar objects

  Computes : norm   = vect1 X vect2
             n1     = Dotprod (norm,loc)
             len1   = POW( | VECT1 | )
             len2   = POW( | VECT2 | )

  Changed 12 Mar 89 to add:

             sin1   = v1->x * v2->y - v1->y * v2->x;
             cos1   = v1->x * v2->z - v1->z * v2->x;
             sin2   = v1->y * v2->z - v1->z * v2->y;

 *********************************************************'/

Sub PreCompPlane(obj As OBJ_PTR)
# ifdef ROBUST
    if ((obj->types = _PARALLELOGRAM) OrElse (obj->types = _TRIANGLE) OrElse (obj->types = _RING))=0 Then 
      Errors(INTERNAL_ERROR,902)
    EndIf
# endif

  CrossProd(@obj->precomp.norm, @obj->vect1, @obj->vect2) 
  Normalize(@obj->precomp.norm) 

  obj->precomp.n1 = DOTPROD(obj->precomp.norm,obj->locs) 

  obj->precomp.len1 = DOTPROD(obj->vect1,obj->vect1) 
  obj->precomp.len2 = DOTPROD(obj->vect2,obj->vect2) 

  /' these precompute fields are not really sine and cos;
     they are just used to hold stuff for planes          '/

  obj->precomp.sin1 = obj->vect1.x * obj->vect2.y - _
                      obj->vect1.y * obj->vect2.x 

  obj->precomp.cos1 = obj->vect1.x * obj->vect2.z - _
                      obj->vect1.z * obj->vect2.x 

  obj->precomp.sin2 = obj->vect1.y * obj->vect2.z - _
                      obj->vect1.z * obj->vect2.y 

End Sub


/'*********************************************************

                   Precompute Quadratic

 *********************************************************'/

Sub PreCompQuadratic(obj As OBJ_PTR)
  Dim As VECTOR newdir 

# ifdef ROBUST
    if (obj->types <> _QUADRATIC) Then Errors(INTERNAL_ERROR,903) 
# endif

  Normalize(@obj->vect1) 

  if ((obj->vect1.x<>0) OrElse (obj->vect1.z<>0)) Then 
    obj->precomp.cos1 = obj->vect1.z / sqr(POW(obj->vect1.x) + POW(obj->vect1.z)) 
    obj->precomp.sin1 = sqr(1 - POW(obj->precomp.cos1)) 
  else
    obj->precomp.cos1 = 0
    obj->precomp.sin1 = 0 
  EndIf
  
  /' find new direction after first rotation '/
  newdir.x = obj->vect1.x *  obj->precomp.cos1 + _
             obj->vect1.z * -obj->precomp.sin1 

  newdir.y = obj->vect1.y 

  newdir.z = obj->vect1.x * obj->precomp.sin1 + _
             obj->vect1.z * obj->precomp.cos1 

  /' now do second rotation '/
  if ((newdir.y<>0) OrElse (newdir.z<>0)) Then 
    obj->precomp.cos2 = newdir.y / sqr(POW(newdir.y) + POW(newdir.z)) 
    obj->precomp.sin2 = sqr(1 - POW(obj->precomp.cos2)) 
  else
    obj->precomp.cos2 = 0
    obj->precomp.sin2 = 0 
  EndIf
  
End Sub


/'*********************************************************

      Null precomputing routine for all other objects

 *********************************************************'/

Sub PreCompNull(obj As OBJ_PTR)
	' no borrar
End Sub

