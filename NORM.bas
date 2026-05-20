 
/'*********************************************************

   ** Normal finder ** - one proceedure here for each
   primitive type.  Functions are pointed at by entries
   in the ObjData structure. Some functions will find
   the normal for >1 objects.

 *********************************************************'/

/'*********************************************************

           Generates normal to sphere ( or lamp )

 *********************************************************'/

Sub SphereNorm(norm As VECT_PTR , objects As OBJ_PTR , position As VECT_PTR)
# ifdef ROBUST
    if  ((objects->types = _SPHERE) OrElse (objects->types = _LAMP))=0 Then 
      Errors(INTERNAL_ERROR,501)
    EndIf
# endif

  VecSubtract(norm,position, @objects->locs) 
  Normalize(norm) 
End Sub


/'*********************************************************

             Generates normal to planar objects

 *********************************************************'/

Sub PlaneNorm(norm As VECT_PTR , objects As OBJ_PTR , position As VECT_PTR)
# ifdef ROBUST
    if ((objects->types = _PARALLELOGRAM) OrElse (objects->types = _RING) OrElse (objects->types = _TRIANGLE))=0 Then 
      Errors(INTERNAL_ERROR,502)
    EndIf
# endif

  VectEQ(norm, @objects->precomp.norm)
End Sub


/'*********************************************************

          Generates normal to quadratic surface
                    ^         ^         ^
  Normal to { a*x*x i + b*y*y j + c*z*z k = d } is:
                    ^         ^         ^
            { 2*a*x i + 2*b*y j + 2*c*d k }

 *********************************************************'/

Sub QuadraticNorm(norm As VECT_PTR , objects As OBJ_PTR , position As VECT_PTR)
  dim as VECTOR newpos, newdir 

# ifdef ROBUST
    if (objects->types <> _QUADRATIC) Then 
      Errors(INTERNAL_ERROR,503)
    EndIf
# endif

  /' translate collision point '/

  VecSubtract(@newpos,position, @objects->locs) 

  if (objects->vect1.x = 0) AndAlso (objects->vect1.y = 1) AndAlso (objects->vect1.z = 0) Then 
      norm->x = objects->vect2.x * newpos.x  /' throw out factor of '/
      norm->y = objects->vect2.y * newpos.y  /' 2 because of normalization '/
      norm->z = objects->vect2.z * newpos.z 

      Normalize(norm)  /' here we must rotate '/
  else
    ' Rot12 y Rot21 opcionales si no se activa ROTATEFINISH en MTH.C
    Rot12(@newpos, @newpos,_                /' rotate position '/
           objects->precomp.cos1,_
           objects->precomp.sin1,_
           objects->precomp.cos2,_
           objects->precomp.sin2 ) 

    newdir.x = objects->vect2.x * newpos.x   /' normal '/
    newdir.y = objects->vect2.y * newpos.y 
    newdir.z = objects->vect2.z * newpos.z 

    Rot21(@newdir, norm,_                    /' rotate back '/
           objects->precomp.cos1,_
           objects->precomp.sin1,_
           objects->precomp.cos2,_
           objects->precomp.sin2 ) 
  EndIf
End Sub

