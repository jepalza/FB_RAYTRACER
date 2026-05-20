 /'*********************************************************

                  File for vector math

 *********************************************************'/



/'*********************************************************

    Returns pseudo random number between one and 100

 *********************************************************'/

Function PsRand() As Integer
  static As Integer vals=43 

  vals=(vals*73+97) 
  return(vals mod CNUM) 
End Function

/'*********************************************************
                                    -
                    Vector clear to 0

 *********************************************************'/

Sub VectEqZero(v As VECT_PTR)
  v->x=0.0 
  v->y=0.0 
  v->z=0.0 
End Sub


/'*********************************************************
                   -    -       -       -
     Vector math:  V1 = V1 + k2 V2 + k3 V3

 *********************************************************'/

Sub VectAddMult(v1 As VECT_PTR , k2 As Single , v2 As VECT_PTR , k3 As Single , v3 As VECT_PTR)
  v1->x+=k2*(v2->x)+k3*(v3->x) 
  v1->y+=k2*(v2->y)+k3*(v3->y) 
  v1->z+=k2*(v2->z)+k3*(v3->z) 
End Sub

/'*********************************************************
                   -      -
     Vector math:  V1 = - V2

 *********************************************************'/

Sub VectNegate(v1 As VECT_PTR , v2 As VECT_PTR)
  v1->x = -(v2->x) 
  v1->y = -(v2->y) 
  v1->z = -(v2->z) 
End Sub


/'*********************************************************
                   -    -    -
     Vector math:  V1 = V2 + V3

 *********************************************************'/

Sub VectorAdd(v1 As VECT_PTR , v2 As VECT_PTR , v3 As VECT_PTR)
  v1->x=v2->x+v3->x 
  v1->y=v2->y+v3->y 
  v1->z=v2->z+v3->z 
End Sub


/'*********************************************************
                   -    -    -
     Vector math:  V1 = V2 * V3

 *********************************************************'/

Sub VectorMult(v1 As VECT_PTR , v2 As VECT_PTR , v3 As VECT_PTR)
  v1->x=v2->x*v3->x 
  v1->y=v2->y*v3->y 
  v1->z=v2->z*v3->z 
End Sub


/'*********************************************************
                   -    -    -
     Vector math:  V1 = V2 - V3

 *********************************************************'/

Sub VecSubtract(v1 As VECT_PTR , v2 As VECT_PTR , v3 As VECT_PTR) /' 1=2-3 '/
  v1->x=v2->x-v3->x 
  v1->y=v2->y-v3->y 
  v1->z=v2->z-v3->z 
End Sub


/'*********************************************************
                   -      -
     Vector math:  V1 = k V1

 *********************************************************'/

Sub VectScale(v1 As VECT_PTR , k As Single ) /' 1=2*1 '/
  v1->x *= k 
  v1->y *= k 
  v1->z *= k 
End Sub


/'*********************************************************

                Color vector assignment

 *********************************************************'/

Sub SVectEQ(v1 As SVECT_PTR , v2 As SVECT_PTR) /' short vector v1=v2 '/
  v1->r=v2->r 
  v1->g=v2->g 
  v1->b=v2->b 
End Sub


/'*********************************************************

                    Vector assignment

 *********************************************************'/

Sub VectEQ(v1 As VECT_PTR , v2 As VECT_PTR)/' long vector v1=v2 '/
  v1->x=v2->x 
  v1->y=v2->y 
  v1->z=v2->z 
End Sub


/'*********************************************************

                  Normalize vector V

 *********************************************************'/

Sub Normalize(v As VECT_PTR)
  Dim As Single l 
  l=sqr(DOTPROD((*v),(*v))) 

  v->x/=l 
  v->y/=l 
  v->z/=l 
End Sub


/'*********************************************************
                   -    -    -
     Vector math:  V1 = V2 x V3

 *********************************************************'/

Sub CrossProd(v1 As VECT_PTR , v2 As VECT_PTR , v3 As VECT_PTR)
  v1->x=v2->y*v3->z-v2->z*v3->y 
  v1->y=v2->z*v3->x-v2->x*v3->z 
  v1->z=v2->x*v3->y-v2->y*v3->x 
End Sub


/'*********************************************************

      Returns position vector given line structure
      and parameter T

 *********************************************************'/
/' find position on parametric line 2 '/
Sub FindPos(pos_ As VECT_PTR , line2 As OBJ_PTR , t As Single)
# ifdef ROBUST
    if (line2->types<>_LINE) Then Errors(INTERNAL_ERROR,401) 
# endif

  pos_->x=line2->locs.x+line2->vect1.x*t 
  pos_->y=line2->locs.y+line2->vect1.y*t 
  pos_->z=line2->locs.z+line2->vect1.z*t 
End Sub

/'*********************************************************

        Reflects vector IN about NORM to produce OUT

 *********************************************************'/

Sub Reflect(outs As VECT_PTR , in As VECT_PTR , norm As VECT_PTR)
  Dim As Single dot 

  dot = -DOTPROD((*in),(*norm)) 

  VectEQ(outs,in) 
  VectAddMult(outs,dot,norm,dot,norm) 
End Sub


/'*********************************************************

   Arctan of 2 numbers.  This *should* exist in the
   compiler library, but it´s not there.
   MMT - modified to use the Turbo C library function atan2(..,..);
 **********************************************************'/

Function atan2w(x As Single , y As Single) As Single
  return(atan2(y,x)) 
End Function


/'*********************************************************

                    DOUBLE ROTATION

   This is a really two multiplied transformation
   matricies, but this form is faster. The if statements
   rotate only if necessary.

 *********************************************************'/

Sub Rot12(invect As VECT_PTR, outvect As VECT_PTR, cos1 As Single, sin1 As Single, cos2 As Single, sin2 As Single)

#ifdef ROTATEFINISH
  Dim As VECTOR tempdir 

  /'* Rotate once to YZ plane *'/
  if ((cos1<>0) OrElse (sin1<>0)) Then 
    tempdir.x = invect->x *  cos1 + invect->z * -sin1 
    tempdir.y = invect->y 
    tempdir.z = invect->x *  sin1 + invect->z *  cos1 
  else
    VectEQ(@tempdir,invect) 
  EndIf
  
  /'* Rotate again to positive Y *'/
  if ((cos2<>0) OrElse (sin2<>0)) Then 
    outvect->x = tempdir.x 
    outvect->y = tempdir.y * -cos2 + tempdir.z *  sin2 
    outvect->z = tempdir.y * -sin2 + tempdir.z * -cos2 
  else
    VectEQ(outvect,@tempdir) 
  EndIf
#endif
End Sub


/'*********************************************************

                    DOUBLE ROTATION

       Same as above, but in reverse direction

 *********************************************************'/

Sub Rot21(invect As VECT_PTR, outvect As VECT_PTR, cos1 As Single, sin1 As Single, cos2 As Single, sin2 As Single)

#ifdef ROTATEFINISH
  dim as VECTOR tempdir 

  /'* Rotate once from Y up direction *'/
  if ((cos2<>0) OrElse (sin2<>0)) Then 
    tempdir.x = invect->x 
    tempdir.y = invect->y *  cos2 + invect->z * -sin2 
    tempdir.z = invect->y *  sin2 + invect->z *  cos2 
  else
    VectEQ(@tempdir,invect) 
  EndIf
  

  /'* Rotate again to arbitrary direction *'/
  if ((cos1<>0) OrElse (sin1<>0)) Then 
    outvect->x = tempdir.x * -cos1 + tempdir.z *  sin1 
    outvect->y = tempdir.y 
    outvect->z = tempdir.x * -sin1 + tempdir.z * -cos1 
  else
    VectEQ(outvect,@tempdir) 
  EndIf
  
#endif
End Sub

