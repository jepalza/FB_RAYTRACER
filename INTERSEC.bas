 
/'*********************************************************

  Line/Object intersection routines.  Routine returns
  TRUE if objects is hit, along with the nearest of the
  possibly multiple intersections.  First parameter is
  lines, second is objects, and third is pointer to parameter
  on lines for intersection (filled by routine).  Functions
  are pointed at by entries in ObjData structure.

 *********************************************************'/


/'*********************************************************

          Line/bounding box intersection test.

   Looks bad, but its pretty fast.  Many times we don´t
   even have to go through the whole routine if a miss
   can be detected early.

 *********************************************************'/

Function LineBbox(lines As OBJ_PTR , bbox As OBJ_PTR ,byref t As Single) As Integer
  Dim As Single tminx, tmaxx, tminy, tmaxy, tminz, tmaxz, tmin, tmax, t1,t2 

  /' this number is not really important, since nothing
     cares about the exact location of the intersection '/
  t=10 

# ifdef ROBUST
    if (lines->types<>_LINE) Then Errors(INTERNAL_ERROR,301) 
    if  (bbox->types<>_BBOX) Then Errors(INTERNAL_ERROR,302) 
# endif

  if (abs(lines->vect1.x) < SMALL) Then 
    if ((bbox->lower.x < lines->locs.x) AndAlso (bbox->upper.x > lines->locs.x)) Then 
        tminx = -BIG: tmaxx=BIG 
    else
        return(FALSE)
    EndIf
  else
    t1 = (bbox->lower.x-lines->locs.x)/lines->vect1.x 
    t2 = (bbox->upper.x-lines->locs.x)/lines->vect1.x 
    tminx = MIN(t1,t2) 
    tmaxx = MAX(t1,t2) 
    if (tmaxx<0) Then return(FALSE) 
  EndIf
  
  if (abs(lines->vect1.y) < SMALL) Then 
    if ((bbox->lower.y < lines->locs.y) AndAlso (bbox->upper.y > lines->locs.y)) Then 
        tminy = -BIG: tmaxy=BIG 
    else
        return(FALSE)
    EndIf
  else
    t1 = (bbox->lower.y-lines->locs.y)/lines->vect1.y 
    t2 = (bbox->upper.y-lines->locs.y)/lines->vect1.y 
    tminy = MIN(t1,t2) 
    tmaxy = MAX(t1,t2) 
    if (tmaxy<0) Then return(FALSE) 
  EndIf
  
  if (abs(lines->vect1.z) < SMALL) Then 
    if ((bbox->lower.z < lines->locs.z) AndAlso (bbox->upper.z > lines->locs.z)) Then 
        tminz = -BIG: tmaxz=BIG 
    else
        return(FALSE)
    EndIf
  else
    t1 = (bbox->lower.z-lines->locs.z)/lines->vect1.z 
    t2 = (bbox->upper.z-lines->locs.z)/lines->vect1.z 
    tminz = MIN(t1,t2) 
    tmaxz = MAX(t1,t2) 
    if (tmaxz<0) Then return(FALSE) 
  EndIf
  
  tmin = MAX(MAX(tminx,tminy),tminz) 
  tmax = MIN(MIN(tmaxx,tmaxy),tmaxz) 

  if (tmax<0) Then return(FALSE) 

  if (tmax<tmin) Then return(FALSE) 

  return(TRUE) 
End Function

/'*********************************************************

               Line/ring intersection test

     Similar to, but slower than parallelogram test

     Changed 11 Mar 89 to fix bug with planar position
     computation.  Now uses Plane_Pos(), which is a
     little slower than before.

 *********************************************************'/

Function LineRing(lines As OBJ_PTR , ring As OBJ_PTR ,byref t As Single) As Integer
  dim as VECTOR locs 
  Dim As Single dot, rad 
  Dim As Single pos1, pos2 

# ifdef ROBUST
    if (lines->types<>_LINE) Then Errors(INTERNAL_ERROR,303) 
    if (ring->types<>_RING) Then Errors(INTERNAL_ERROR,304) 
# endif

  dot = DOTPROD(ring->precomp.norm,lines->vect1) 

  if (Abs(dot)<SMALL) Then return(FALSE) 

  pos1 = ring->precomp.n1 
  pos2 = DOTPROD(ring->precomp.norm,lines->locs) 

  t=(pos1-pos2)/dot 

  FindPos(@locs,lines,t) 
  Plane_Pos(ring,@locs,pos1,pos2,TRUE) 

  rad = sqr(POW(pos1)+POW(pos2)) 

  if (rad<ring->vect3.x) OrElse (rad>ring->vect3.y) Then 
     return(FALSE)
  EndIf

  return(TRUE) 
End Function


/'*********************************************************

        Line/Parallelogram intersection test
        Returns Parameter T for intersection

        Changed 11 Mar 89 to fix bug with planar position
        computation.  Now uses Plane_Pos(), which is a
        little slower than before.

 *********************************************************'/

Function LineParallelogram(lines As OBJ_PTR , para As OBJ_PTR , byref t As Single) As Integer
  Dim As VECTOR  locs 
  Dim As Single   dot, in1, in2 

# ifdef ROBUST
    if (lines->types<>_LINE) Then Errors(INTERNAL_ERROR,305) 
    if (para->types<>_PARALLELOGRAM) Then Errors(INTERNAL_ERROR,306) 
# endif

  dot = DOTPROD((para->precomp.norm),lines->vect1) 

  if (Abs(dot)<SMALL) Then return(FALSE) 

  in1 = para->precomp.n1 
  in2 = DOTPROD(para->precomp.norm,lines->locs) 

  t=(in1-in2)/dot 

  FindPos(@locs,lines,t) 
  Plane_Pos(para,@locs,in1,in2,TRUE) 

  if ((in1>=0) AndAlso (in2>=0) AndAlso (in1<=1) AndAlso (in2<=1))=0 Then 
    return(FALSE)
  EndIf
  
  return(TRUE) 
End Function


/'*********************************************************

        Line/Triangle intersection test
        Returns Parameter T for intersection

        Changed 11 Mar 89 to fix bug with planar position
        computation. Now uses Plane_Pos(), which is a
        little slower than before.

 *********************************************************'/

Function LineTriangle(lines As OBJ_PTR , obj As OBJ_PTR , byref t As Single) As Integer
  Dim As VECTOR locs 
  Dim As Single dot 
  Dim As Single in1, in2 

  dot = DOTPROD(obj->precomp.norm,lines->vect1) 

  if (Abs(dot)<SMALL) Then return(FALSE) 

  in1 = obj->precomp.n1 
  in2 = DOTPROD(obj->precomp.norm,lines->locs) 

  t=(in1-in2)/dot 

  FindPos(@locs,lines,t) 
  Plane_Pos(obj,@locs,in1,in2,TRUE) 

  if ((in1>=0) AndAlso (in2>=0) AndAlso (in1+in2<=1))=0 Then 
    return(FALSE)
  EndIf
  
  return(TRUE) 
End Function


/'*********************************************************

           Line/sphere intersection test
        Returns parameter T for intersection

 *********************************************************'/

Function LineSphere(lines As OBJ_PTR , sph As OBJ_PTR ,byref t As Single) As Integer
  Dim As Single a,b,c,d,t1, tmpx,tmpy,tmpz 

# ifdef ROBUST
    if (lines->types<>_LINE) Then Errors(INTERNAL_ERROR,309) 
    if ((sph->types=_SPHERE) OrElse (sph->types=_LAMP))=0 Then 
      Errors(INTERNAL_ERROR,310)
    EndIf
# endif

  tmpx = sph->locs.x-lines->locs.x 
  tmpy = sph->locs.y-lines->locs.y 
  tmpz = sph->locs.z-lines->locs.z 

  c = POW(tmpx)+ POW(tmpy)+ POW(tmpz) - (sph->precomp.n1) 

  b = -2*(lines->vect1.x*tmpx+ _                     /' find b '/
          lines->vect1.y*tmpy+ _
          lines->vect1.z*tmpz) 

  a = POW(lines->vect1.x)+ _                         /' find a '/
      POW(lines->vect1.y)+ _
      POW(lines->vect1.z) 

  d = POW(b)-4.0*a*c 

  if (d<=0) Then return(FALSE)                           /' does sphere hit? '/

  d=sqr(d):   t=(-b+d)/(a+a) 
             t1=(-b-d)/(a+a) 

  if (t1<t) AndAlso (t1>SMALL) Then t=t1                      /' find 1st collision '/

  if (t > SMALL) Then  return(TRUE) 

  return(FALSE) 
End Function


/'*********************************************************

        Line/quadratic intersection test
        Returns parameter T for intersection

  newline is the input lines translated and rotated so that
  the quadratic is @ 0,0,0 and pointed up.

 *********************************************************'/

Function LineQuadratic(lines As OBJ_PTR , quad As OBJ_PTR , byref  t As Single) As Integer
  Dim As Single a,b,c,d, t1 
  Dim As VECTOR locs, loc1, tempdir 
  Dim As OBJ_STRUCT newline 

# ifdef ROBUST
    if (lines->types<>_LINE) Then Errors(INTERNAL_ERROR,311) 
    if (quad->types<>_QUADRATIC) Then Errors(INTERNAL_ERROR,312) 
# endif

  newline.types=_LINE 

  /'** translation transformation to newpos for lines **'/
  VecSubtract(@newline.locs, @lines->locs, @quad->locs) 

  if ((quad->vect1.x = 0) AndAlso (quad->vect1.y = 1) AndAlso (quad->vect1.z = 0)) Then 
    VectEQ(@newline.vect1, @lines->vect1) /' here we must rot '/
  else
    Rot12(@lines->vect1, @newline.vect1,_    /' rot view direction '/
           quad->precomp.cos1,_
           quad->precomp.sin1,_
           quad->precomp.cos2,_
           quad->precomp.sin2 ) 

    Rot12(@newline.locs, @newline.locs,_      /' rotate view location '/
           quad->precomp.cos1,_
           quad->precomp.sin1,_
           quad->precomp.cos2,_
           quad->precomp.sin2 ) 
  EndIf

  c = -(quad->cterm) + quad->vect2.x * POW(newline.locs.x) + _
                       quad->vect2.y * POW(newline.locs.y) + _
                       quad->vect2.z * POW(newline.locs.z) 

  b = 2*( quad->vect2.x * newline.locs.x * newline.vect1.x + _
          quad->vect2.y * newline.locs.y * newline.vect1.y + _
          quad->vect2.z * newline.locs.z * newline.vect1.z) 

  a = quad->vect2.x * POW(newline.vect1.x) + _
      quad->vect2.y * POW(newline.vect1.y) + _
      quad->vect2.z * POW(newline.vect1.z) 

  d = POW(b)-4.0*a*c 

  if (d<0) Then return(FALSE)                            /' we missed it '/


  d=sqr(d):  t=(-b+d)/(a+a) 
             t1=(-b-d)/(a+a) 

  FindPos(@locs,@newline,t)                         /' find locations '/
  FindPos(@loc1,@newline,t1) 

  if (locs.x < quad->lower.x) OrElse (locs.x > quad->upper.x) _
		OrElse (locs.y < quad->lower.y) OrElse (locs.y > quad->upper.y) _
		OrElse (locs.z < quad->lower.z) OrElse (locs.z > quad->upper.z) Then t = -1 

  if (loc1.x < quad->lower.x) OrElse (loc1.x > quad->upper.x) _
		OrElse (loc1.y < quad->lower.y) OrElse (loc1.y > quad->upper.y) _
		OrElse (loc1.z < quad->lower.z) OrElse (loc1.z > quad->upper.z) Then t1 = -1 
		
  if (t<=SMALL) AndAlso (t1<=SMALL) Then return(FALSE) 

  if (t<=SMALL) AndAlso (t1>SMALL) Then t=t1 
  if (t1<t) AndAlso (t1>SMALL) Then t=t1    /' find 1st collision '/

  if (t>SMALL) Then return(TRUE) 

  return(FALSE) 
End Function

