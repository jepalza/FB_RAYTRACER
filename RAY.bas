 /'***********************************************************

                     ray trace module

 ***********************************************************'/


/'*********************************************************

  Diffuse colors module. Also compute specular reflections
  here for efficiency sake - only one lamp eneumerator
  needed that way.

  - Changed 12 Mar 89 to fix bug where sreflect instead of
    reflect was being tested.  This caused the ray tracer
    to go much slower than needed!!

 *********************************************************'/

Sub DiffColor(colors As SVECT_PTR , cinfo As CINFO_PTR , norm As VECT_PTR , locs As VECT_PTR , oline As OBJ_PTR)
  Dim As OBJ_PTR lamp, lines, CurrObj
  Dim As Single t 
  Dim As Single dist, t1,t2 
  Dim As VECTOR refl, atten 

  lines=new_line() 
  VectEQ(@lines->locs,locs) 
  lamp=THEWORLD.lamps 

  while (lamp<>NULL)  
#ifdef ROBUST
      if (lamp->types<>_LAMP) Then Errors(INTERNAL_ERROR,601) 
#endif

    /' find dir to lamp '/
    VecSubtract(@lines->vect1, @lamp->locs, locs) 

    CurrObj=NULL 
    atten.x = 1.00
	atten.y = 1.00
	atten.z = 1.00 

    if (def.shadow=TRUE) Then /' set light attenuation factor '/
      CurrObj=Ray_Hit(THEWORLD.stack,lines,t,TRUE,TRUE,@atten) 
      THEWORLD.to_lamp+=1  
    EndIf
  
    if (CurrObj=NULL) Then  /' hit nothing ? '/
      dist = DOTPROD(lines->vect1,lines->vect1) 
      Normalize(@lines->vect1) 

      if (cinfo->diff.r>0) OrElse (cinfo->diff.g>0) OrElse (cinfo->diff.b>0) Then 
        t1 = DOTPROD((*norm),lines->vect1) 

        if (t1>0) Then 
          t2 = lamp->vect1.y*t1/sqr(dist) 

          colors->r+=int(lamp->cinfo.amb.r * atten.x * t2*cinfo->diff.r/CNUM) 
          colors->g+=int(lamp->cinfo.amb.g * atten.y * t2*cinfo->diff.g/CNUM) 
          colors->b+=int(lamp->cinfo.amb.b * atten.z * t2*cinfo->diff.b/CNUM) 
        EndIf
      EndIf
  
      if (cinfo->reflect>0) Then  /' specular '/
        lines->vect1.x = -lines->vect1.x         /' reverse lines '/
        lines->vect1.y = -lines->vect1.y 
        lines->vect1.z = -lines->vect1.z 
        Reflect(@refl, @lines->vect1,norm) 
        t1 = -DOTPROD(refl,oline->vect1) 
        if (t1>SMALL) Then  /' this is slow !! '/
          t2 = (t1 ^ cinfo->sreflect) * lamp->vect1.y/sqr(dist) 
          t2 *= cinfo->reflect/CNUM 
          colors->r+=int(t2 * atten.x * lamp->cinfo.amb.r) 
          colors->g+=int(t2 * atten.y * lamp->cinfo.amb.g) 
          colors->b+=int(t2 * atten.z * lamp->cinfo.amb.b) 
        EndIf
      EndIf
    EndIf
  
    lamp=lamp->nextobj 
 Wend
    
  delete(lines) 
End Sub


/'*********************************************************

                     Ambient colors module

   Really simple - just add the colors, no questions asked.

 *********************************************************'/

Sub AmbColor(colors As SVECT_PTR , cinfo As CINFO_PTR)
  colors->r+=int(cinfo->amb.r*cinfo->diff.r/CNUM) 
  colors->g+=int(cinfo->amb.g*cinfo->diff.g/CNUM) 
  colors->b+=int(cinfo->amb.b*cinfo->diff.b/CNUM) 
End Sub


/'*********************************************************

                 Transmitted colors module

 This is hairy and slow - bends ray around normal vector
 by ratio of indicies of refraction.  lines->flag=TRUE
 if we are inside a piece of glass - this means glass must
 have a simple structure.

 10 Aug 88 - Fixed TransColor so that it works.  The
 original algorithm came from a book, and I had some
 problems getting it to work.  This one came from my head,
 and seems to work ok.

 11 Aug 88 - Added density effects (light is reduced
 more by thicker glass).

 *********************************************************'/

/' #define TESTTRANS TRUE '/

Sub TransColor(colors As SVECT_PTR , cinfo As CINFO_PTR , norm As VECT_PTR , locs As VECT_PTR , _
				lines As OBJ_PTR , inmult As Single)
  Dim As OBJ_STRUCT newline 
  Dim As SVECTOR    col1 
  Dim As VECTOR     toadd, negvect1 
  Dim As Single     index2, multiplier, side1 
  Dim As Integer    maxtrans 

  /' these vars are used to keep track of the distance
     traveled through the glass so we can attenuate the
     light accordingly.                                 '/

  static As VECTOR  prevpos 

  Dim As VECTOR     displacement 
  Dim As SVECTOR    attenuation 
  Dim As Single     glassdist 

  if ((cinfo->transp.r < def.ithreshold) AndAlso (cinfo->transp.g < def.ithreshold) _
			AndAlso (cinfo->transp.b < def.ithreshold)) Then return 

# ifdef ROBUST
    if (lines->types<>_LINE) Then Errors(INTERNAL_ERROR,602) 
    if ((THEWORLD.globindex = 0) OrElse (cinfo->index = 0)) Then 
          Errors(ZERO_INDEX,603)
    EndIf
# endif

  if (lines->flag) Then 
    /' inside an objects going out ? '/
    index2 = cinfo->index/THEWORLD.globindex 

    /' find offset from prev position '/
    VecSubtract(@displacement,locs,@prevpos) 

    /' measure distance '/
    glassdist = sqr(DOTPROD(displacement,displacement)) 
  else
    /' outside an objects going in ? '/
    index2 = THEWORLD.globindex/cinfo->index 
    /' remember this position '/
    VectEQ(@prevpos,locs) 
  EndIf
  

  /' doesn´t currently use new_line() call '/
  newline.types = _LINE 
  newline.child   = NULL
  newline.nextobj = NULL 

  /' this ray starts from where we hit glass '/
  VectEQ(@newline.locs,locs) 
  VectEqZero(@toadd) 

  VectNegate(@negvect1, @lines->vect1) 
  side1 = DOTPROD((*norm),negvect1) 

  VectAddMult(@toadd,-side1,norm,1,@negvect1) 

  VectScale(@toadd,1.0-index2) 

  VectorAdd(@newline.vect1, @lines->vect1,@toadd) 

  Normalize(@newline.vect1) 

  maxtrans = MAX(MAX(cinfo->transp.r,cinfo->transp.g), cinfo->transp.b) 

  /' if we were inside, now we´re out '/
  newline.flag = iif(lines->flag=0,1,0) ' revisar

  multiplier = inmult * maxtrans/CNUM 
  Ray_Trace(@newline,@col1,multiplier) 
  THEWORLD.refl_trans+=1  

  colors->r += col1.r 
  colors->g += col1.g 
  colors->b += col1.b 

  if (lines->flag) Then /' density effects? '/
    /' density uses x,y,z not r,g,b cuz its a floating
       point vector.                                   '/
    attenuation.r = int(cinfo->density.x * glassdist * (colors->r)) 
    attenuation.g = int(cinfo->density.y * glassdist * (colors->g)) 
    attenuation.b = int(cinfo->density.z * glassdist * (colors->b)) 

    /' don´t remove more than original intensity! '/
    colors->r -= MIN(attenuation.r,colors->r) 
    colors->g -= MIN(attenuation.g,colors->g) 
    colors->b -= MIN(attenuation.b,colors->b) 
  EndIf
  
End Sub


/'*********************************************************

                   Reflective colors module

      Bounce ray off objects and recursively ray trace.

 *********************************************************'/

Sub ReflectColor(colors As SVECT_PTR, cinfo As CINFO_PTR ,norm As VECT_PTR ,_
				locs As VECT_PTR ,lines As OBJ_PTR , inmult As Single)
  Dim As SVECTOR col1 
  Dim As OBJ_STRUCT newline 
  Dim As Single multiplier 
  Dim As Integer maxmirror 

# ifdef ROBUST
    if (lines->types<>_LINE) Then Errors(INTERNAL_ERROR,604) 
# endif

  if ((cinfo->mirror.r < def.ithreshold) AndAlso (cinfo->mirror.g < def.ithreshold) _
			AndAlso (cinfo->mirror.b < def.ithreshold)) Then return 

  newline.types = _LINE 

  newline.child = NULL
  newline.nextobj = NULL 

  VectEQ(@newline.locs,locs) 
  Reflect(@newline.vect1, @lines->vect1,norm) 

  maxmirror = MAX(MAX(cinfo->mirror.r,cinfo->mirror.g), cinfo->mirror.b) 

  multiplier = inmult * maxmirror/CNUM 

  Ray_Trace(@newline,@col1,multiplier) 
  THEWORLD.refl_trans+=1  

  colors->r+=col1.r*cinfo->mirror.r/CNUM 
  colors->g+=col1.g*cinfo->mirror.g/CNUM 
  colors->b+=col1.b*cinfo->mirror.b/CNUM 
End Sub


/'*********************************************************

   Computes sky colors given lines - interpolate between
   horizon and zenith to find colors (user should dither
   the explitive out of the sky to compensate for lack
   of colors resolution.

 *********************************************************'/

Sub SkyColor(lines As OBJ_PTR , colors As SVECT_PTR)
  Dim As Single length, horiz, zenith 

# ifdef ROBUST
    if (lines->types<>_LINE) Then Errors(INTERNAL_ERROR,605) 
# endif

  if (THEWORLD.sky=NULL) Then return 

# ifdef ROBUST
    if (THEWORLD.sky->types <> _SKY) Then 
      Errors(INTERNAL_ERROR,606)
    EndIf
# endif

  length = DOTPROD(lines->vect1,lines->vect1) 

  zenith =  POW(lines->vect1.y)/length 
  horiz  = (POW(lines->vect1.x)+POW(lines->vect1.z))/length 

  colors->r += int(zenith*THEWORLD.skycolor_zenith.r+horiz *THEWORLD.skycolor_horiz.r) 
  colors->g += int(zenith*THEWORLD.skycolor_zenith.g+horiz *THEWORLD.skycolor_horiz.g) 
  colors->b += int(zenith*THEWORLD.skycolor_zenith.b+horiz *THEWORLD.skycolor_horiz.b) 

  Dither(colors, @THEWORLD.sky->cinfo) 
End Sub


/'*********************************************************

  Color dithering rountine - negative dither number will
  dither all three colors together - positive with perform
  separate colors dithering on all colors.

 *********************************************************'/

#define MINCOL 10
Sub Dither(colors As SVECT_PTR , cinfo As CINFO_PTR)
  Dim As Integer r,g,b  

  if (cinfo->dither=0) Then return 

  if (cinfo->dither>0) Then 
    r=PsRand(): g=PsRand(): b=PsRand() 

    if (colors->r < MINCOL) Then r=ABS(r) 
    if (colors->g < MINCOL) Then g=ABS(g) 
    if (colors->b < MINCOL) Then b=ABS(b) 

    colors->r += int(r*cinfo->dither/CNUM) 
    colors->g += int(g*cinfo->dither/CNUM) 
    colors->b += int(b*cinfo->dither/CNUM) 
  else
    r=PsRand() 
    if ((colors->r+colors->g+colors->b)>(3*MINCOL)) Then r=ABS(r) 

    colors->r += int(r*cinfo->dither/CNUM) 
    colors->g += int(r*cinfo->dither/CNUM) 
    colors->b += int(r*cinfo->dither/CNUM) 
  EndIf
  
End Sub


/'*********************************************************

   Returns pointer to objects hit by ray.
   Parameters:
     CurrObj = root of objects tree
     lines    = light ray
     MinT    = parameter T for lines/obj intersection
     sflag   = if TRUE, stop on first intersection
     fflag   = ALWAYS pass TRUE here.
     pos1    = position vector for objects relative coords
     pos2    = position vector for objects relative coords

  - changed 11 jun 88 to fix shadow routine -

  - changed 12 aug 88 to add light attenuation by glass.
            this is not done correctly, but is better
            than nothing.

 *********************************************************'/

Function Ray_Hit(CurrObj As OBJ_PTR , lines As OBJ_PTR , byref MinT As Single , sflag As Short, _
					fflag As Short , atten As VECT_PTR) As OBJ_PTR
  static As OBJ_PTR MinObj 
  Dim As VECTOR locs 
  Dim As OBJ_PTR obj 
  Dim As Short collision 
  static As Short stops 
  Dim As Single t 

  obj = CurrObj 

  if (fflag) Then 
	#ifdef ROBUST
		if (lines->types<>_LINE) Then Errors(INTERNAL_ERROR,607) 
	#endif
    MinT=BIG 
    MinObj=NULL 
    stops=FALSE 
  EndIf

  while (obj<>NULL) AndAlso (stops=0)           /' check for objects collisions '/
    collision = ObjData(obj->types).ColTest(lines,obj,t) ' revisar 
    if (collision<>0) AndAlso (obj->remove <> NULL) Then 
      FindPos(@locs,lines,t) 
      if (Find_Color(obj,obj->remove,@locs,NULL, 1.0, 1.0)) Then 
        collision = FALSE
      EndIf
    EndIf
  
    THEWORLD.intersect_tests+=1  

	if (collision<>0) AndAlso (t>SMALL) Then  /' did we hit something ? '/
		if (obj->types<>_BBOX) Then  /' if not _BBOX '/
			THEWORLD.ray_intersects+=1  
			if (sflag<>0) AndAlso (t<1) Then 
				/' did we hit a transparent objects? '/
				/' PS - this is not right - fix it later '/
				if (obj->cinfo.transp.r < def.ithreshold) _
						AndAlso (obj->cinfo.transp.g < def.ithreshold) _
						AndAlso (obj->cinfo.transp.b < def.ithreshold) Then 
					stops  = TRUE 
					MinObj = obj 
					return(obj) 
				else 
					/' attenuate light if transparent objects.  This is
					   REALLY screwy and not at all correct, but it is
					   the only simple way to do it.
					'/
					if (atten <> NULL) Then 
						atten->x *= POW(obj->cinfo.transp.r / CNUM) 
						atten->y *= POW(obj->cinfo.transp.g / CNUM) 
						atten->z *= POW(obj->cinfo.transp.b / CNUM) 
					EndIf
				EndIf
			EndIf

			if (sflag=0) AndAlso (t<MinT) Then  /' nearest collision ? '/
				MinT  = t   /' if so, save it '/
				MinObj = obj 
			EndIf   
		Else /' is bbox hit '/
				THEWORLD.bbox_intersects+=1 
				Ray_Hit(obj->child,lines,MinT,sflag,FALSE,atten)    
		EndIf
	EndIf
  
    obj=obj->nextobj 
  Wend
   
  return(MinObj) 
End Function


/'*********************************************************

  Performs ray tracing in lines, fills colors structure.
  Multiplier is a number by which the colors output will
  be mulplied (0..1) so that we can tell when its useless
  to continue recursivly tracing rays.

 *********************************************************'/

Function Ray_Trace(lines As OBJ_PTR , colors As SVECT_PTR , multiplier As Single) As Integer
  Dim As Single MinT, divisor 
  Dim As OBJ_PTR MinObj 
  Dim As CINFOS cinfo 
  Dim As VECTOR MinLoc, MinNorm 

# ifdef ROBUST
    if (lines->types<>_LINE) Then Errors(INTERNAL_ERROR,608) 
# endif

  colors->r = 0
  colors->g = 0
  colors->b = 0 

  /' check here if so little light is added that it doesn´t matter '/
  if (multiplier < def.threshold) Then return(FALSE) 

  MinObj=Ray_Hit(THEWORLD.stack,lines,MinT,FALSE,TRUE,NULL) 

  if (MinObj<>NULL) Then 
    FindPos(@MinLoc,lines,MinT) 

    /'  Find objects normal vector '/
    ObjData(MinObj->types).FindNorm(@MinNorm,MinObj,@MinLoc) ' revisar 

    if (DOTPROD(MinNorm,lines->vect1) >0) Then  /' reverse   '/
      MinNorm.x = -MinNorm.x /' normal if '/
      MinNorm.y = -MinNorm.y /' necessary '/
      MinNorm.z = -MinNorm.z /' Find colinfo '/
    EndIf

    Find_Color(MinObj,_
               MinObj->pattern,_
               @MinLoc,_
               @cinfo,_
               MinObj->xmult,_
               MinObj->ymult ) 

    AmbColor(colors,@cinfo)
    DiffColor(colors,@cinfo,@MinNorm,@MinLoc,lines) 
    TransColor(colors,@cinfo,@MinNorm,@MinLoc,lines,multiplier) 
    ReflectColor(colors,@cinfo,@MinNorm,@MinLoc,lines,multiplier) 
    Dither(colors,@cinfo) 
  else
      SkyColor(lines,colors) 
  EndIf
  

  if (colors->r > CNUM) OrElse (colors->g > CNUM) OrElse (colors->b > CNUM) Then 
    divisor = MAX(MAX(colors->r,colors->g),colors->b) / CNUM 

    colors->r = colors->r/divisor 
    colors->g = colors->g/divisor 
    colors->b = colors->b/divisor 
  EndIf
  
  return iif(MinObj<>NULL,1,0) ' revisar
End Function


/'*********************************************************

     Generates lines for a given x,y pixel position
     This fn needs a little work, as currently it
     produces some distortion around the edge of
     the screen.

 *********************************************************'/

Sub PixelLine(x As Integer , y As Integer , lines As OBJ_PTR)
  Dim As Single xf, yf 

# ifdef ROBUST
    if (lines->types<>_LINE) Then Errors(INTERNAL_ERROR,609) 
# endif

  xf=(def.x_center-x)/THEWORLD.x_divisor 
  yf=(def.y_center-y)/THEWORLD.y_divisor 

  VectEQ(@lines->locs , @THEWORLD.observer->locs) 
  VectEQ(@lines->vect1, @THEWORLD.observer->vect1) 
  VectAddMult(@lines->vect1,xf, @THEWORLD.obsright, yf, @THEWORLD.obsup) 

  lines->flag = FALSE            /' redundant? '/
End Sub


/'*********************************************************

                  Ray trace whole screen

 *********************************************************'/

Sub Screen_Trace() 
  Dim As Integer x,y,c
  Dim As Short rbyte(MAX_XRES*2), gbyte(MAX_XRES*2), bbyte(MAX_XRES*2)  ' jepalza, *2 para que no de error en modos super_alta_res 1920x1080
  Dim As SVECTOR colors 
  Dim As OBJ_PTR lines 

  lines=new_line() 

  c=csrlin
  for y=0 To def.y_res-1         
    for x=0 To def.x_res-1         
      PixelLine(x,y,lines) 

      Ray_Trace(lines,@colors,1.0) 
      THEWORLD.primary_traced+=1  

      rbyte(x) = colors.r 
      gbyte(x) = colors.g 
      bbyte(x) = colors.b 
    Next

    locate 1,1:Print "Trazando Linea:";y
    Dump_Line_BMP(y,rbyte(),gbyte(),bbyte()) 
  Next

  delete(lines) 
End Sub
