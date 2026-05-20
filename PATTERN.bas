 /'*********************************************************

                Pattern and texture module

 *********************************************************'/

/'*********************************************************

    Given two coordinates on the surface of an objects,
    and a pointer to a colorinfo structure, this routine
    fills the structure with the objects colors info at
    this location. If the objects does not have an associated
    pattern, its default colorinfo is returned.  The
    routine returns TRUE if the pattern is hit, FALSE
    otherwise.

 *********************************************************'/

Function Find_Color(obj As OBJ_PTR, pattern As PATTERN_PTR ,locs As VECT_PTR ,cinfo As CINFO_PTR ,xmult As Single ,ymult As Single) As Integer
  Dim As PATTERN_PTR patt 
  Dim As Single modpos1, modpos2, pos1, pos2 
  Dim As Integer modx, mody 

  if (pattern=NULL) Then  /' no pattern '/
    copy_colorinfo(cinfo, @obj->cinfo) 
    return(FALSE) 
  EndIf
  
# ifdef ROBUST
    if (pattern->types <> PATT_HEADER) Then 
      Errors(INTERNAL_ERROR,1001)
    EndIf

    if ((xmult=0) OrElse (ymult=0)) Then 
      Errors(ZERO_MULTIPLIER,1002)
    EndIf
# endif

  /' find objects relative position '/

  ObjData(obj->types).RelPos(obj,locs,pos1,pos2,FALSE) ' revisar 

  pos1 /= xmult /' x and y multipliers '/
  pos2 /= ymult /' for pattern sizing  '/

  modx = int(pos1 \ pattern->xsize) 
  if (pos1<0) Then modx-=1  

  mody = int(pos2 \ pattern->ysize) 
  if (pos2<0) Then mody-=1  

  modpos1 = pos1 - (modx * pattern->xsize) 
  modpos2 = pos2 - (mody * pattern->ysize) 

  patt = pattern->child 

  while (patt<>NULL)  
    THEWORLD.pattern_matches+=1  

    ' revisar
    if PattData(patt->types).PattHit(modpos1, modpos2, patt) Then 
      copy_colorinfo(cinfo, @patt->cinfo) 
      return(TRUE) 
    EndIf
  
    patt = patt->sibling 
  Wend

  copy_colorinfo(cinfo, @obj->cinfo) 
  return(FALSE) 
End Function


/'*********************************************************

         Determines if point is inside rectangle

 *********************************************************'/

Function Rect_Hit(pos1 As Single , pos2 As Single , patt As PATTERN_PTR) As Integer
# ifdef ROBUST
    if (patt->types <> RECT_PATTERN) Then 
      Errors(INTERNAL_ERROR,1003)
    EndIf
# endif

  if (pos1 > patt->startx) AndAlso _
	 (pos1 < patt->endx) AndAlso _
	 (pos2 > patt->starty) AndAlso _
	 (pos2 < patt->endy) Then _
			return(TRUE) 

  return(FALSE) 
End Function


/'*********************************************************

         Determines if point is inside circle

 *********************************************************'/

Function Circle_Hit(pos1 As Single , pos2 As Single , patt As PATTERN_PTR) As Integer
  Dim As Single rad, a, b 

# ifdef ROBUST
    if (patt->types <> CIRCLE_PATTERN) Then 
      Errors(INTERNAL_ERROR,1004)
    EndIf
# endif

/'  a = (pos1 - patt->radius);   '/   /' bug in damn compiler  '/
/'  b = (pos2 - patt->radius);   '/   /' we have to break up   '/
                                  /' long float operations '/
  rad = POW(pos1 - patt->radius) + POW(pos2 - patt->radius) 
                                  /'  rad = POW(a)+POW(b); MMT '/
  if (rad <= POW(patt->radius)) Then return(TRUE) 
  return(FALSE) 
End Function


/'*********************************************************

   Determines if point px, py intersects lines x1,y1,x2,y2.
   Returns true if it does.

 *********************************************************'/

Function line_intersect(px As Single, py As Single, x1 As Single, y1 As Single, x2 As Single, y2 As Single) As Integer
  Dim As Single t 

  x1 -= px: x2 -= px            /' translate lines '/
  y1 -= py: y2 -= py 

  if ((y1 > 0.0) AndAlso (y2 > 0.0)) Then return(FALSE) 
  if ((y1 < 0.0) AndAlso (y2 < 0.0)) Then return(FALSE) 
  if ((x1 < 0.0) AndAlso (x2 < 0.0)) Then return(FALSE) 

  if (y1 = y2) Then 
    if (y1 <> 0.0) Then return(FALSE) 
    if ((x1 > 0.0) OrElse (x2 > 0.0)) Then return(FALSE) 
    return(TRUE) 
  EndIf
  
  t = (-y1) / (y2 - y1) 

  if ((x1 + t * (x2 - x1)) > 0.0) Then return(TRUE) 
  return(FALSE) 
End Function

/'*********************************************************

         Determines if point is inside Polygon

 *********************************************************'/

Function Poly_Hit(pos1 As Single , pos2 As Single , patt As PATTERN_PTR) As Integer
  Dim As Single xpos, ypos, nxpos, nypos 
  Dim As PATTERN_PTR lseg 
  Dim As Integer count 

# ifdef ROBUST
    if (patt->types <> POLY_PATTERN) Then 
      Errors(INTERNAL_ERROR,1005)
    EndIf
# endif

  lseg = patt->link 

  if (lseg = NULL) Then Errors(INTERNAL_ERROR,1006) 

  count = 0 

  while (lseg->link <> NULL)  
     if (line_intersect( pos1, pos2,lseg->startx, lseg->starty, lseg->link->startx, lseg->link->starty) ) Then 
       count+=1 
     EndIf
     lseg = lseg->link 
  Wend
   
  if (count mod 2) = 0 Then return(FALSE) 
  return(TRUE) 
End Function



