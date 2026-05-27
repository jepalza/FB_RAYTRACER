 
/'*********************************************************

   This module loads patterns and sub-patterns for the
   inout.c file, which was getting inconvienently big.

 *********************************************************'/


/'*********************************************************

     Attaches a pattern pointer to an objects given
     pattern name.

 *********************************************************'/

Function Attach_Pattern() As PATTERN_PTR
  Dim As string patname 
  dim as PATTERN_PTR pat 

  patname=Get_Next_Name() 
  pat = find_pat(patname) 
  if (pat=NULL) Then Errors(PATTERN_NOT_FOUND,1101) 

  return(pat) 
End Function


/'*********************************************************

   Load a circle sub-pattern and return pointer to it

 *********************************************************'/

Function Get_Circle_Pattern() As PATTERN_PTR
  Dim As string strs 
  Dim As Integer ends, f, found 
  Dim As PATTERN_PTR pattern 
  Dim As CINFOS cinfo 

  ends=0
  f=0 
  pattern=new_pat() 
  pattern->types = CIRCLE_PATTERN 
  def_colorinfo(@cinfo) 

  GetLeftParen() 

  while (ends=0) andalso (reg>-1)  
    GetToken(strs): found = TRUE 

    if (strcmp(strs,",")<>0) Then 
      found = GetOpt(strs,@cinfo) 
      if (strcmp(strs,"RADIUS")=0) Then 
        pattern->radius = IsPos(Get_Next_Num()) 
        f Or= 1: found = TRUE 
      EndIf
  
      if (strcmp(strs,")")=0) Then 
         ends=1: found = TRUE  
      EndIf
    EndIf
  
    if (found=0) Then Errors(UNDEFINED_PARAM,1102) 
  Wend
    
  if (f<>1) Then Errors(TOO_FEW_PARMS,1103) 

  copy_colorinfo(@pattern->cinfo,@cinfo) 

  return(pattern) 
End Function

/'*********************************************************

   Load a rectangle sub-pattern and return pointer to it

 *********************************************************'/

Function Get_Rect_Pattern() As PATTERN_PTR
  Dim As string strs  
  Dim As Integer ends, f, found 
  Dim As PATTERN_PTR pattern  ', new_pat() 
  Dim As CINFOS cinfo 

  ends=0
  f=0 
  pattern=new_pat() 
  pattern->types = RECT_PATTERN 
  def_colorinfo(@cinfo) 

  GetLeftParen() 

  while (ends=0) andalso (reg>-1)  
    GetToken(strs): found = TRUE 

    if (strcmp(strs,",")<>0) Then 
      found = GetOpt(strs,@cinfo) 
      if (strcmp(strs,"START_X")=0) Then 
        pattern->startx=Get_Next_Num() 
        f Or= 1: found = TRUE  
      EndIf
  
      if (strcmp(strs,"START_Y")=0) Then 
        pattern->starty=Get_Next_Num() 
        f Or= 2: found = TRUE 
      EndIf
  
      if (strcmp(strs,"END_X")=0) Then 
        pattern->endx=Get_Next_Num() 
        f Or= 4: found = TRUE 
      EndIf
  
      if (strcmp(strs,"END_Y")=0) Then 
        pattern->endy=Get_Next_Num() 
        f Or= 8: found = TRUE 
      EndIf
  
      if (strcmp(strs,")")=0) Then 
         ends=1: found = TRUE  
      EndIf
    EndIf
  
    if (found=0) Then Errors(UNDEFINED_PARAM,1104) 
 Wend

  if (f<>15) Then Errors(TOO_FEW_PARMS,1105) 

  copy_colorinfo(@pattern->cinfo,@cinfo) 

  return(pattern) 
End Function

/'*********************************************************

   Load a polygon sub-pattern and return pointer to it

 *********************************************************'/

Function Get_Poly_Pattern() As PATTERN_PTR
  Dim As string strs  
  Dim As Integer ends, f, found 
  Dim As PATTERN_PTR pattern, pointpatt 
  Dim As CINFOS cinfo 

  ends=0
  f=0 
  pattern=new_pat() 
  pattern->types = POLY_PATTERN 
  def_colorinfo(@cinfo) 

  GetLeftParen() 

  while (ends=0) andalso (reg>-1)  
    GetToken(strs): found = TRUE 

    if (strcmp(strs,",")<>0) Then 
      found = GetOpt(strs,@cinfo) 
      if (strcmp(strs,"POINT")=0) Then 
        
        GetLeftParen() 

        pointpatt = new_pat() 
        pointpatt->startx = Get_Next_Num() 
        pointpatt->starty = Get_Next_Num() 
        f+=1: found = TRUE 
        GetToken(strs) 

        if (strcmp(strs,")") <> 0) Then 
          Errors(RPAREN_EXPECTED,1106) 
        EndIf
  
        strs = "" /' clear str '/

        pointpatt->link = pattern->link 
        pattern->link = pointpatt 
      EndIf
  
      if (strcmp(strs,")")=0) Then 
         ends=1: found = TRUE  
      EndIf
  
    EndIf
  
    if ( found=0) Then Errors(UNDEFINED_PARAM,1107) 
  
 Wend


  if (f<3) Then Errors(TOO_FEW_PARMS,1108) 

  copy_colorinfo(@pattern->cinfo,@cinfo) 

  return(pattern) 
End Function


/'*********************************************************

       Load a sub-pattern and return a pointer to it,
       or null if not found.

 *********************************************************'/

Function Get_SubPattern( strs As string) As PATTERN_PTR
  if (strcmp(strs,"RECTANGLE")=0) Then 
    return(Get_Rect_Pattern())
  EndIf
  
  if (strcmp(strs,"CIRCLE")=0) Then 
    return(Get_Circle_Pattern())
  EndIf

  if (strcmp(strs,"POLYGON")=0) Then 
    return(Get_Poly_Pattern())
  EndIf
  
  return(NULL) 
End Function


/'*********************************************************

        Load pattern and attach it to pattern list

 *********************************************************'/

Function GetPattern() As Integer
  Dim As string strs  
  Dim As Integer ends, f, found 
  Dim As PATTERN_PTR pattern, spat ', new_pat() 

  ends=0
  f=0 
  GetLeftParen() 

  pattern = new_pat() 

  pattern->types = PATT_HEADER 

  while (ends=0) andalso (reg>-1) 
    GetToken(strs): found = TRUE 
    if (strcmp(strs,",")<>0) Then 
      
      found = FALSE 
      if (strcmp(strs,"X_SIZE")=0) Then 
         pattern->xsize=Get_Next_Num() 
         f Or= 1: found = TRUE 
      EndIf
  
      if (strcmp(strs,"Y_SIZE")=0) Then 
         pattern->ysize=Get_Next_Num() 
         f Or= 2: found = TRUE 
      EndIf
  
      if (strcmp(strs,"NAME")=0) Then 
         pattern->names=Get_Next_Name() 
         f Or= 4: found = TRUE 
      EndIf
  
	   spat=Get_SubPattern(strs)
      if (spat<>NULL) Then 
         spat->sibling=pattern->child 
         pattern->child=spat 
         f Or= 8: found = TRUE 
      EndIf
  
      if (strcmp(strs,")")=0) Then 
         ends=1: found = TRUE  
      EndIf
  
    EndIf
  
    if (found=0) Then Errors(UNDEFINED_PARAM,1109) 
 Wend
    
  if (f<>15) Then Errors(TOO_FEW_PARMS,1110) 

  pattern->sibling=THEWORLD.patlist 
  THEWORLD.patlist=pattern 

  return(TRUE) 
End Function


