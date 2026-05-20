 
/'*********************************************************
                    objects loader language
 *********************************************************'/
Declare Function Get_Object() As OBJ_PTR
Declare Function Get_Instance_Of() As OBJ_PTR 
Declare Function Attach_Pattern() As PATTERN_PTR

/'*********************************************************
             External declarations from lexer
 *********************************************************'/
Declare Function Get_Next_Num() As Single 
Declare Function Get_Color_Val() As Short 
Declare Function Get_Next_Name() As string 


' emulacion de STRCMP
function strcmp(sa as string, sb as string) as integer
	return iif(sa=sb,0,1) ' 0 si son iguales
end function


/'*********************************************************
   Global input lines number.  If negative, error rountines
   will not report lines number (already reached EOF).
 *********************************************************'/
linenumber=0


/'*********************************************************
          Load the entire world from standard input
 *********************************************************'/
Function LoadWorld() As Integer                      /' load world from stdio '/
  linenumber = 1
  THEWORLD.stack=Get_Object()
  if(THEWORLD.stack=NULL) Then 
    Errors(ILLEGAL_OBJECT,201)
  EndIf

  linenumber = -1 
  return(TRUE) 
End Function

/'*********************************************************

     Get an objects and return a pointer to it. If it is
     not an objects, but some other world attribute
     specification, note it, but keep trying until an
     objects is found or end of input is encountered.
     This is slighly hacked out, but it works for now.
     It calls itself recursively sometimes.

     Changed 12 Mar 89 to include fix by Paul Balyoz
     to intitialize colorinfo structure (apparently some
     machines have a problem with it otherwise).

 *********************************************************'/

Function Get_Object() As OBJ_PTR 
  Dim As string strs'=space(SLEN) 
  Dim As string names 
  Dim As OBJ_PTR newobj, queue, temp 
  Dim As VECTOR locs,rad,d,v3, upper, lower 
  Dim As CINFOS cinfo 
  Dim As Integer found 

  def_colorinfo(@cinfo)           /' initialize colors info '/

  names=""
  queue=NULL 
  VectEqZero(@locs) 
  VectEqZero(@rad) 
  VectEqZero(@d) 
  VectEqZero(@v3) 
  VectEqZero(@upper) 
  VectEqZero(@lower) 

  if reg<0 Then return NULL 
  while (reg>-1)  
    GetToken(strs): found = FALSE 

    if (GetAttrib(strs)) Then 
      found = TRUE
    EndIf

    if (strcmp(strs,"BEGIN_INSTANCES")=0) Then 
      found = TRUE 
      THEWORLD.instances = Get_Object() 
    EndIf
  
    if (strcmp(strs,"END_INSTANCES")=0) Then 
      found = TRUE 
      return(queue) 
    EndIf
  
    if (strcmp(strs,"END_BBOX")=0) Then 
      found = TRUE 
      return(queue) 
    EndIf
  
    if (strcmp(strs,"BEGIN_BBOX")=0) Then 
		newobj=new_obj(_BBOX,@locs,@rad,@d,@v3,@cinfo,NULL,NULL,names,_
                      @upper, @lower,_
                      0.0, 0.0, 0.0) 
		newobj->child = Get_Object()
		newobj->nextobj = queue 
		queue = newobj 
		found = TRUE 
    EndIf
  
    if (strcmp(strs,"NAME")=0) Then 
       names=Get_Next_Name() 
       found = TRUE 
    EndIf

	temp=Get_Primitive(strs)
    if (temp<>NULL) Then 
      found = TRUE 
      temp->nextobj=queue 
      queue = temp 
    EndIf
  
    if (found=0) AndAlso (reg>-1) Then Errors(SYNTAX_ERROR,207) 
  Wend
    
  return(queue) 
End Function


/'*********************************************************

                 Load default colors info

 *********************************************************'/

Sub def_colorinfo(cinfo As CINFO_PTR)
  copy_colorinfo(cinfo, @def.cinfo) 
End Sub


/'*********************************************************

                    Copy colors info

 *********************************************************'/

Sub copy_colorinfo(c1 As CINFO_PTR , c2 As CINFO_PTR)
  if (c1 = NULL) Then return 

  SVectEQ(@c1->amb, @c2->amb) 
  SVectEQ(@c1->diff, @c2->diff) 
  SVectEQ(@c1->mirror, @c2->mirror) 
  SVectEQ(@c1->transp, @c2->transp) 
  VectEQ (@c1->density, @c2->density) 

  c1->fuzz     = c2->fuzz 
  c1->index    = c2->index 
  c1->dither   = c2->dither 
  c1->sreflect = c2->sreflect 
  c1->reflect  = c2->reflect 
End Sub


/'*********************************************************

            Get defaults from input file

 *********************************************************'/

Function Get_Default() As Integer
  Dim As string strs'=space(SLEN) 
  Dim As Integer ends, found 

  ends=0 

  GetLeftParen() 

  while (ends=0) AndAlso (reg>-1)  
    GetToken(strs) 

    found = GetOpt(strs, @def.cinfo) 

    if (strcmp(strs,"NO_SHADOW")=0) Then 
      def.shadow = FALSE 
      found = TRUE 
    EndIf

    if (strcmp(strs,"NO_LAMP")=0) Then 
      def.vlamp = FALSE 
      found = TRUE 
    EndIf

    if (strcmp(strs,"THRESHOLD")=0) Then 
      def.threshold = IsPos(Get_Next_Num()) 
      found = TRUE 
    EndIf

    if (strcmp(strs,"X_RES")=0) Then 
      def.x_res = int(IsPos(Get_Next_Num())) 
      found = TRUE 
    EndIf

    if (strcmp(strs,"Y_RES")=0) Then 
      def.y_res = int(IsPos(Get_Next_Num())) 
      found = TRUE 
    EndIf

    if (strcmp(strs,"ASPECT")=0) Then 
      def.aspect = IsPos(Get_Next_Num()) 
      found = TRUE 
    EndIf

    if (strcmp(strs,")")=0) Then 
       ends = 1: found = TRUE 
    EndIf
  
    if (found=0) Then Errors(UNDEFINED_PARAM,209) 
  Wend
   
  return(TRUE) 
End Function


/'*********************************************************

                    Get objects options

 *********************************************************'/

Function GetOpt(strs As string , cinfo As CINFO_PTR) As Integer
  Dim As Integer found 

  found = FALSE 

  if (strcmp(strs,"AMB")=0) Then 
    GetSVector(@cinfo->amb) 
    found = TRUE 
  EndIf

  if (strcmp(strs,"DIFF")=0) Then 
    GetSVector(@cinfo->diff) 
    found = TRUE 
  EndIf

  if (strcmp(strs,"MIRROR")=0) Then 
    GetSVector(@cinfo->mirror) 
    found = TRUE 
  EndIf

  if (strcmp(strs,"TRANS")=0) Then 
    GetSVector(@cinfo->transp) 
    found = TRUE 
  EndIf
  
  if (strcmp(strs,"DENSITY")=0) Then 
    GetVector(@cinfo->density) 
    found = TRUE 
  EndIf
  
  if (strcmp(strs,"FUZZ")=0) Then 
    cinfo->fuzz = Get_Color_Val() 
    found = TRUE 
  EndIf
  
  if (strcmp(strs,"INDEX")=0) Then 
    cinfo->index = IsPos(Get_Next_Num()) 
    found = TRUE 
  EndIf
  
  if (strcmp(strs,"DITHER")=0) Then 
    cinfo->dither = IsPos(Get_Next_Num()) 
    found = TRUE 
  EndIf
  
  if (strcmp(strs,"SREFLECT")=0) Then 
    cinfo->sreflect = IsPos(Get_Next_Num()) 
    found = TRUE 
  EndIf
  
  if (strcmp(strs,"REFLECT")=0) Then 
    cinfo->reflect = Get_Color_Val() 
    found = TRUE 
  EndIf
  
  return(found) 
End Function


/'*********************************************************

          Load a sphere and return pointer to it

 *********************************************************'/

Function GetSphere() As OBJ_PTR
  Dim As string strs'=space(SLEN)
  Dim As String names 
  Dim As VECTOR locs, rad, d, v3, upper, lower 
  Dim As Single xmult, ymult 
  Dim As CINFOS cinfo 
  Dim As OBJ_PTR newobj 
  Dim As PATTERN_PTR pattern, removes 
  Dim As Integer ends, f, found 

	ends=0
	f=0 
	def_colorinfo(@cinfo) 
	xmult=1
	ymult=1 
	pattern = Null
	removes = NULL 
	names = "" 

   GetLeftParen() 

   while (ends=0) AndAlso (reg>-1)  
    GetToken(strs) 

    found = GetOpt(strs,@cinfo) 

    if ((strcmp(strs,"POS")=0) OrElse (strcmp(strs,"LOC")=0) OrElse _
		(strcmp(strs,"POSITION")=0) OrElse (strcmp(strs,"LOCATION")=0)) Then 
      GetVector(@locs) 
      f Or= 1: found = TRUE 
    EndIf
  
    if (strcmp(strs,"RADIUS")=0) Then 
      rad.x = IsPos(Get_Next_Num()) 
      f Or= 2: found = TRUE 
    EndIf
  
    if (strcmp(strs,"PATTERN")=0) Then 
      pattern = Attach_Pattern() 
      found = TRUE 
    EndIf
  
    if (strcmp(strs,"REMOVE")=0) Then 
      removes = Attach_Pattern() 
      found = TRUE 
    EndIf
  
    if (strcmp(strs,"XMULT")=0) Then 
      xmult = IsPos(Get_Next_Num()) 
      found = TRUE 
    EndIf
  
    if (strcmp(strs,"YMULT")=0) Then 
      ymult = IsPos(Get_Next_Num()) 
      found = TRUE 
    EndIf
  
    if (strcmp(strs,"NAME")=0) Then 
       names = Get_Next_Name() 
       found = TRUE 
    EndIf
  
    if (strcmp(strs,")")=0) Then 
       ends = 1: found = TRUE  
    EndIf

    if ( found=0) Then Errors(UNDEFINED_PARAM,211) 
 Wend
    

  if (f<>3) Then Errors(TOO_FEW_PARMS,212) 
  rad.y=0
  rad.z=0 

  VectEqZero(@d) 
  VectEqZero(@upper) 
  VectEqZero(@lower) 

  newobj=new_obj(_SPHERE,@locs,@rad,@d,@v3,@cinfo,pattern,_
                 removes,names,_
                 @upper, @lower, 0.0, xmult, ymult) 

  THEWORLD.objcount+=1  
  return(newobj) 
End Function


/'*********************************************************

             Load lamp and attach to world

 *********************************************************'/

Function GetLamp() As Integer
  Dim As string strs'=space(SLEN) 
  dim as VECTOR locs, rad, d, v3, upper, lower 
  dim as CINFOS cinfo 
  Dim As Integer ends, f, found 

  ends=0
  f=0 

  cinfo.amb.r=CNUM
  cinfo.amb.g=CNUM
  cinfo.amb.b=CNUM 
  rad.y=150 

  GetLeftParen() 

  while (ends=0) AndAlso (reg>-1)  
    GetToken(strs): found = TRUE 

    found = GetOpt(strs,@cinfo) 

    if ((strcmp(strs,"POS")=0) OrElse (strcmp(strs,"LOC")=0) OrElse _
		(strcmp(strs,"POSITION")=0) OrElse (strcmp(strs,"LOCATION")=0)) Then 
      GetVector(@locs) 
      f Or= 1: found = TRUE 
    EndIf

    if (strcmp(strs,"RADIUS")=0) Then
      rad.x = IsPos(Get_Next_Num()) 
      f Or= 2: found = TRUE 
    EndIf
  
    if (strcmp(strs,"DIST")=0) Then 
      rad.y = IsPos(Get_Next_Num()) 
      f Or= 4: found = TRUE 
    EndIf
  
    if (strcmp(strs,")")=0) Then 
       ends = 1: found = TRUE 
    EndIf
  
    if ( found=0) Then Errors(UNDEFINED_PARAM,214) 
 Wend
    
  if (f<>7) Then return(FALSE) 

  rad.z=0 

  VectEqZero(@d) 
  VectEqZero(@v3) 
  VectEqZero(@upper) 
  VectEqZero(@lower) 

  cinfo.diff.r=CNUM
  cinfo.diff.g=CNUM
  cinfo.diff.b=CNUM 

  add_lamp(new_obj(_LAMP,@locs,@rad,@d,@v3,@cinfo,NULL,NULL,_
                   "", @upper, @lower,_
                   0.0, 0.0, 0.0)) 

  THEWORLD.lampcount+=1  
  return(TRUE) 
End Function


/'*********************************************************

         Load observer and attach him to the world

 *********************************************************'/

Function GetObserver() As Integer
  Dim As string strs'=space(SLEN) 
  Dim As VECTOR locs, lk, up, dirs, v3, upper, lower 
  Dim As CINFOS  cinfo 
  Dim As Integer    ends, f, found 

  def_colorinfo(@cinfo) 

  ends=0
  f=0 
  up.x = 0
  up.z = 0
  up.y = 1 

  GetLeftParen() 

  while (ends=0) AndAlso (reg>-1)  
    GetToken(strs): found = FALSE 

    if ((strcmp(strs,"POS")=0) OrElse (strcmp(strs,"LOC")=0) OrElse _
		(strcmp(strs,"POSITION")=0) OrElse (strcmp(strs,"LOCATION")=0)) Then 
      GetVector(@locs) 
      f Or= 1: found = TRUE 
    EndIf
  
    if (strcmp(strs,"LOOKAT")=0) Then 
      GetVector(@lk) 
      f Or= 2: found = TRUE 
    EndIf

    if (strcmp(strs,"UP")=0) Then 
      GetVector(@up) 
      found = TRUE 
    EndIf
  
    if (strcmp(strs,")")=0) Then 
       ends = 1: found = TRUE  
    EndIf
  
    if ( found=0) Then Errors(UNDEFINED_PARAM,216) 
 Wend
    

  VecSubtract(@dirs,@lk,@locs)     /' find view direction '/

  VectEqZero(@v3) 
  VectEqZero(@upper) 
  VectEqZero(@lower) 

  if (f<>3) Then Errors(TOO_FEW_PARMS,217) 

  THEWORLD.observer=_
     new_obj(_OBSERVER,@locs,@dirs,@up,@v3,@cinfo,NULL,NULL,_
             "", @upper, @lower,_
             0.0, 0.0, 0.0) 

  return(TRUE) 
End Function


/'*********************************************************

          Load triangle  - not yet implimented

 *********************************************************'/

Function GetTriangle() As OBJ_PTR
  Dim As string strs'=space(SLEN)
  Dim As String names 
  Dim As CINFOS       cinfo 
  Dim As VECTOR      locs, v1, v2, v3, upper, lower 
  Dim As OBJ_PTR     newobj 
  Dim As PATTERN_PTR pattern, removes 
  Dim As Integer         ends, f, found 
  Dim As Single       xmult, ymult 

  ends=0
  f=0
  xmult=1
  ymult=1 
  def_colorinfo(@cinfo) 
  pattern = Null
  removes = NULL 
  names   = "" 

  GetLeftParen() 

  while (ends=0) AndAlso (reg>-1)  
    GetToken(strs) 

    found = GetOpt(strs,@cinfo) 

    if ((strcmp(strs,"POS")=0) OrElse (strcmp(strs,"LOC")=0) OrElse _
		(strcmp(strs,"POSITION")=0) OrElse (strcmp(strs,"LOCATION")=0)) Then 
      GetVector(@locs) 
      f Or= 1: found = TRUE 
    EndIf
  
    if ((strcmp(strs,"V1")=0) OrElse (strcmp(strs,"VECT1")=0)) Then 
      GetVector(@v1) 
      f Or= 2: found = TRUE 
    EndIf
  
    if ((strcmp(strs,"V2")=0) OrElse (strcmp(strs,"VECT2")=0)) Then 
      GetVector(@v2) 
      f Or= 4: found = TRUE 
    EndIf
  
    if (strcmp(strs,"PATTERN")=0) Then 
      pattern=Attach_Pattern() 
      found = TRUE 
    EndIf
  
    if (strcmp(strs,"REMOVE")=0) Then 
      removes=Attach_Pattern() 
      found = TRUE 
    EndIf
  
    if (strcmp(strs,"XMULT")=0) Then 
      xmult = IsPos(Get_Next_Num()) 
      found=TRUE 
    EndIf
  
    if (strcmp(strs,"YMULT")=0) Then 
      ymult = IsPos(Get_Next_Num()) 
      found=TRUE 
    EndIf
  
    if (strcmp(strs,"NAME")=0) Then 
       names=Get_Next_Name() 
       found = TRUE 
    EndIf
  
    if (strcmp(strs,")")=0) Then 
       ends = 1: found = TRUE 
    EndIf

    if ( found=0) Then Errors(UNDEFINED_PARAM,219) 
 Wend
    
  if (f<>7) Then Errors(TOO_FEW_PARMS,220) 

  VectEqZero(@v3) 
  VectEqZero(@upper) 
  VectEqZero(@lower) 

  newobj=new_obj(_TRIANGLE,@locs,@v1,@v2,@v3,@cinfo,pattern,_
                 removes, names,_
                 @upper, @lower, 0.0, xmult, ymult) 

  THEWORLD.objcount+=1  
  return(newobj) 
End Function


/'*********************************************************

         Load sky data and attach it to the world

 *********************************************************'/

Function GetSky() As Integer
  Dim As VECTOR locs, v1, v2, v3, upper, lower 
  Dim As CINFOS  cinfo 
  Dim As string strs'=space(SLEN) 
  Dim As Integer    ends, found 

  ends=0 
  GetLeftParen() 

  def_colorinfo(@cinfo) 

  while (ends=0) AndAlso (reg>-1)  
    GetToken(strs) 

    found = GetOpt(strs,@cinfo)                     /'** DITHER ONLY **'/

    if (strcmp(strs,"ZENITH")=0) Then 
       GetSVector(@THEWORLD.skycolor_zenith) 
       found = TRUE 
    EndIf
  
    if ((strcmp(strs,"HORIZ")=0) OrElse (strcmp(strs,"HORIZON")=0)) Then 
       GetSVector(@THEWORLD.skycolor_horiz) 
       found = TRUE 
    EndIf
  
    if (strcmp(strs,")")=0) Then 
       ends=1: found=TRUE  
    EndIf
	
    if (  found=0) Then Errors(UNDEFINED_PARAM,222) 
 Wend
    
  VectEqZero(@v3) 
  VectEqZero(@upper) 
  VectEqZero(@lower) 

  THEWORLD.sky=new_obj(_SKY,@locs,@v1,@v2,@v3,@cinfo,NULL,NULL,_
                       "", @upper, @lower,_
                       0.0, 0.0, 0.0) 
  return(TRUE) 
End Function

/'*********************************************************

            Load ring and return pointer to it

 *********************************************************'/

Function GetRing() As OBJ_PTR
  Dim As string strs'=space(SLEN)
  Dim As String names 
  Dim As CINFOS       cinfo 
  Dim As VECTOR      locs, v1, v2, v3, upper, lower 
  Dim As OBJ_PTR     newobj 
  Dim As PATTERN_PTR pattern, removes 
  Dim As Integer     ends, f, found 
  Dim As Single      xmult, ymult 

  ends=0
  f=0 
  xmult=1
  ymult=1 
  def_colorinfo(@cinfo) 
  pattern = Null
  removes = NULL 
  names    = "" 

  GetLeftParen() 

  while (ends=0) AndAlso (reg>-1)  
    GetToken(strs) 

    found = GetOpt(strs,@cinfo) 

    if ((strcmp(strs,"POS")=0) OrElse (strcmp(strs,"LOC")=0) OrElse _
		(strcmp(strs,"POSITION")=0) OrElse (strcmp(strs,"LOCATION")=0)) Then 
      GetVector(@locs) 
      f Or= 1: found = TRUE 
    EndIf
  
    if ((strcmp(strs,"V1")=0) OrElse (strcmp(strs,"VECT1")=0)) Then 
      GetVector(@v1) 
      f Or= 2: found = TRUE 
    EndIf
  
    if ((strcmp(strs,"V2")=0) OrElse (strcmp(strs,"VECT2")=0)) Then 
      GetVector(@v2) 
      f Or= 4: found = TRUE 
    EndIf
  
    if (strcmp(strs,"RAD_1")=0) Then 
       v3.x = IsPos(Get_Next_Num()) 
       f Or= 8: found = TRUE 
    EndIf
  
    if (strcmp(strs,"RAD_2")=0) Then 
       v3.y = IsPos(Get_Next_Num()) 
       f Or= 16: found = TRUE 
    EndIf
  
    if (strcmp(strs,"PATTERN")=0) Then 
       pattern=Attach_Pattern() 
       found = TRUE 
    EndIf
  
    if (strcmp(strs,"REMOVE")=0) Then 
      removes=Attach_Pattern() 
      found = TRUE 
    EndIf
  
    if (strcmp(strs,"XMULT")=0) Then 
      xmult = IsPos(Get_Next_Num()) 
      found=TRUE 
    EndIf
  
    if (strcmp(strs,"YMULT")=0) Then 
      ymult = IsPos(Get_Next_Num()) 
      found=TRUE 
    EndIf
  
    if (strcmp(strs,"NAME")=0) Then 
       names=Get_Next_Name() 
       found = TRUE 
    EndIf
  
    if (strcmp(strs,")")=0) Then 
       ends = 1: found = TRUE  
    EndIf
  
    if (  found=0) Then Errors(UNDEFINED_PARAM,224) 
 Wend
    
  if (f<>31) Then Errors(TOO_FEW_PARMS,225) 
  v3.z = 0 
  Normalize(@v1) 
  Normalize(@v2) 

  newobj=new_obj(_RING,@locs,@v1,@v2,@v3,@cinfo,pattern,_
                 removes, names,_
                 @upper, @lower, 0.0, xmult, ymult) 

  THEWORLD.objcount+=1  
  return(newobj) 
End Function


/'*********************************************************

       Load parallelogram and return pointer to it

 *********************************************************'/

Function GetParallelogram() As OBJ_PTR
  Dim As string strs'=space(SLEN)
  Dim As String names 
  Dim As CINFOS      cinfo 
  Dim As VECTOR      locs, v1, v2, v3, upper, lower 
  Dim As OBJ_PTR     newobj 
  Dim As PATTERN_PTR pattern, removes 
  Dim As Integer     ends, f, found 
  Dim As Single      xmult, ymult 

  ends=0
  f=0 
  xmult=1
  ymult=1 
  def_colorinfo(@cinfo) 
  pattern = Null
  removes = NULL 
  names   = "" 

  GetLeftParen() 

  while (ends=0) AndAlso (reg>-1)  
    GetToken(strs) 

    found = GetOpt(strs,@cinfo) 

    if ((strcmp(strs,"POS")=0) OrElse (strcmp(strs,"LOC")=0) OrElse _
		(strcmp(strs,"POSITION")=0) OrElse (strcmp(strs,"LOCATION")=0)) Then 
      GetVector(@locs) 
      f Or= 1: found = TRUE 
    EndIf
  
    if ((strcmp(strs,"V1")=0) OrElse (strcmp(strs,"VECT1")=0)) Then 
      GetVector(@v1) 
      f Or= 2: found = TRUE 
    EndIf
  
    if ((strcmp(strs,"V2")=0) OrElse (strcmp(strs,"VECT2")=0)) Then 
      GetVector(@v2) 
      f Or= 4: found = TRUE 
    EndIf
  
    if (strcmp(strs,"PATTERN")=0) Then 
      pattern=Attach_Pattern() 
      found = TRUE 
    EndIf
  
    if (strcmp(strs,"REMOVE")=0) Then 
      removes=Attach_Pattern() 
      found = TRUE 
    EndIf
  
    if (strcmp(strs,"XMULT")=0) Then 
      xmult = IsPos(Get_Next_Num()) 
      found=TRUE 
    EndIf
  
    if (strcmp(strs,"YMULT")=0) Then 
      ymult = IsPos(Get_Next_Num()) 
      found=TRUE 
    EndIf
  
    if (strcmp(strs,"NAME")=0) Then 
       names=Get_Next_Name() 
       found = TRUE 
    EndIf

    if (strcmp(strs,")")=0) Then 
       ends = 1: found = TRUE 
    EndIf

    if ( found=0) Then Errors(UNDEFINED_PARAM,227) 
 Wend

  if (f<>7) Then Errors(TOO_FEW_PARMS,228) 

  VectEqZero(@v3) 
  VectEqZero(@upper) 
  VectEqZero(@lower) 

  newobj=new_obj(_PARALLELOGRAM,@locs,@v1,@v2,@v3,@cinfo,_
                 pattern, removes, names,_
                 @upper, @lower, 0.0, xmult, ymult) 

  THEWORLD.objcount+=1  
  return(newobj) 
End Function

/'*********************************************************

                  Load quadratic surface

 *********************************************************'/

Function GetQuadratic() As OBJ_PTR
  Dim As string strs'=space(SLEN)
  Dim As String names 
  Dim As CINFOS      cinfo 
  Dim As VECTOR      locs, v1, v2, v3, upper, lower 
  Dim As OBJ_PTR     newobj 
  Dim As PATTERN_PTR pattern, removes 
  Dim As Integer     ends, f, found 
  Dim As Single      cterm, xmult, ymult 

  ends=0
  f=0 
  xmult=1
  ymult=1 
  def_colorinfo(@cinfo) 
  pattern = Null
  removes = NULL 
  names   = "" 

  GetLeftParen() 

  upper.x = 10
  upper.y = 10
  upper.z = 10 

  lower.x = -10
  lower.y = -10
  lower.z = -10 

  v1.x = 0
  v1.y = 1
  v1.z = 0 

  while (ends=0) AndAlso (reg>-1)  
    GetToken(strs) 
    found = GetOpt(strs,@cinfo) 

    if ((strcmp(strs,"POS")=0) OrElse (strcmp(strs,"LOC")=0) OrElse _
		(strcmp(strs,"POSITION")=0) OrElse (strcmp(strs,"LOCATION")=0)) Then 
      GetVector(@locs) 
      f Or= 1: found = TRUE 
    EndIf

    if (strcmp(strs,"A")=0) Then 
      v2.x  = Get_Next_Num() 
      f Or= 2: found = TRUE 
    EndIf
  
    if (strcmp(strs,"B")=0) Then 
      v2.y  = Get_Next_Num() 
      f Or= 4: found = TRUE 
    EndIf
  
    if (strcmp(strs,"C")=0) Then 
      v2.z  = Get_Next_Num() 
      f Or= 8: found = TRUE 
    EndIf
  
    if (strcmp(strs,"D")=0) Then 
      cterm = Get_Next_Num() 
      f Or= 16: found = TRUE 
    EndIf
  
    if (strcmp(strs,"XMIN")=0) Then 
      lower.x  = Get_Next_Num() 
      found    = TRUE 
    EndIf
  
    if (strcmp(strs,"XMAX")=0) Then 
      upper.x  = Get_Next_Num() 
      found    = TRUE 
    EndIf
  
    if (strcmp(strs,"YMIN")=0) Then 
      lower.y  = Get_Next_Num() 
      found    = TRUE 
    EndIf
  
    if (strcmp(strs,"YMAX")=0) Then 
      upper.y  = Get_Next_Num() 
      found    = TRUE 
    EndIf
  
    if (strcmp(strs,"ZMIN")=0) Then 
      lower.z  = Get_Next_Num() 
      found    = TRUE 
    EndIf
  
    if (strcmp(strs,"ZMAX")=0) Then 
      upper.z  = Get_Next_Num() 
      found    = TRUE 
    EndIf
  
    if (strcmp(strs,"DIR")=0) Then 
      GetVector(@v1) 
      found = TRUE 
    EndIf
  
    if (strcmp(strs,"PATTERN")=0) Then 
      pattern=Attach_Pattern() 
      found = TRUE 
    EndIf
  
    if (strcmp(strs,"REMOVE")=0) Then 
      removes=Attach_Pattern() 
      found = TRUE 
    EndIf
  
    if (strcmp(strs,"XMULT")=0) Then 
      xmult = IsPos(Get_Next_Num()) 
      found=TRUE 
    EndIf
  
    if (strcmp(strs,"YMULT")=0) Then 
      ymult = IsPos(Get_Next_Num()) 
      found=TRUE 
    EndIf
  
    if (strcmp(strs,"NAME")=0) Then 
       names=Get_Next_Name() 
       found = TRUE 
    EndIf
  
    if (strcmp(strs,")")=0) Then 
       ends = 1: found = TRUE 
    EndIf
  
    if (found=0) Then Errors(UNDEFINED_PARAM,230) 
 Wend

  if (f<>31) Then Errors(TOO_FEW_PARMS,231) 

  VectEqZero(@v3) 

  newobj=new_obj(_QUADRATIC,@locs,@v1,@v2,@v3,@cinfo,pattern,_
                 removes, names,_
                 @upper, @lower, cterm, xmult, ymult) 

  THEWORLD.objcount+=1  
  return(newobj) 
End Function


/'*********************************************************

       Load the focal length of the observers lens

 *********************************************************'/

Function GetFocLength() As Integer 
  THEWORLD.flength=IsPos(Get_Next_Num()) 
  return(TRUE) 
End Function


/'*********************************************************

    Load a primitive from input and return pointer to it

 *********************************************************'/

Function Get_Primitive(strs As string) As OBJ_PTR
  Dim As OBJ_PTR newobj 

  newobj=NULL 
  if (strcmp(strs,"SPHERE")=0) Then 
    newobj=GetSphere()
	if newobj=NULL Then 
      Errors(ILLEGAL_OBJECT,232)
    EndIf
  EndIf
  
  if (strcmp(strs,"PARALLELOGRAM")=0) Then 
    newobj=GetParallelogram()
	if newobj=NULL Then 
      Errors(ILLEGAL_OBJECT,233)
    EndIf
  EndIf
  
  if (strcmp(strs,"TRIANGLE")=0) Then 
    newobj=GetTriangle()
	if newobj=NULL Then 
      Errors(ILLEGAL_OBJECT,234)
    EndIf
  EndIf
  
  if (strcmp(strs,"RING")=0) Then 
    newobj=GetRing()
	if newobj=NULL Then 
      Errors(ILLEGAL_OBJECT,235)
    EndIf
  EndIf
  
  if (strcmp(strs,"QUADRATIC")=0) Then 
    newobj=GetQuadratic()
	if newobj=NULL Then 
      Errors(ILLEGAL_OBJECT,236)
    EndIf
  EndIf
  
  if (strcmp(strs,"INSTANCE_OF")=0) Then 
    newobj=Get_Instance_Of()
	if newobj=NULL Then 
      Errors(ILLEGAL_OBJECT,237)
    EndIf
  EndIf
  
  return(newobj) 
End Function


/'*********************************************************

     Load an attribute from input and do stuff with it

 *********************************************************'/

Function GetAttrib(strs As string) As Integer
  Dim As Integer found, scrap 

  found=FALSE 

  if (strcmp(strs,"SKY")=0) Then 
    if (GetSky()=0) Then Errors(INTERNAL_ERROR,238) 
    found=TRUE 
  EndIf
  
  if (strcmp(strs,"FOC_LENGTH")=0) Then 
    if (GetFocLength()=0) Then Errors(INTERNAL_ERROR,239) 
    found=TRUE 
  EndIf
  
  if (strcmp(strs,"DEFAULT")=0) Then 
    if (Get_Default()=0) Then Errors(INTERNAL_ERROR,240) 
    found=TRUE 
  EndIf
  
  if (strcmp(strs,"FILE_NAME")=0) Then 
    THEWORLD.outfile=Get_Next_Name() 
    found=TRUE 
  EndIf
  
  if (strcmp(strs,"LAMP")=0) Then 
    if (GetLamp()=0) Then Errors(INTERNAL_ERROR,241) 
    found=TRUE 
  EndIf
  
  if (strcmp(strs,"OBSERVER")=0) Then 
    if (GetObserver()=0) Then Errors(INTERNAL_ERROR,242) 
    found=TRUE 
  EndIf
  
  if (strcmp(strs,"PATTERN")=0) Then 
    if (GetPattern()=0) Then Errors(INTERNAL_ERROR,243) 
    found=TRUE 
  EndIf
  
  if (strcmp(strs,"FIRST_SCAN")=0) Then 
    Warning(OBSOLETE_OPTION,"FIRST_SCAN") 
    scrap = cint(IsPos(Get_Next_Num())) 
    found=TRUE 
  EndIf
  
  if (strcmp(strs,"LAST_SCAN")=0) Then 
    Warning(OBSOLETE_OPTION,"LAST_SCAN") 
    scrap = cint(IsPos(Get_Next_Num())) 
    found=TRUE 
  EndIf

  return(found) 
End Function

