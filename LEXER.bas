 
/'*********************************************************

      Small lexical analyser for inout. Also contains
      some bounds checking code.

 *********************************************************'/

function LeeChar() as string
	if reg<0 then return ""
	if reg>=(scenechar-1) then reg=-1:return " " ' final del fichero, ya no leemos mas
	reg+=1
	Return qrtscene(reg)
end function

sub DejaChar()
	reg-=1
end sub

/'*********************************************************

           Transforms all white spaces to blanks

    - changed 13 Mar 89 to handle tabs in input file

 *********************************************************'/

Function towhite(c as string) as string
  if c=chr(13) orelse c=chr(10) then linenumber+=1  

  if (c=chr(10) orelse (c=chr(13)) OrElse (c=chr(9)) OrElse (c="=")  OrElse (c=",")  OrElse (c=";")) Then return(" ") 

  return(c) 
End Function


/'*********************************************************

        Removes blank space before next token

 *********************************************************'/

Sub rmspace() 
  Dim As string c=" "
  while c=" "
	c=towhite(LeeChar())
  Wend
  DejaChar() 
End Sub

/'*********************************************************

 Comment Killer - Added 16 Jun 88 to handle nested comments

 *********************************************************'/

Sub Comment_Killer()
  Dim As string c 
  c="" 
  while c <> "}" 
    c = towhite(LeeChar()) 
    if (c = "{") Then Comment_Killer() 
  Wend
End Sub

/'*********************************************************

               Get next token from FileIn

   - Changed 12 Mar 89 to fix compatibility problem with
     toupper() across different versions of UN*X.
     Aparently some versions change a letter even if it is
     already upper case.

 *********************************************************'/

Sub GetToken(s As string) /' get a token from stdio '/
  Dim As string c
  'Dim As integer x
a1:
  'x=0
  s=""
  c=""                  /' char count '/
  if reg=scenechar then exit sub
'  While (eof(FileIn)<(SLEN-1)=0) andalso (x<(SLEN-1))
  While (len(s)<(SLEN-1)) andalso (reg>-1)
    c = towhite(LeeChar()) 
    if ("a"<=c AndAlso c<="z") Then c=Ucase(c) 
    s+=c' : x+=1

    if (c=" ") Then 
		'x-=1
		's=chr(0)
		s=left(s,len(s)-1)
		exit while
    EndIf
  
    if (c="(" OrElse c=")") Then 
      if (reg=1) Then 
        's[x]=chr(0)
		s=""
		exit while   
	  else
		'if c=")" then s=left(s,len(s)-1):Print #99, s,s,s':reg+=1
		'if c="(" then s=left(s,len(s)-1):reg-=2
		'x-=1
		's[x]=chr(0)
		's=left(s,len(s)-1)
		exit while
      EndIf
    EndIf
  
    if (c="{") Then 
      'x-=1
	  'reg-=1
	  s=left(s,len(s)-1)
      Comment_Killer() 
      rmspace() 
    EndIf
  Wend
  s=trim(s)
	' if reg>=lof(FileIn) then beep:exit sub
	'Print #99, eof(FileIn);
	if reg<0 then exit sub
	if s="" then goto a1
	' parche para solucionar el problema del parentesis derecho, que sale por error mio
	if right(s,1)=")" andalso len(s)>1 then reg-=1:s=left(s,len(s)-1)
	'for x=1 to len(s)
	'	Print #99, mid(s,x,1),asc(mid(s,x,1))
	'next x

	  
	'if s<>"" then Print #99, ":";s;
End Sub


/'*********************************************************

      Return value if colors is in range 0<=cnum<=CNUM
      otherwise call error routine.

 *********************************************************'/

Function InRange(cnum As single) As Single
  if (cnum>=0.0) AndAlso (cnum<=1.00) then return(cnum) 
  Errors(COLOR_VALUE_ERR,1501) 
  return(0)    /' this is to keep lint happy! '/
End Function


/'*********************************************************

         Return value if value is >=0.
         otherwise call error routine.

 *********************************************************'/

Function IsPos(vals As single) As Single
  if (vals>= 0.0) then return(vals) 
  Errors(LESS_THAN_ZERO,1502) 
  return(0)    /' this is to keep lint happy! '/
End Function

/'*********************************************************

    Reads next number and converts to float from string

 *********************************************************'/

Function Get_Next_Num() As Single 
  Dim As string strs'=space(SLEN)
  Dim As Single vals 
  GetToken(strs)
  'if instr(strs,")") then reg-=1 ' este parche soluciona el problema del parentesis no localizado
  vals=val(strs) 
  return(vals) 
End Function


/'*********************************************************

      Reads a name from input, and allocates some space
      for it. Returns a pointer to space.

 *********************************************************'/

Function Get_Next_Name() As string 
  Dim As string strs=""

	while strs=""
		GetToken(strs) 
	wend

  's=space(len(strs)+1)
'  if (s=NULL) Then 
'    Errors(MALLOC_FAILURE,1503)
'  EndIf
  
  's=(s+strs) 
  return(strs) 
End Function

/'*********************************************************

     Reads a number 0..1 and returns a colors value
     0..CNUM;

 *********************************************************'/

Function Get_Color_Val() As Short 
  return((InRange(Get_Next_Num())*CNUM)) 
End Function


/'*********************************************************

       Returns true if the next token is a left paren

 *********************************************************'/

Sub GetLeftParen() 
  Dim As string strs'=space(SLEN)
  GetToken(strs) 
  if (strcmp(strs,"(")<>0) Then Errors(LPAREN_EXPECTED,1504) 
  return 
End Sub


/'*********************************************************

       Returns true if the next token is a left paren

 *********************************************************'/

Function GetRightParen() As Integer 
  Dim As string strs'=space(SLEN)
  GetToken(strs)
  if (strcmp(strs,")")<>0) Then return(FALSE) 
  return(TRUE) 
End Function


/'*********************************************************

           Gets a VECTOR structure of the form
           (num1, num2, num3)

 *********************************************************'/
 
Sub GetVector(vector As VECT_PTR)
  GetLeftParen() 

  vector->x = Get_Next_Num() 
  vector->y = Get_Next_Num() 
  vector->z = Get_Next_Num() 

  if( GetRightParen()=0) Then Errors(ILLEGAL_VECTOR,1505) 
End Sub


/'*********************************************************

           Gets a SVECTOR structure of the form
           (num1, num2, num3)

 *********************************************************'/

Sub GetSVector(svector As SVECT_PTR)
  GetLeftParen() 

  svector->r = Get_Color_Val() 
  svector->g = Get_Color_Val() 
  svector->b = Get_Color_Val() 

  if( GetRightParen()=0) Then Errors(ILLEGAL_SVECTOR,1506) 
End Sub
