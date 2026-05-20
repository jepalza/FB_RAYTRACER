

#include "header.bi"

#Include "BBOX.BAS"
#Include "BMP.BAS"
#Include "ERROR.BAS"
#Include "INOUT.BAS"
#Include "INSTANCE.BAS"
#Include "INTERSEC.BAS"
#Include "LEXER.BAS"
#Include "MTH.BAS"
#Include "NORM.BAS"
#Include "OFFSET.BAS"
#Include "PATTERN.BAS"
#Include "PATTERNI.BAS"
#Include "PRECOMP.BAS"
#Include "RAY.BAS"
#Include "RELPOS.BAS"
#Include "RESIZE.BAS"
#Include "STACK.BAS"




CNUM = CNUMB          /' MMT addition '/



/'*********************************************************

    Initialize some observer data, like up and right
    vectors, focal length, etc.

 *********************************************************'/

Sub Setup_Observer() 
  if (THEWORLD.observer=NULL) Then Errors(NO_OBSERVER,1) 

  Normalize(@THEWORLD.observer->vect1) 
  VectEQ(@THEWORLD.obsup, @ THEWORLD.observer->vect2) 

  CrossProd(@THEWORLD.obsright, _            /' right = up x dir '/
            @THEWORLD.obsup,_
            @THEWORLD.observer->vect1) 

  CrossProd(@THEWORLD.obsup,_                /' up = dir x right '/
            @THEWORLD.observer->vect1,_
            @THEWORLD.obsright) 

  Normalize(@THEWORLD.obsup) 
  Normalize(@THEWORLD.obsright) 
End Sub


/'*********************************************************

         Initialize the world and assign defaults

 *********************************************************'/

Sub init_world()                      /' make a universe '/
  THEWORLD.stack      = NULL
  THEWORLD.observer   = NULL
  THEWORLD.instances  = NULL
  THEWORLD.sky        = NULL
  THEWORLD.lamps      = NULL 
  THEWORLD.patlist    = NULL 
  THEWORLD.outfile    = "" 
  THEWORLD.objcount   = 0 
  THEWORLD.lampcount  = 0 
  THEWORLD.flength    = 50 

  THEWORLD.skycolor_zenith.r = 0
  THEWORLD.skycolor_zenith.b = 0
  THEWORLD.skycolor_zenith.g = 0
  THEWORLD.skycolor_horiz.r  = 0
  THEWORLD.skycolor_horiz.g  = 0
  THEWORLD.skycolor_horiz.b  = 0 

  THEWORLD.ray_intersects  = 0
  THEWORLD.pixels_hit      = 0
  THEWORLD.primary_traced  = 0
  THEWORLD.to_lamp         = 0
  THEWORLD.refl_trans      = 0
  THEWORLD.bbox_intersects = 0
  THEWORLD.pattern_matches = 0
  THEWORLD.intersect_tests = 0 

  THEWORLD.globindex = 1.00    /' global index of refraction '/

  def.cinfo.transp.r = 0        /' default transmittion '/
  def.cinfo.transp.g = 0
  def.cinfo.transp.b = 0 

  def.cinfo.mirror.r = 0        /' default reflection '/
  def.cinfo.mirror.g = 0
  def.cinfo.mirror.b = 0 

  def.cinfo.amb.r    = 25        /' default ambiant light '/
  def.cinfo.amb.g    = 25
  def.cinfo.amb.b    = 25 

  def.cinfo.diff.r   = CNUM        /' default diffuse light '/
  def.cinfo.diff.g   = CNUM
  def.cinfo.diff.b   = CNUM 

  def.cinfo.density.x= .01
  def.cinfo.density.y= .01
  def.cinfo.density.z= .01     /' default glass density '/

  def.cinfo.fuzz     = 0 
  def.cinfo.index    = CNUM 
  def.cinfo.dither   = 3 
  def.cinfo.reflect  = 0 
  def.cinfo.sreflect = 10 

  def.shadow        = TRUE     /' shadows '/
  def.vlamp         = FALSE    /' no visible lamps '/
  def.int_x         = 1         /' no interpolation '/
  def.int_y         = 1 
  def.threshold     = .1       /' threshold at 10 percent '/
  def.ithreshold    = def.threshold * CNUM 

  def.x_res         = 320      /' IBM PC CLONE '/
  def.y_res         = 200 
  def.aspect        = 0.650 
End Sub







/'*********************************************************

     Call other stuff to load world and generate image

     Changed 12 Mar 88 to add command lines arguments.

 *********************************************************'/



/'*********************************************************

  Added 12 Mar 88 to parse command lines arguments.

  Command lines arguments are optional as follows:

   -xres int -yres int -aspect float -foclen float

  The command lines arguments take precidence over the
  DEFAULT() command values.

 *********************************************************'/

Sub Parse_CL_Args(argv As string)
	Dim As Integer a, x, argc, found 
	dim as string params(20) ' suficiente?
	dim as string sc,sa
	
	argv=trim(argv)
	if argv="" then 
		Print #99, "Usage: QRT <file> [-xres n] [-yres n] [-aspect n] [-foclen n] [-rgb n]"
		Print #99, ""
		Print #99, "Ejemplo -> QRT ejemplos\ball.txt -xres 640 -yres 480 -aspect .5 -foclen 1 -rgb 200"
		'Errors(ILLEGAL_OPTION,011) 
		sleep:end
	EndIf
	
	argv+=" " ' para que el ultimo param se pueda localizar
	a=0
	' separa parametros
	for x=1 To 256 'argc                /' parse command lines args '/
		sc=mid(argv,x,1)
		if sc=chr(9) then sc=" ":mid(argv,x,1)=sc ' elimina posibles tabulaciones
		sa=sa+sc
		if sc=" " then params(a)=trim(sa):sa="":a+=1':Print #99, params(a-1)
	next

	' interpreta parametros
	x=0
	while x<a
	    argv=params(x)
		
		if x=0 then 
			filein=argv
		endif
	
		if argv="-xres" Then 
		  x+=1
		  argv=params(x)
		  if instr(argv,"-") Then 
			Errors(TOO_FEW_PARMS,3)
		  EndIf
		  def.x_res = val(argv)
		  if (def.x_res <= 0) Then 
			Errors(LESS_THAN_ZERO,4)
		  EndIf
		EndIf
	  
		if argv="-yres" Then 
		  x+=1
		  argv=params(x)
		  if instr(argv,"-") Then 
			Errors(TOO_FEW_PARMS,5)
		  EndIf
		   def.y_res = val(argv)
		  if (def.y_res <= 0) Then 
			Errors(LESS_THAN_ZERO,6)
		  EndIf
		EndIf
	  
		if argv="-aspect" Then 
		  x+=1
		  argv=params(x)
		  if instr(argv,"-") Then 
			Errors(TOO_FEW_PARMS,7)
		  EndIf
		  def.aspect = val(argv)
		  if (def.aspect <= 0) Then 
			Errors(LESS_THAN_ZERO,8)
		  EndIf
		EndIf
	  
		if argv="-foclen" Then 
		  x+=1
		  argv=params(x)
		  if instr(argv,"-") Then 
			Errors(TOO_FEW_PARMS,9)
		  EndIf
		  THEWORLD.flength = val(argv)
		  if (THEWORLD.flength <=0) Then 
			Errors(LESS_THAN_ZERO,010)
		  EndIf
		EndIf
	  
		if argv="-rgb" Then 
		  x+=1
		  argv=params(x)
		  if instr(argv,"-") Then 
			Errors(TOO_FEW_PARMS,7)
		  EndIf
		  CNUM = val(argv)
		  if (CNUM <= 0) Then 
			Errors(LESS_THAN_ZERO,7)
		  EndIf
	  
		  def.cinfo.diff.r   = CNUM        /' default diffuse light '/
		  def.cinfo.diff.g   = CNUM
		  def.cinfo.diff.b   = CNUM 
		  def.cinfo.index    = CNUM 
		  def.ithreshold     = def.threshold * CNUM 
		  Print #99, "generating with ";CNUM;" shades/colors"
		EndIf
		x+=1
	wend
	'Print #99, filein,def.x_res,def.y_res,THEWORLD.flength,def.aspect,CNUM
End Sub


sub read_QRT_Scene(filein as string)
	' lee el fichero QRT de escena en memoria al completo
	open filein for input as 1
		dim as string sa
		while not(eof(1))
			line input #1,sa
			sa+=" "
			for f as integer = 1 to len(sa)
				scenechar+=1
				qrtscene(scenechar)=mid(sa,f,1)
			next
			'scenechar+=1
			'qrtscene(scenechar)=chr(10)
		wend
	close 1
end sub



' ----------------------------------------- MAIN ---------------------------------------
open "cons" for output as 99 ' salida de informacion en ventana DOS

dim argv as string=command

#if 0
	dim as string ejem="plus"
	'argv="ejemplos\"+ejem+".txt -xres 1024 -yres 768 -foclen 7 -aspect .5 -rgb 255 " ' pruebas
	'argv="ejemplos\"+ejem+".txt -xres 1024 -yres 768 -aspect .65" ' pruebas
	argv="ejemplos\"+ejem+".txt -xres 640 -yres 480 -aspect .65 -rgb 200" ' pruebas
#endif

#if 0
argv=argv+" -xres 1024 -yres 768 -aspect .7 -rgb 200"
#endif

Print #99, "Quick Ray Trace: Copyright 1988, 1989 Steve Koren"
Print #99, "Version 1.5"
Print #99, "Modifications by:  Markham Thomas  Sept 1989"
Print #99, "Conversion y Mejoras con FreeBasic por Joseba Epalza <jepalza> MAYO-2026"
Print #99,


init_world() 

Parse_CL_Args(argv) 

display=1 ' salida grafica
if display then screenres 1280,1024,32

read_QRT_Scene(filein)

if LoadWorld()=0 Then Errors(SYNTAX_ERROR,2) 

Make_Bbox(THEWORLD.stack)      /' make bboxes '/

Do_Precomp(THEWORLD.stack)     /' precompute stuff '/

Setup_Observer() 

Open_File_BMP()  ' prepara el BMP para salida

Screen_Trace()  ' genera la pantalla linea a linea y la va guardando en un buffer para luego guardar como BMP

Close_File()  ' cierra el BMP

World_Stats() 


sleep
