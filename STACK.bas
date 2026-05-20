 /'******************************************************

                  stacks and queues

 ******************************************************'/


/'*********************************************************

    Calls the pre-computing routine for each objects in
    the objects tree by calling Do_Precomp_Tree().  Also,
    does a bit of general housekeeping, like finding the
    center of the screen, etc.

 *********************************************************'/

Sub Do_Precomp(node As OBJ_PTR)
  def.x_center = def.x_res/2 
  def.y_center = def.y_res/2 

  THEWORLD.x_divisor = THEWORLD.flength * def.x_res * def.aspect / 60 

  THEWORLD.y_divisor = THEWORLD.flength * def.y_res / 70 

  Do_Precomp_Tree(node) 
End Sub


/'*********************************************************

    Calls the pre-computing routine for each objects in
    the objects tree.  The pre-computing routines figure
    out some sub-expressions that don´t change with
    different calls to the intersection routines.

 *********************************************************'/

Sub Do_Precomp_Tree(node As OBJ_PTR)
  ObjData(node->types).PreComp(node) ' revisar   /' precompute '/

  if (node->child <> NULL) Then /' node has children ? '/
#ifdef ROBUST
      if (node->types <> _BBOX) Then Errors(INTERNAL_ERROR,802) 
#endif

    Do_Precomp(node->child) 
  EndIf
  
  if (node->nextobj <> NULL) Then 
    Do_Precomp(node->nextobj) 
  EndIf
End Sub

/'*********************************************************

    Assigns bounding box values for entire objects tree.
    This must be called once before any ray-tracing is done.

 *********************************************************'/

Sub Make_Bbox(node As OBJ_PTR)
  dim as OBJ_PTR tnode 
  dim as VECTOR  v1,v2 

  if (node->child<>NULL) Then  /' node has children ? '/
#ifdef ROBUST
      if (node->types<>_BBOX) Then Errors(INTERNAL_ERROR,801) 
#endif

    Make_Bbox(node->child) 
  EndIf
  
  if (node->nextobj<>NULL) Then /' node has siblings ? '/
    Make_Bbox(node->nextobj) 
  EndIf
  
  if (node->types=_BBOX) Then 
    tnode=node->child 

    node->lower.x=  BIG
	node->lower.y=  BIG
	node->lower.z=  BIG 
	
    node->upper.x= -BIG
	node->upper.y= -BIG
	node->upper.z= -BIG 

    while (tnode<>NULL)  
      ObjData(tnode->types).FindBbox(@v1,@v2,tnode) ' revisar 
      node->lower.x=MIN(node->lower.x,v1.x) 
      node->lower.y=MIN(node->lower.y,v1.y) 
      node->lower.z=MIN(node->lower.z,v1.z) 

      node->upper.x=MAX(node->upper.x,v2.x) 
      node->upper.y=MAX(node->upper.y,v2.y) 
      node->upper.z=MAX(node->upper.z,v2.z) 

      tnode=tnode->nextobj 
    Wend

  EndIf
End Sub


/'*********************************************************

    Returns pointer to pattern structure given name, or
    null if not found.

 *********************************************************'/

Function find_pat( names As string) As PATTERN_PTR
  dim as PATTERN_PTR pat 

  pat=THEWORLD.patlist 

  while (pat<>NULL)  
    if (strcmp(names,pat->names)=0) Then return(pat) 
    pat=pat->sibling 
  Wend
    
  return(NULL) 
End Function


/'*********************************************************

     Allocates a new pattern structure and returns a
     pointer to it.

 *********************************************************'/

Function new_pat() As PATTERN_PTR
  dim as PATTERN_PTR pat 

  'pat=cast (PATTERN_PTR,allocate(sizeof(PATTERN)))
  pat=new PATTERNS
  if (pat=NULL) Then 
    Errors(MALLOC_FAILURE,802)
  EndIf
  
  pat->names   = "" 
  pat->child   = NULL
  pat->sibling = NULL
  pat->link    = NULL 

  return(pat) 
End Function


/'*********************************************************

     Allocates a new objects structure, stuffs most of
     its information fields, and returns a pointer to it.

 *********************************************************'/

Function new_obj(types As Short ,locs As VECT_PTR ,v1 As VECT_PTR ,v2 As VECT_PTR ,v3 As VECT_PTR ,_
				cinfo As CINFO_PTR, pattern As PATTERN_PTR ,  removes As PATTERN_PTR ,_
				names As string , upper As VECT_PTR , lower As VECT_PTR , _
				cterm As Single , xmult As Single , ymult As Single) As OBJ_PTR
  dim as OBJ_PTR obj 

  'obj=cast(OBJ_PTR,allocate(sizeof(OBJ_STRUCT)))
  obj=new OBJ_STRUCT
  if (obj=NULL) Then 
    Errors(MALLOC_FAILURE,803)
  EndIf

  obj->types=types                             /' copy info '/
  VectEQ(@obj->locs,locs) 
  VectEQ(@obj->vect1,v1) 
  VectEQ(@obj->vect2,v2) 
  VectEQ(@obj->vect3,v3) 
  VectEQ(@obj->lower,lower) 
  VectEQ(@obj->upper,upper) 

  obj->cterm = cterm 
  obj->xmult = xmult 
  obj->ymult = ymult 

  obj->names = names 

  copy_colorinfo(@obj->cinfo,cinfo)        /' colorinfo '/

  obj->nextobj = NULL
  obj->child   = NULL           /' no relatives '/
  obj->pattern = pattern 
  obj->remove  = removes 

  return(obj) 
End Function


/'*********************************************************

                Generates an empty lines

   - changed 13 Mar 89 to include fix by Paul Balyoz to
     intialize cinfo structure.

 *********************************************************'/

Function new_line() As OBJ_PTR 
  dim as CINFOS  cinfo 
  dim as VECTOR  locs,v1,v2,v3, upper, lower 
  dim as OBJ_PTR lines 

  def_colorinfo(@cinfo)       /' initialize cinfo structure '/

  VectEqZero(@locs) 
  VectEqZero(@v1) 
  VectEqZero(@v2) 
  VectEqZero(@v3) 
  VectEqZero(@upper) 
  VectEqZero(@lower) 

  lines=new_obj(_LINE,@locs,@v1,@v2,@v3,@cinfo,NULL,NULL,"", _
               @upper,@lower,0.0,0.0,0.0) 

  lines->flag = FALSE 
  return(lines) 
End Function


/'*********************************************************

                 Adds a lamp to the world

 *********************************************************'/

Sub add_lamp(objects As OBJ_PTR)
   objects->nextobj=THEWORLD.lamps 
   THEWORLD.lamps=objects 
End Sub





/'*********************************************************
            Print #99,s some interesting statistics
 *********************************************************'/
Sub World_Stats() 
  Print #99, "----------------------------------------------"	
	
  Print #99, "World Statistics:" 
  Print #99, "  Objects:      ";THEWORLD.objcount 
  Print #99, "  Lamps:        ";THEWORLD.lampcount 
  Print #99,
	    
  Print #99, "  Intersect tests         : "; THEWORLD.intersect_tests
  Print #99, "  Total Intersections     : ";(THEWORLD.ray_intersects+ THEWORLD.bbox_intersects) 
  Print #99, "     Object intersections : "; THEWORLD.ray_intersects
  Print #99, "     Bbox   intersections : "; THEWORLD.bbox_intersects
  Print #99, "  Rays traced             : ";(THEWORLD.primary_traced+ THEWORLD.to_lamp+ THEWORLD.refl_trans)
  Print #99, "     Primary              : "; THEWORLD.primary_traced
  Print #99, "     To lamps             : "; THEWORLD.to_lamp
  Print #99, "     Refl. or Trans.      : "; THEWORLD.refl_trans
  Print #99, "  Pattern match checks    : "; THEWORLD.pattern_matches
  Print #99,
									  
  Print #99, "  Image statistics"         
  Print #99, "     X Resolution         : ";  def.x_res
  Print #99, "     Y Resolution         : ";  def.y_res
  Print #99, "     Aspect Ratio         : "; def.aspect
  Print #99,

  Print #99, "  Data sent to: ";THEWORLD.outfile 
End Sub




/'*********************************************************
                      Close file
 *********************************************************'/
Sub Close_File() 
  Save_File_BMP() 
End Sub




/' display object '/
Sub Print_Obj(obj as OBJ_PTR)                     
    Print #99, "OBJECT :  type: "
    Select Case (obj->types)
		case _LINE
		    Print #99, "LINE"
		case _SPHERE
		    Print #99, "SPHERE"
		case _PARALLELOGRAM
		    Print #99, "PARALLELOGRAM"
		case _TRIANGLE
		    Print #99, "TRIANGLE"
		case _LAMP
		    Print #99, "LAMP"
		case _OBSERVER
		    Print #99, "OBSERVER"
		'case _GROUND ' no se emplea
		'    Print #99, "GROUND"
		case _SKY
		    Print #99, "SKY"
		case _BBOX
		    Print #99, "BBOX"
		case _RING
		    Print #99, "RING"
		case _QUADRATIC
		    Print #99, "QUADRATIC"
		case else
		    Print #99, "Unknown!"
    End Select

    Print #99, "          loc   : ";_
           (obj->locs.x);_
           (obj->locs.y);_
           (obj->locs.z)

    Print #99, "          vect1 : ";_
           (obj->vect1.x);_
           (obj->vect1.y);_
           (obj->vect1.z)

    Print #99, "          vect2 : ";_
           (obj->vect2.x);_
           (obj->vect2.y);_
           (obj->vect2.z)

    Print #99, "          vect3 : ";_
           (obj->vect3.x);_
           (obj->vect3.y);_
           (obj->vect3.z)
    sleep
End Sub