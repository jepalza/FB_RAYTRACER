 
/'*********************************************************

    Routines for user-defined primitives (INSTANCE_OF)

 *********************************************************'/


/'*********************************************************

     Returns a pointer to the objects in the objects
     tree with the given name.

 *********************************************************'/


Function Name_Find(obj As OBJ_PTR, names As string) As OBJ_PTR
  dim as OBJ_PTR temp 

  if (obj=NULL) Then return(NULL) 

  if (obj->names<>"") Then 
    if (strcmp(names,obj->names)=0) Then return(obj)
  EndIf
  
  if (obj->child <> NULL) Then 
	temp=Name_Find(obj->child,names)
    if (temp<>NULL) Then 
      return(temp)
    EndIf
  EndIf
  
  if (obj->nextobj <> NULL) Then 
	temp=Name_Find(obj->nextobj,names)
    if (temp<>NULL) Then 
      return(temp)
    EndIf
  EndIf
  
  return(NULL) 
End Function


/'*********************************************************

    Copies a given sub-tree and returns a pointer to
    the copy.    Always pass fflag=TRUE

 *********************************************************'/

Function Subtree_Copy(obj As OBJ_PTR , fflag As Integer) As OBJ_PTR
  dim as OBJ_PTR newobj

  if (obj=NULL) Then return(NULL) 

  newobj = new_obj( obj->types,_        /' make new obj '/
                    @obj->locs,_
                    @obj->vect1,_
                    @obj->vect2,_
                    @obj->vect3,_
                    @obj->cinfo,_
                    obj->pattern,_
                    obj->remove,_
                    "",_              /' no name here '/
                    @obj->upper,_
                    @obj->lower,_
                    obj->cterm,_
                    obj->xmult,_
                    obj->ymult ) 

  newobj->child   = Subtree_Copy(obj->child,FALSE) 

  if ( fflag=0) Then 
    newobj->nextobj = Subtree_Copy(obj->nextobj,FALSE) 
  else
    newobj->nextobj = NULL
  EndIf
  
  return(newobj) 
End Function

/'*********************************************************

          Offsets sub-tree by offset distance units
          ALWAYS pass fflag = TRUE

 *********************************************************'/

Sub Subtree_Offset(obj As OBJ_PTR , offset As VECT_PTR , fflag As Integer)
  if (obj=NULL) Then return 

  ObjData(obj->types).Offset(obj,offset) ' revisar 

  Subtree_Offset(obj->child,offset,FALSE) 

  if (fflag=0) Then 
    Subtree_Offset(obj->nextobj,offset,FALSE)
  EndIf
End Sub

/'*********************************************************

          Scales a sub-tree by mult distance units
          ALWAYS pass fflag = TRUE

 *********************************************************'/

Sub Subtree_Scale(obj As OBJ_PTR , mult As VECT_PTR , fflag As Integer)
  if (obj=NULL) Then return 

  ObjData(obj->types).Resize(obj,mult) ' revisar 

  Subtree_Scale(obj->child,mult,FALSE) 

  if (fflag=0) Then 
    Subtree_Scale(obj->nextobj,mult,FALSE)
  EndIf
  
End Sub

/'*********************************************************

  Load an "Instance_of" structure.  This really makes a
  copy of another part of the tree, offsets it, and returns
  a pointer to this copy.  All "name" fields for the copy
  are set to null to avoid duplicate names.  Also, a new
  scale factor can be specified (default 1).

 *********************************************************'/

Function Get_Instance_Of() As OBJ_PTR
  Dim As string strs'=space(SLEN) 
  Dim As string names 
  Dim As Integer ends, f, found 
  Dim As VECTOR offset, mult 
  Dim As OBJ_PTR source, dest 

  ends=0
  f=0 
  mult.x = 1.00 
  mult.y = 1.00
  mult.z = 1.00 

  GetLeftParen() 

  while (ends=0) andalso (reg>-1) 
    GetToken(strs) 

    found = FALSE 
    if (strcmp(strs,"NAME")=0) Then 
      names = Get_Next_Name() 
      f Or= 1: found = TRUE 
    EndIf
  
    if ((strcmp(strs,"POS")=0) OrElse (strcmp(strs,"LOC")=0) OrElse _
		(strcmp(strs,"POSITION")=0) OrElse (strcmp(strs,"LOCATION")=0)) Then 
      GetVector(@offset) 
      f Or= 2: found = TRUE 
    EndIf
  
    if (strcmp(strs,"SCALE")=0) Then 
      GetVector(@mult) 
      found = TRUE 
    EndIf
  
    if (strcmp(strs,")")=0) Then 
       ends=1: found = TRUE  
    EndIf
  
    if ( found=0) Then Errors(UNDEFINED_PARAM,1203) 
 Wend

  if (f<>3) Then Errors(TOO_FEW_PARMS,1204) 

  source = Name_Find(THEWORLD.instances,names)
  if (source=NULL) Then 
    Errors(UNDEFINED_NAME,1205)
  EndIf
  
  dest = Subtree_Copy(source,TRUE)
  if (dest=NULL) Then 
    Errors(INTERNAL_ERROR,1206)
  EndIf
  
  /' scale the subtree '/
  Subtree_Scale(dest,@mult,TRUE) 

  /' move the subtree '/
  Subtree_Offset(dest,@offset,TRUE) 

  return(dest) 
End Function


