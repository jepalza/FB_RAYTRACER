 /'*********************************************************

      Routines to offset primitives by dx, dy, dz
      (used for user defined primitive types)

 *********************************************************'/


/'*********************************************************

    This will offset most types of objects that use
    the ´loc´ vector for their location.

 *********************************************************'/

Sub Standard_Offset(obj As OBJ_PTR , offset As VECT_PTR)
# ifdef ROBUST
    if ((obj->types = _SPHERE) OrElse (obj->types = _PARALLELOGRAM) OrElse _
		(obj->types = _RING) OrElse (obj->types = _TRIANGLE) OrElse (obj->types = _QUADRATIC))=0 Then 
      Errors(INTERNAL_ERROR,1301)
    EndIf
# endif

  /'* now add the offset (this is a tough one) *'/
  VectorAdd(@obj->locs, @obj->locs,offset) 
End Sub


/'*********************************************************

  This will offset _BBOX type objects.  It actually
  does nothing, since bbox values are filled after
  the tree is built.

 *********************************************************'/

Sub Offset_Bbox(obj As OBJ_PTR , offset As VECT_PTR)
# ifdef ROBUST
    if (obj->types <> _BBOX) Then Errors(INTERNAL_ERROR,1302) 
# endif
End Sub

