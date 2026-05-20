 
' #Include "qrt.bi"

'Dim Shared As Integer linenumber 

/'*********************************************************

         Here if pointer to fn goes astray
         (from ObjInfo structure)

 *********************************************************'/

'Sub RayErr() 
'  Errors(INTERNAL_ERROR,9999) 
'End Sub

/'*********************************************************

      Print #99,s warning message to stdio.  These are not
      critical; ray tracing continues.

 *********************************************************'/

Sub Warning(num As Integer ,  messg As string)

  if (linenumber >=0) Then Print #99, "Input lines number ",linenumber
  
  Print #99, "Warning: "

  Select Case  (num)  
	case OBSOLETE_OPTION
		Print #99, "obsolete option"

	case else
		Print #99, "programmer stupidity error" 
  
  End Select

  if (messg <> "") Then Print #99, messg

End Sub

/'*********************************************************

      Errors reporting routine. Dumps user back DOS

 *********************************************************'/

Sub Errors(num As Integer , code As Integer)
  if (linenumber >= 0) Then 
    Print #99, "Input lines number ",linenumber
  EndIf
  

  Print #99, "Fatal error code : ",code

  Select Case  (num)  
	case ILLEGAL_PARAMETER
		Print #99, "illegal parameter" 

	case TOO_FEW_PARMS
		Print #99, "too few parameters" 

	case ILLEGAL_OBJECT
		Print #99, "illegal objects type" 

	case MALLOC_FAILURE
		Print #99, "malloc failure" 

	case SYNTAX_ERROR
		Print #99, "syntax error" 

	case INTERNAL_ERROR
		Print #99, "programmer confusion error" 

	case FILE_ERROR
		Print #99, "file error" 

	case PATTERN_NOT_FOUND
		Print #99, "pattern not found" 

	case PATTERN_EXISTS
		Print #99, "pattern already defined" 

	case NO_OBSERVER
		Print #99, "no observer defined" 

	case UNDEFINED_PARAM
		Print #99, "undefined parameter" 

	case NON_HOMOGENIOUS
		Print #99, "world contains non-homogenious objects" 

	case ZERO_INDEX
		Print #99, "an index of refraction is 0" 

	case COLOR_VALUE_ERR
		Print #99, "illegal color_info value" 

	case LESS_THAN_ZERO
		Print #99, "parameter should be >= 0"

	case ZERO_MULTIPLIER
		Print #99, "a pattern multiplier is 0"

	case UNDEFINED_NAME
		Print #99, "undefined name"

	case LPAREN_EXPECTED
		Print #99, "left paren expected"

	case RPAREN_EXPECTED
		Print #99, "right paren expected"

	case ILLEGAL_VECTOR
		Print #99, "illegal vector structure"

	case ILLEGAL_SVECTOR
		Print #99, "illegal colors triple"

	case ILLEGAL_OPTION
		Print #99, "illegal command lines option"

	case else
		Print #99, "programmer stupidity error"

  
 End Select

  beep:sleep:end
End Sub

