

#Include "bmp.bi"

' jepalza,26
' salida BMP 24 RAW RGB
' rutina BMP cambiando todo lo relacionado con el formato TARGA original

ReDim Shared As ubyte Buff_BMP(16) 
Dim Shared As integer Outs 

Dim Shared As Integer windowWidth 
Dim Shared As Integer windowHeight 
	
Dim Shared As Long fila_BMP 

Sub Dump_Line_BMP(lineno As Integer , r() As short , g() As short , b() As short )
	Dim As Integer i 
	Static As Integer y=0 
	fila_BMP-=windowWidth*3  ' empieza por la fila inferior a la hora de guardar las filas BMP
	for i=0 To def.x_res-1       
		Buff_BMP(fila_BMP)=cbyte(b(i)): fila_BMP+=1 
		Buff_BMP(fila_BMP)=cbyte(g(i)): fila_BMP+=1 
		Buff_BMP(fila_BMP)=cbyte(r(i)): fila_BMP+=1 
		if display then
			pset(i,y),RGB(r(i),g(i),b(i))
		endif
    Next
	y+=1
	fila_BMP-=windowWidth*3  ' y voy decrementando
End Sub

Sub Save_File_BMP()
	for f as integer=0 to ubound(Buff_BMP)
		put #Outs,,Buff_BMP(f)
	next
	close (Outs) 
End Sub

Sub Open_File_BMP()
	windowWidth  = def.x_res 
	windowHeight = def.y_res 

	Redim Buff_BMP(windowWidth*windowHeight*3) 

	Outs=freefile()
	open THEWORLD.outfile for binary as Outs
	if (Outs=0) Then Print #99, "Error creando BMP":beep:sleep:end
  
	fila_BMP=windowHeight*windowWidth*3  ' para el BMP debo empezar por abajo hacia arriba al guardar las filas

	dim as BITMAPFILEHEADER bitmapFileHeader 
		bitmapFileHeader.bfType = &h4D42  ' MB->BM->ID BMP
		bitmapFileHeader.bfSize = windowWidth * windowHeight * 3 
		bitmapFileHeader.bfReserved1 = 0 
		bitmapFileHeader.bfReserved2 = 0 
		bitmapFileHeader.bfOffBits = sizeof(BITMAPFILEHEADER) + sizeof(BITMAPINFOHEADER) 

	dim as BITMAPINFOHEADER bitmapInfoHeader 
		bitmapInfoHeader.biSize = sizeof(BITMAPINFOHEADER) 
		bitmapInfoHeader.biWidth = windowWidth - 1 
		bitmapInfoHeader.biHeight = windowHeight - 1 
		bitmapInfoHeader.biPlanes = 1 
		bitmapInfoHeader.biBitCount = 24 
		bitmapInfoHeader.biCompression = BI_RGB 
		bitmapInfoHeader.biSizeImage = 0 
		bitmapInfoHeader.biXPelsPerMeter = 0  ' ?
		bitmapInfoHeader.biYPelsPerMeter = 0  ' ?
		bitmapInfoHeader.biClrUsed = 0 
		bitmapInfoHeader.biClrImportant = 0 

	put #Outs,,bitmapFileHeader
	put #Outs,,bitmapInfoHeader
End Sub

