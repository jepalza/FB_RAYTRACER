 /'
	estructura reducida para que solo se usen las dos necesarias
	de este modo, es mucho mas simple y peque, y solo saca BMP RAW RGB sin comprimir
 '/

#define WORD unsigned short
#define DWORD unsigned long

Type BITMAPFILEHEADER field=1 
	  As WORD  bfType 
	  As DWORD bfSize 
	  As WORD  bfReserved1 
	  As WORD  bfReserved2 
	  As DWORD bfOffBits 
 End Type 

Type BITMAPINFOHEADER field=1 
	  As DWORD biSize 
	  As LONG  biWidth 
	  As LONG  biHeight 
	  As WORD  biPlanes 
	  As WORD  biBitCount 
	  As DWORD biCompression 
	  As DWORD biSizeImage 
	  As LONG  biXPelsPerMeter 
	  As LONG  biYPelsPerMeter 
	  As DWORD biClrUsed 
	  As DWORD biClrImportant 
 End Type 

#define BI_RGB       0             /' No compression - straight BGR data '/
