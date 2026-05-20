#include "qrt.bi"
#include "pattern.bi"

#include "declares.bi"

/'*********************************************************
   Contains actual instantiations of some variables
 *********************************************************'/
#define MAXSCENE 16*1024 ' maximo de lineas por fichero (suficiente?)
Dim Shared As String  qrtscene(MAXSCENE)  ' contiene el fichero QRT de escena al completo
Dim Shared As integer scenechar     /' total caracteres leidos del QRT de entrada '/
Dim Shared As string  FileIn        /' fichero de entrada '/
Dim Shared As Long    reg           /' registro contador de posicion de caracter actual '/
Dim Shared As Integer linenumber    /' lineas contador '/
Dim Shared As Integer display       /' visualizar salida grafica '/


/'*********************************************************
   Table of pointers to functions for objects. There are
   six functions per objects (see qrt.h)
 *********************************************************'/
' NOTA: no h elogrado implementar la rutina RayErr() , por lo que la dejo como NULL
'       debido a ello, si un elemento no es reconocido, no se detiene y puede fallar
Dim Shared As OBJ_DATA ObjData(11) 
/' nothing       '/
    ObjData(0).ColTest =Null '@RayErr()                 
    ObjData(0).FindNorm=Null '@RayErr()
    ObjData(0).FindBbox=Null '@RayErr()
    ObjData(0).RelPos  =Null '@RayErr()
    ObjData(0).PreComp =@PreCompNull()
    ObjData(0).Offset  =Null '@RayErr()
    ObjData(0).Resize  =Null '@RayErr()
/' LINEA          '/
    ObjData(1).ColTest= Null '@RayErr()                 
    ObjData(1).FindNorm=Null '@RayErr()
    ObjData(1).FindBbox=Null '@RayErr()
    ObjData(1).RelPos=  Null '@RayErr()
    ObjData(1).PreComp= @PreCompNull()
    ObjData(1).Offset=  Null '@RayErr()
    ObjData(1).Resize=  Null '@RayErr()
/' SPHERE        '/
    ObjData(2).ColTest= @LineSphere()          
    ObjData(2).FindNorm=@SphereNorm()
    ObjData(2).FindBbox=@BboxSphere()
    ObjData(2).RelPos=  @Sphere_Pos()
    ObjData(2).PreComp= @PreCompSphere()
    ObjData(2).Offset=  @Standard_Offset()
    ObjData(2).Resize=  @Resize_Sphere()
/' PARALLELOGRAM '/
    ObjData(3).ColTest= @LineParallelogram()   
    ObjData(3).FindNorm=@PlaneNorm()
    ObjData(3).FindBbox=@BboxParallelogram()
    ObjData(3).RelPos=  @Plane_Pos()
    ObjData(3).PreComp= @PreCompPlane()
    ObjData(3).Offset=  @Standard_Offset()
    ObjData(3).Resize=  @Resize_Plane()
/' TRIANGLE      '/
    ObjData(4).ColTest= @LineTriangle()        
    ObjData(4).FindNorm=@PlaneNorm()
    ObjData(4).FindBbox=@BboxTriangle()
    ObjData(4).RelPos=  @Plane_Pos()
    ObjData(4).PreComp= @PreCompPlane()
    ObjData(4).Offset=  @Standard_Offset()
    ObjData(4).Resize=  @Resize_Plane()
/' LAMP          '/
    ObjData(5).ColTest= @LineSphere()          
    ObjData(5).FindNorm=@SphereNorm()
    ObjData(5).FindBbox=@BboxSphere()
    ObjData(5).RelPos=  Null '@RayErr()
    ObjData(5).PreComp= @PreCompNull()
    ObjData(5).Offset=  Null '@RayErr()
    ObjData(5).Resize=  Null '@RayErr()
/' OBSERVER      '/
    ObjData(6).ColTest= Null '@RayErr()                 
    ObjData(6).FindNorm=Null '@RayErr()
    ObjData(6).FindBbox=Null '@RayErr()
    ObjData(6).RelPos=  Null '@RayErr()
    ObjData(6).PreComp= @PreCompNull()
    ObjData(6).Offset=  Null '@RayErr()
    ObjData(6).Resize=  Null '@RayErr()
/' GROUND  ************ NO EMPLEADO ***********      '/
'    ObjData(7).ColTest= Null '@RayErr()                 
'    ObjData(7).FindNorm=Null '@RayErr()
'    ObjData(7).FindBbox=Null '@RayErr()
'    ObjData(7).RelPos=  Null '@RayErr()
'    ObjData(7).PreComp= @PreCompNull()
'    ObjData(7).Offset=  Null '@RayErr()
'    ObjData(7).Resize=  Null '@RayErr()
/' SKY           '/
    ObjData(8).ColTest= Null '@RayErr()                 
    ObjData(8).FindNorm=Null '@RayErr()
    ObjData(8).FindBbox=Null '@RayErr()
    ObjData(8).RelPos=  Null '@RayErr()
    ObjData(8).PreComp= @PreCompNull()
    ObjData(8).Offset=  Null '@RayErr()
    ObjData(8).Resize=  Null '@RayErr()
/' BBOX          '/
    ObjData(9).ColTest= @LineBbox()            
    ObjData(9).FindNorm=Null '@RayErr()
    ObjData(9).FindBbox=@BboxBbox()
    ObjData(9).RelPos=  Null '@RayErr()
    ObjData(9).PreComp= @PreCompNull()
    ObjData(9).Offset=  @Offset_Bbox()
    ObjData(9).Resize=  @Resize_Bbox()
/' RING          '/
    ObjData(10).ColTest= @LineRing()            
    ObjData(10).FindNorm=@PlaneNorm()
    ObjData(10).FindBbox=@BboxRing()
    ObjData(10).RelPos=  @Plane_Pos()
    ObjData(10).PreComp= @PreCompPlane()
    ObjData(10).Offset=  @Standard_Offset()
    ObjData(10).Resize=  @Resize_Plane()
/' QUADRATIC     '/
    ObjData(11).ColTest= @LineQuadratic()       
    ObjData(11).FindNorm=@QuadraticNorm()
    ObjData(11).FindBbox=@BboxQuadratic()
    ObjData(11).RelPos=  @Quadratic_Pos()
    ObjData(11).PreComp= @PreCompQuadratic()
    ObjData(11).Offset=  @Standard_Offset()
    ObjData(11).Resize=  @Resize_Quadratic()


/'*********************************************************
   Table of pointers to functions for patterns.
   (see pattern.h)
 *********************************************************'/
  PattData(0).PattHit=Null '@RayErr()
  PattData(1).PattHit=@Rect_Hit()
  PattData(2).PattHit=@Circle_Hit()
  PattData(3).PattHit=@Poly_Hit()



