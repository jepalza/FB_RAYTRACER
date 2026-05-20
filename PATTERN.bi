 
/'*********************************************************

                HEADER FILE FOR PATTERNS

 *********************************************************'/

#define PATT_HEADER       0             /' header    '/
#define RECT_PATTERN      1             /' rectangle '/
#define CIRCLE_PATTERN    2             /' circle    '/
#define POLY_PATTERN      3             /' polygon   '/


/'*********************************************************

            FUNCTIONS FOR PATTERN INTERSECTIONS

 *********************************************************'/

Type PATT_DATA 
  PattHit As Function(pos1 As Single , pos2 As Single , patt As PATTERN_PTR) As integer' did we hit the pattern ? 
End Type 

Dim Shared As PATT_DATA PattData(3) 


