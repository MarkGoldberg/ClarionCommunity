
  INCLUDE(   'CWTouch.inc'),ONCE
 !INCLUDE('CWTouchDef.inc'),ONCE  implied

TouchHelper CLASS,TYPE,MODULE('TouchHelper.clw'),LINK('TouchHelper.clw')  
TouchKeyboard              CSTRING(FILE:MaxFileName + 1)
!------------------
CONSTRUCT                  PROCEDURE
ShowTouchKeyboard          PROCEDURE() 

PointerTypes_ToString      PROCEDURE(SIGNED xPointerType),STRING
TouchAction_ToString       PROCEDURE(SIGNED xTouchAction),STRING !Guessing at DataType
InputAction_ToString       PROCEDURE(SIGNED xInputAction),STRING
InputFlags_ToString        PROCEDURE(LONG xFlag),STRING
PointFlags_ToString        PROCEDURE(LONG xFlags),STRING
                     
InputData_ToString         PROCEDURE(CONST *InputData xInputData),STRING
TouchPoint_ToString        PROCEDURE(CONST *TouchPoint xTouchPoint),STRING    

Point_ToString             PROCEDURE(              CONST *TPOINT    xPt),STRING
Point_ToStringWhenNotZero  PROCEDURE(STRING xName, CONST *TPOINT    xPt),STRING
ShowWhenNotZero            PROCEDURE(STRING xName, ? xValue),STRING


             END