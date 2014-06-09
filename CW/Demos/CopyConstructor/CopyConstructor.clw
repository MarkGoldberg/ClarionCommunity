
  PROGRAM

OMIT('***')
 * Created with Clarion 9.1
 * User: Mark.Live
 * Date: 6/9/2014
 * Time: 3:38 PM
 * 
 * To change this template use Tools | Options | Coding | Edit Standard Headers.
 ***

  MAP
    MODULE('RTL')
      memcpy(*? CopyTo,*? CopyFrom,UNSIGNED BytesToCopy),*?,PROC,RAW,NAME('_memcpy')
    END
  END


ctYada CLASS,TYPE
X        LONG
Y        LONG
_New     PROCEDURE(LONG XValue, LONG YValue),*ctYada
Copy     PROCEDURE(),*ctYada
ToString PROCEDURE(),STRING
       END

oYada1  ctYada
oYada2  &ctYada     
oYada3  &ctYada
  CODE
  oYada1.X = 47
  oYada1.Y = 100

  oYada2 &= oYada1.Copy()

  oYada3 &= oYada3._New(42, 47)

  MESSAGE('oYada1[ '& oYada1.TOSTRING() &' ]|' & |
          'oYada2[ '& oYada2.TOSTRING() &' ]|' & |
          'oYada3[ '& oYada3.TOSTRING() &' ]')


ctYada._New     PROCEDURE(LONG XValue, LONG YValue)!,*ctYada
Answer  &ctYada
  CODE
  Answer &= NEW ctYada 
  Answer.X = XValue
  Answer.Y = YValue
  RETURN Answer
  

ctYada.Copy PROCEDURE()!,*ctYada
Answer  &ctYada
  CODE
  Answer &= NEW ctYada 
  DO CopyAllProperties
  RETURN Answer

CopyAllProperties ROUTINE

   memcpy( Answer, SELF, SIZE(SELF))
  
  !Answer = SELF !works, but get:  Warning!! : Unusual type conversion

  !Answer.X = SELF.X
  !Answer.Y = SELF.Y

ctYada.ToString PROCEDURE()!,STRING
  CODE
  RETURN 'X['& SELF.X &'] Y['& SELF.Y &']'


 
