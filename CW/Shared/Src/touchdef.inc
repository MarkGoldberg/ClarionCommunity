! --- Pointer input types --------------------------------------------

PointerTypes       ITEMIZE(1)            ! Possible values of TouchPoint.PTType
PT_POINTER           EQUATE              ! Generic
PT_TOUCH             EQUATE              ! Touch
PT_PEN               EQUATE              ! Pen
PT_MOUSE             EQUATE              ! Mouse
                   END

! --- Touch action constance -----------------------------------------

TouchAction        ITEMIZE(0)            ! Possible values of TouchPoint.ia
TOUCH_UNDEFINED      EQUATE
TOUCH_ENTER          EQUATE
TOUCH_LEAVE          EQUATE
TOUCH_ACTIVATE       EQUATE
TOUCH_DOWN           EQUATE
TOUCH_UP             EQUATE
TOUCH_UPDATE         EQUATE
TOUCH_WHEEL          EQUATE
                   END

! --- Input action constants -----------------------------------------

InputAction        ITEMIZE(0)            ! Possible values of InputData.ia
INPUT_UNDEFINED      EQUATE
INPUT_ENTER          EQUATE
INPUT_LEAVE          EQUATE

! === Drag-n-drop

INPUT_STARTDRAGGING  EQUATE
INPUT_DRAGGING       EQUATE
INPUT_ENDDRAGGING    EQUATE

! === Mouse input

MOUSE_DOWN           EQUATE
MOUSE_UP             EQUATE
MOUSE_DBLCLICK       EQUATE
MOUSE_HOVER          EQUATE
MOUSE_WHEEL          EQUATE

! === Touch gestures

GESTURE_PAN          EQUATE
GESTURE_TAP          EQUATE
GESTURE_DBLTAP       EQUATE
GESTURE_PRESSANDTAP  EQUATE
GESTURE_ZOOMIN       EQUATE
GESTURE_ZOOMOUT      EQUATE
GESTURE_ROTATE       EQUATE
                   END

! --- Input flags ----------------------------------------------------

InputFlags         ITEMIZE               ! Flags for InputData.buttons, InputData.param, TouchPoint.buttons, TouchPoint.param
INFLAG_LBUTTON       EQUATE (0001h)      ! First button
INFLAG_RBUTTON       EQUATE (0002h)      ! Second button
INFLAG_MBUTTON       EQUATE (0010h)      ! Third button
INFLAG_XBUTTON1      EQUATE (0020h)      ! Fourth button
INFLAG_XBUTTON2      EQUATE (0040h)      ! Fifth button

INFLAG_CONTROL       EQUATE (0008h)      ! Ctrl button is down during event
INFLAG_SHIFT         EQUATE (0004h)      ! Shift button is down during event

INFLAG_BEGIN         EQUATE (0100h)      ! Gesture begins
INFLAG_INERTIA       EQUATE (0200h)      ! Pan gesture has inertia information
INFLAG_END           EQUATE (0400h)      ! Gesture ends
INFLAG_CAPTURED      EQUATE (1000h)      ! Capture active during event
INFLAG_VERTICAL      EQUATE (2000h)      ! Panning or wheel rotation has vertical component
INFLAG_HORIZONTAL    EQUATE (4000h)      ! Panning or wheel rotation has horizontal component
INFLAG_TWOFINGERS    EQUATE (8000h)      ! Panning is using two fingers
                   END

! --- Point flags ----------------------------------------------------

PointFlags         ITEMIZE               ! Flags for the PTFlags field of TouchPoint
PTFLAG_NONE          EQUATE (0)
PTFLAG_NEW           EQUATE (00000001h)  ! New pointer
PTFLAG_INRANGE       EQUATE (00000002h)  ! Pointer still exists
PTFLAG_INCONTACT     EQUATE (00000004h)  ! Pointer is hovering if not set
PTFLAG_PRIMARY       EQUATE (00002000h)  ! Primary pointer of simultaneously acting pointers
PTFLAG_DOWN          EQUATE (00010000h)  ! Pointer is transitioned to a down state
PTFLAG_UPDATE        EQUATE (00020000h)  ! Pointer is updated
PTFLAG_UP            EQUATE (00040000h)  ! Pointer is transitioned to an up state
PTFLAG_CANCELED      EQUATE (00008000h)  ! Pointer is departing in abnormal manner
PTFLAG_WHEEL         EQUATE (00080000h)  ! Input is associated with a pointer wheel
PTFLAG_HWHEEL        EQUATE (00100000h)  ! Input is associated with a horizontal pointer wheel
                 END
