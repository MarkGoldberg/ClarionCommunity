

!=========================================================================
!=========================================================================
!=========================================================================
!========                                                      ===========
!========               File Definitions                       ===========
!========                                                      ===========
!=========================================================================
!=========================================================================
!=========================================================================

FileName STRING(File:MaxFileName)            ! File name for input and output files


! LIBfile is used to read and write import library files

LIBfile  FILE,DRIVER('DOS','/FILEBUFFERS=20'),PRE(LIB),CREATE,NAME(FileName)
Rec           RECORD
RawBytes     BYTE,DIM(1024)
header       GROUP,OVER(RawBytes)
typ            BYTE             ! OMF record type = 88H (Coment)
len            USHORT           ! Size of OMF record to follow
kind           USHORT           ! Comment kind = 0A000H
bla            BYTE             ! Always 1 for our purposes
ordflag        BYTE             ! ditto
             END
! For the records we want, the header is follower by the pubname
! and modname in PSTRING format, then the ordinal export number (USHORT)

pstringval   PSTRING(128),OVER(RawBytes)
ushortval    USHORT,OVER(RawBytes)
           END
         END

! EXEfile is used for reading NE and PE format executable files

EXEfile  FILE,DRIVER('DOS','/FILEBUFFERS=20'),PRE(EXE),NAME(FileName)
Rec           RECORD
RawBytes     BYTE,DIM(1024)
cstringval   CSTRING(128),OVER(RawBytes)
pstringval   PSTRING(128),OVER(RawBytes)
ulongval     ULONG,OVER(RawBytes)
ushortval    USHORT,OVER(RawBytes)

! DOSheader is the old exe (stub) header format
DOSheader    GROUP,OVER(RawBytes)
dos_magic      STRING(2)         ! contains 'MZ'
dos_filler     USHORT,DIM(29)    ! we don't care about these fields
dos_lfanew     ULONG             ! File offset of new exe header
             END

! NEheader is the new exe (16-bit) header format
NEheader     GROUP,OVER(RawBytes)
ne_magic       STRING(2)         ! Contains 'NE'
ne_ver         BYTE
ne_rev         BYTE
ne_enttab      USHORT
ne_cbenttab    USHORT
ne_crc         LONG
ne_flags       USHORT
ne_autodata    USHORT
ne_heap        USHORT
ne_stack       USHORT
ne_csip        ULONG
ne_sssp        ULONG
ne_cseg        USHORT
ne_cmod        USHORT
ne_cbnrestab   USHORT
ne_segtab      USHORT
ne_rsrctab     USHORT
ne_restab      USHORT
ne_modtab      USHORT
ne_imptab      USHORT
ne_nrestab     ULONG
ne_cmovent     USHORT
ne_align       USHORT
ne_rescount    USHORT
ne_osys        BYTE
ne_flagsother  BYTE
ne_gangstart   USHORT
ne_ganglength  USHORT
ne_swaparea    USHORT
ne_expver      USHORT           ! Expected Window version number
             END

! PEheader is the flat-model (32-bit) header format (PE signature)
PEheader     GROUP,OVER(RawBytes)
pe_signature   ULONG
pe_machine     USHORT
pe_nsect       USHORT
pe_stamp       ULONG
pe_psymbol     ULONG
pe_nsymbol     ULONG
pe_optsize     USHORT
pe_character   USHORT
             END

! Optheader is the "optional header" that follows the PEheader

OptHeader    GROUP,OVER(RawBytes)
opt_Magic          USHORT
opt_MajorLinkerVer BYTE
opt_MinorLinkerVer BYTE
opt_SizeOfCode     ULONG
opt_SizeOfInitData ULONG
opt_SizeOfUninit   ULONG
opt_EntryPoint     ULONG
opt_BaseOfCode     ULONG
opt_BaseOfData     ULONG
opt_ImageBase      ULONG
opt_SectAlignment  ULONG
opt_FileAlignment  ULONG
opt_MajorOSVer     USHORT
opt_MinorOSVer     USHORT
opt_MajorImageVer  USHORT
opt_MinorImageVer  USHORT
opt_MajorSubVer    USHORT
opt_MinorSubVer    USHORT
opt_Reserved1      ULONG
opt_SizeOfImage    ULONG
opt_SizeOfHeaders  ULONG
opt_CheckSum       ULONG
opt_Subsystem      USHORT
opt_DllChar        USHORT
opt_StackReserve   ULONG
opt_StackCommit    ULONG
opt_HeapReserve    ULONG
opt_HeapCommit     ULONG
opt_LoaderFlags    ULONG
opt_DataDirNum     ULONG
             END

! The Optional header is followed by an array of the following structures

DataDir      GROUP,OVER(RawBytes)
data_VirtualAddr   ULONG
data_Size          ULONG
             END

! SectHeader describes a section in a PE file
SectHeader GROUP,OVER(RawBytes)
sh_SectName  CSTRING(8)
sh_VirtSize  ULONG
sh_PhysAddr  ULONG,OVER(sh_VirtSize)
sh_VirtAddr  ULONG
sh_RawSize   ULONG
sh_RawPtr    ULONG
sh_Reloc     ULONG
sh_LineNum   ULONG
sh_RelCount  USHORT
sh_LineCount USHORT
sh_Character ULONG
           END

! ExpDirectory is at start of a .edata section in a PE file
ExpDirectory GROUP,OVER(RawBytes)
exp_Character   ULONG
exp_stamp       ULONG
exp_Major       USHORT
exp_Minor       USHORT
exp_Name        ULONG
exp_Base        ULONG
exp_NumFuncs    ULONG
exp_NumNames    ULONG
exp_AddrFuncs   ULONG
exp_AddrNames   ULONG
exp_AddrOrds    ULONG
              END
            END
          END

newoffset ULONG   ! File offset to NE/PE header

! vim: set filetype=clarion:
