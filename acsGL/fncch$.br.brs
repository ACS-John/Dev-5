00010 ! Replace S:\acsGL\fnCCH$
00020 !
00030   def library fncch$*20(;cch$*20)
00040     library 'S:\Core\Library': fncno,fngethandle
00050     dim cch$*20
00060     fncno(cno)
00070     get=1 : put=2
00080     if trim$(cch$)="" then get_or_put=get else get_or_put=put
00090     open #tmp=fngethandle: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,outIn,relative 
00100     if get_or_put=get then !:
            read #tmp,using "Form POS 276,C 20",rec=1: cch$ noRec CLOSE_TMP
00110     if get_or_put=put then !:
            rewrite #tmp,using "Form Pos 276,C 20",rec=1: cch$
00120 CLOSE_TMP: close #tmp: 
00130     fncch$=cch$
00140 XIT: ! 
00150   fnend 
