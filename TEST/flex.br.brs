00010 ! Replace Test\Flex
00020 ! -------------------------------------------------------------------
00030   library 'S:\Core\Library': fntos,fnacs,fnflexinit1,fnflexadd1,fncmdkey,fntop,fnxit,fngethandle
00040 ! -------------------------------------------------------------------
00041   dim resp$(22)*100
00042 ! -------------------------------------------------------------------
00050   fntop(program$,cap$="Test Flex")
00070   dim item$(6)*30,prg$*30,chdr$(6)*20,cm$(6)*2
00072 SCREEN1: ! 
00080   fntos("S-Flex")
00081 ! let use_old=1 : let replace_old=0 : let select_by_row=1
00090 ! chdr$(1)="AccountX" : chdr$(2)="Name" !:
        ! chdr$(3)="Address" : chdr$(4)="City" !:
        ! chdr$(5)="State" : chdr$(6)="Zip"
00100 ! cm$(1)="81" : cm$(2)="81" : cm$(3)="80" : cm$(4)="80" !:
        ! cm$(5)="80" : cm$(6)="80"
00110 ! let fnflexinit1('flex_test',1,1,10,50,mat chdr$,mat cm$,select_by_row,replace_old)
73300   mat chdr$(7) : mat cmask$(7) : mat item$(7)
73310   chdr$(1)='Rec'
73320   chdr$(2)='GL Account'
73330   chdr$(3)='Amount'
73340   chdr$(4)='Description'
73350   chdr$(5)='Invoice'
73360   chdr$(6)='PO Number'
73370   chdr$(7)='PC'
73372   mat cmask$=("")
73380   cmask$(1)='30'
73390   cmask$(2)=''
73400   cmask$(3)='10'
73410   cmask$(5)=''
73900   fnflexinit1('TrAlloc-tran2',17,1,4,90,mat chdr$,mat cmask$,1)
74000   open #h_tralloc=fngethandle: "Name="&env$('Q')&"\CLmstr\TrAlloc.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\TrAlloc-Idx.h"&env$('cno')&",Shr",internal,input,keyed 
74020   do 
74040     read #h_tralloc,using 'Form Pos 1,C 11,C 12,pd 5.2,C 30,G 6,X 3,C 12,G 1': newkey$,item$(2),tmp,item$(4),item$(5),item$(6),item$(7) eof EOF1
74800     allocationstotal+=tmp
74900     let item$(1)=str$(rec(h_tralloc))
75000     let item$(3)=str$(tmp)
75100     fnflexadd1(mat item$)
75200   loop 
75300 EOF1: close #h_tralloc: 
75400   fncmdkey('Ok',1,1,0)
75500   fncmdkey('Cancel',99,0,1)
75600   fnacs("S-Flex",0,mat resp$,ck)
75700   pr "returned response is "&resp$(1)
75800   pr "Press Enter to continue"
75900 ! input fields "1,1,C 1,AE,N": pause$
76000   goto SCREEN1
76100 XIT: let fnxit
