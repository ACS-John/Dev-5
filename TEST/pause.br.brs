00010 ! Replace Test\Pause
00020 ! -------------------------------------------------------------------
00030   library 'S:\Core\Library': fntop,fnTos,fnAcs,fnflexinit1,fnflexadd1,fnCmdKey,fnpause
00040 ! -------------------------------------------------------------------
00050   dim item$(6)*30,prg$*30,chdr$(6)*20,cm$(6)*2
00060 ! -------------------------------------------------------------------
00070   fntop(prg$="Sample\Flex",cap$="Sample Flex")
00080 ! -------------------------------------------------------------------
00090   fnTos("S-Flex")
00100   chdr$(1)="Account" : chdr$(2)="Name" !:
        chdr$(3)="Address" : chdr$(4)="City" !:
        chdr$(5)="State" : chdr$(6)="Zip"
00110   cm$(1)="80" : cm$(2)="80" : cm$(3)="80" : cm$(4)="80" !:
        cm$(5)="80" : cm$(6)="80"
00120   fnflexinit1('flex_test',1,1,10,50,mat chdr$,mat cm$,0,repl=1)
00130   open #1: "Name=Sample\FlexTest,RecL=200,Use",internal,outIn 
00140 READ_1: ! !:
        read #1,using "Form POS 1,C 10,2*C 30,C 20,C 2,C 5": mat item$ eof EOF1
00150   fnflexadd1(mat item$)
00160   goto READ_1
00170 EOF1: close #1: 
00180   fnCmdKey('Ok',1,1,0)
00190   fnCmdKey('Cancel',99,0,1)
00200   fnAcs("S-Flex",0,mat resp$,ck)
00210   pr "returned response is "&resp$(1) !:
        pr "Press Enter to continue"
00220   fnpause
00230   stop 
