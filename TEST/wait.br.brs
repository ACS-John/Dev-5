00010 ! Replace Test\wait
00020 ! -------------------------------------------------------------------
00030   library 'S:\Core\Library': fntop,fntos,fnacs,fnflexinit1,fnflexadd1,fncmdkey,fnpause
00040 ! -------------------------------------------------------------------
00050   dim item$(6)*30,prg$*30,chdr$(6)*20,cm$(6)*2,cap$*128
00060 ! -------------------------------------------------------------------
00070   fntop(prg$="Test\Wait",cap$="Test 5 second transition between screens")
00080 ! -------------------------------------------------------------------
00085   pr 'just before tos - '&time$
00090   fntos("S-1")
00100   chdr$(1)="Account" : chdr$(2)="Name" !:
        chdr$(3)="Address" : chdr$(4)="City" !:
        chdr$(5)="State" : chdr$(6)="Zip"
00110   cm$(1)="80" : cm$(2)="80" : cm$(3)="80" : cm$(4)="80" !:
        cm$(5)="80" : cm$(6)="80"
00120   fnflexinit1('flex_test',1,1,10,50,mat chdr$,mat cm$,0,repl=1)
00130   open #1: "Name=Sample\FlexTest,RecL=200,Use",internal,outin 
00140 READ_1: ! !:
        read #1,using "Form POS 1,C 10,2*C 30,C 20,C 2,C 5": mat item$ eof EOF1
00150   fnflexadd1(mat item$)
00160   goto READ_1
00170 EOF1: close #1: 
00180   fncmdkey('Ok-1',1,1,0)
00190   fnacs("S-Flex",0,mat resp$,ck)
00191   pr 'just after return from fnacs - '&time$
00200 ! 
00210   sleep(5) : pr "sleep for 5 seconds"
00215   pr 'just before tos - '&time$
00220   fntos(sn$="S-2")
00230   fncmdkey('Ok-2',1,1,0)
00240   fnacs(sn$,0,mat resp$,ck)
00241   pr 'just after return from fnacs - '&time$
00250 ! 
00260   sleep(5) : pr "sleep for 5 seconds"
00261   pr 'just before tos - '&time$
00270   fntos(sn$="S-3")
00280   fncmdkey('Ok-3',1,1,0)
00290   fnacs(sn$,0,mat resp$,ck)
00291   pr 'just after return from fnacs - '&time$
00300 ! 
00310   sleep(5) : pr "sleep for 5 seconds"
00311   pr 'just before tos - '&time$
00320   fntos(sn$="S-4")
00330   fncmdkey('Ok-4',1,1,0)
00340   fnacs(sn$,0,mat resp$,ck)
00341   pr 'just after return from fnacs - '&time$
00350 ! 
00360   stop 
