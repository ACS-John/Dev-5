00010 ! Replace Test\wait
00020 ! -------------------------------------------------------------------
00030   library 'S:\Core\Library': fntop,fntos,fnacs,fnflexinit1,fnflexadd1,fncmdkey,fnpause
00040 ! -------------------------------------------------------------------
00050   dim item$(6)*30,prg$*30,chdr$(6)*20,cm$(6)*2,cap$*128
00060 ! -------------------------------------------------------------------
00070   let fntop(prg$="Test\Wait",cap$="Test 5 second transition between screens")
00080 ! -------------------------------------------------------------------
00085   print 'just before tos - '&time$
00090   let fntos("S-1")
00100   let chdr$(1)="Account" : let chdr$(2)="Name" !:
        let chdr$(3)="Address" : let chdr$(4)="City" !:
        let chdr$(5)="State" : let chdr$(6)="Zip"
00110   let cm$(1)="80" : let cm$(2)="80" : let cm$(3)="80" : let cm$(4)="80" !:
        let cm$(5)="80" : let cm$(6)="80"
00120   let fnflexinit1('flex_test',1,1,10,50,mat chdr$,mat cm$,0,repl=1)
00130   open #1: "Name=Sample\FlexTest,RecL=200,Use",internal,outin 
00140 READ_1: ! !:
        read #1,using "Form POS 1,C 10,2*C 30,C 20,C 2,C 5": mat item$ eof EOF1
00150   let fnflexadd1(mat item$)
00160   goto READ_1
00170 EOF1: close #1: 
00180   let fncmdkey('Ok-1',1,1,0)
00190   let fnacs("S-Flex",0,mat resp$,ck)
00191   print 'just after return from fnacs - '&time$
00200 ! 
00210   let sleep(5) : print "sleep for 5 seconds"
00215   print 'just before tos - '&time$
00220   let fntos(sn$="S-2")
00230   let fncmdkey('Ok-2',1,1,0)
00240   let fnacs(sn$,0,mat resp$,ck)
00241   print 'just after return from fnacs - '&time$
00250 ! 
00260   let sleep(5) : print "sleep for 5 seconds"
00261   print 'just before tos - '&time$
00270   let fntos(sn$="S-3")
00280   let fncmdkey('Ok-3',1,1,0)
00290   let fnacs(sn$,0,mat resp$,ck)
00291   print 'just after return from fnacs - '&time$
00300 ! 
00310   let sleep(5) : print "sleep for 5 seconds"
00311   print 'just before tos - '&time$
00320   let fntos(sn$="S-4")
00330   let fncmdkey('Ok-4',1,1,0)
00340   let fnacs(sn$,0,mat resp$,ck)
00341   print 'just after return from fnacs - '&time$
00350 ! 
00360   stop 
