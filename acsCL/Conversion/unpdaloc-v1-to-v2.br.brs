00010 ! Replace R:\acsCL\Conversion\UnPdAloc-v1-to-v2
00020 ! converts the CL UnPdAloc file from version 0 or version 1 to Version 2
00030   def library fnunpdaloc_v1_to_v2
00040     library 'R:\Core\Library': fntop,fnxit, fncno,fnerror,fnmsgbox,fnwait,fncopy,fnindex_it,fnstatus
00050     on error goto ERTN
00060 ! ______________________________________________________________________
00070     dim cnam$*40,cap$*128,message$*40,msgline$(6)*48,response$(5)*1
00080 ! ______________________________________________________________________
00090     let fncno(cno,cnam$)
00100     let cap$="Check Book update UnPdAloc from v1 to v2"
00120     let fnstatus("Payment Allocation file until it is updating from v1 to v2")
00180     open #unpdaloc=1: "Name=Q:\CLmstr\UnPdAloc.h"&str$(cno)&",KFName=Q:\CLmstr\UAIdx1.h"&str$(cno),internal,outin,keyed 
00182 !      close #unpdaloc:
00190     if version(unpdaloc)=2 then let fnstatus("UnPdAloc is already version 2") : goto XIT
00200     let version(unpdaloc,2)
00210     close #unpdaloc: 
00220 ! 
00230 ! change the record length
00240     let fncopy("Q:\CLmstr\UnPdAloc.h"&str$(cno),"Q:\CLmstr\UnPdAloc.h"&str$(cno),67)
00270 ! 
00280 ! make sure the Key is right justified
00290     open #unpdaloc=1: "Name=Q:\CLmstr\UnPdAloc.h"&str$(cno)&",KFName=Q:\CLmstr\UAIdx1.h"&str$(cno),internal,outin,keyed 
00300     for j=1 to lrec(unpdaloc)
00310       read #unpdaloc,using 'Form Pos 1,C 8,c 12',rec=j: vn$,iv$ norec L330
00315       let vn$=lpad$(rtrm$(vn$),8): let iv$=lpad$(rtrm$(iv$),12)
00320       rewrite #unpdaloc,using 'Form Pos 1,Cr 8,c 12',rec=j: vn$,iv$
00330 L330: next j
00340     close #unpdaloc: 
00350     let fnindex_it("Q:\CLmstr\UnPdAloc.h"&str$(cno),"Q:\CLmstr\UAIdx1.h"&str$(cno),"9 12")
00360     let fnindex_it("Q:\CLmstr\UnPdAloc.h"&str$(cno),"Q:\CLmstr\UAIdx2.h"&str$(cno),"1 20")
00370     goto XIT
00380 ! ______________________________________________________________________
00390 ! <Updateable Region: ERTN>
00400 ERTN: let fnerror(cap$,err,line,act$,"xit")
00410     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00420     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00430     print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00440 ERTN_EXEC_ACT: execute act$ : goto ERTN
00450 ! /region
00460 ! ______________________________________________________________________
00470 XIT: fnend 
00480 ! ______________________________________________________________________
