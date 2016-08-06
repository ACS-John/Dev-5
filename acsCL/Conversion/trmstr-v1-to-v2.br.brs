00010 ! Replace R:\acsCL\Conversion\TRmstr-v1-to-v2
00020 ! converts the CL TRmstr file from version 1 to Version 2
00030   def library fntrmstr_v1_to_v2
00040     library 'R:\Core\Library': fntop,fnxit, fncno,fnerror,fnmsgbox,fngethandle,fncopy,fnindex_it,fnstatus
00050     on error goto ERTN
00060 ! ______________________________________________________________________
00070     dim cnam$*40,cap$*128,message$*40,msgline$(6)*48,response$(5)*1
00080 ! ______________________________________________________________________
00090     let fncno(cno,cnam$)
00100     let cap$="Check Book update Trans from v1 to v2"
00110 ! ______________________________________________________________________
00120     let fnstatus(cap$)
00180 ! 
00190     open #trmstr:=fngethandle: "Name=Q:\CLmstr\TRmstr.h"&str$(cno),internal,outin,relative 
00200     if version(trmstr)<>1 and version(trmstr)<>2 then let fnstatus("TRmstr is not version 1.  You must update it to version 1 before running this conversion program") : goto XIT
00210     if version(trmstr)=2 then let fnstatus("TRmstr is already version 2") : goto XIT
00220     let version(trmstr,2)
00230     goto XIT
00240 ! ______________________________________________________________________
00250 ! <Updateable Region: ERTN>
00260 ERTN: let fnerror(cap$,err,line,act$,"NO")
00270     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00280     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00290     print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00300 ERTN_EXEC_ACT: execute act$ : goto ERTN
00310 ! /region
00320 ! ______________________________________________________________________
00330 XIT: ! 
00340     if file$(trmstr)>'' then close #trmstr: 
00350     let fncopy("Q:\CLmstr\TRmstr.h"&str$(cno),"Q:\CLmstr\TRmstr.h"&str$(cno),78)
00380     let fnindex_it("Q:\CLmstr\TrMstr.H"&str$(cno),"Q:\CLmstr\TrIdx1.H"&str$(cno),"1 11")
00382     let fnindex_it("Q:\CLmstr\TrMstr.H"&str$(cno),"Q:\CLmstr\TrIdx2.H"&str$(cno),"28/1 8/11")
00390   fnend 
00400 ! ______________________________________________________________________
