00010 ! formerly S:\acsGL\StdAdj
00020 ! Post Standard Adjusting Entries
00030 ! r: setup library, dims, fntop, on error
00040   library 'S:\Core\Library': fntop,fnxit, fnerror,fnprocess,fnchain,fnTos,fnLbl,fnTxt,fnCmdSet,fnAcs,fnmsgbox,fnqgl,fnrgl$,fnagl$
00050   on error goto Ertn
00060 ! ______________________________________________________________________
00070   dim ref$*12,des$*30,glan$(10)*12,glam(10),ml$(3)*80
24000   fntop(program$)
24020   dat=date("mmddyy")
00080 ! /r
24080   fnTos("poststdadj") 
24120   fnLbl(1,1,"Date to be used on Standard Adjusting Entries:",45,1)
24140   fnTxt(1,48,8,0,0,"1",0,"Enter the date to be used on the standard adjusting entries.") 
24160   resp$(1)=str$(dat)
24180   fnLbl(1,60,"",1,1) ! space it over
24200   fnCmdSet(2)
24210   fnAcs(sn$,0,mat resp$,ckey)
24220   if ckey=5 then goto XIT
24240   dat=val(resp$(1))
24260 ! ______________________________________________________________________
36000   open #2: "Name=[Q]\GLmstr\GL_Work_"&env$('acsUserId')&".h[cno],NoShr",internal,output ioerr L240
36020   close #2,free: 
36040   L240: !
36060   open #2: "Name=[Q]\GLmstr\GL_Work_"&env$('acsUserId')&".h[cno],size=0,RecL=104,NoShr",internal,output 
36080   open #3: "Name=[Q]\GLmstr\GLSTDAD.h[cno],KFName=[Q]\GLmstr\GLStdIdx.h[cno],Shr",internal,input,keyed 
36100   net=0
36120   L270: !
36140   read #3,using L340: ref$,des$,glan$,glam eof ChainToAcGlMrge
36160   L340: form pos 1,c 12,c 30,c 12,pd 5.2
36180   if trim$(holdref$)<>"" and holdref$<>ref$ and totaldr+totalcr<>0 then 
36200     gosub MSGBOX1
36220     goto L320 
36240   else 
36260     goto L320
36280   end if
36300 L320: !
36320   if trim$(holdref$)<>"" and holdref$<>ref$ then totaldr=totalcr=0
36340   holdref$=ref$
36360   write #2,using L380: glan$,dat,glam,3,0,ref$,des$,""
36380   L380: form pos 1,c 12,n 6,pd 6.2,n 2,n 2,c 12,c 30,c 8
36400   if glam<0 then totalcr+=glam ! add credits
36420   if glam>0 then totaldr+=glam ! add debits
36440 goto L270
38000 MSGBOX1: ! r: entries don't balance
38020   mat ml$(3) 
38040   ml$(1)="Journal entry # "&trim$(ref$)&" does not foot." 
38060   ml$(2)="Credits  = "&trim$(cnvrt$("pic(---,---,---.##)",totalcr))& "     Debits = "&trim$(cnvrt$("pic(---,---,---.##)",totaldr)) 
38080   ml$(3)="The entry will be posted, but it may need to be corrected!" 
38100   fnmsgbox(mat ml$,resp$)
38120 return ! /r
38140 ! ______________________________________________________________________
40000 ChainToAcGlMrge: !
40020   if totaldr+totalcr<>0 then gosub MSGBOX1
40040   close #2: 
40060   close #3: 
40080 fnchain("S:\acsGL\acglMrge")
42000 XIT: fnxit
44000 ! <Updateable Region: ERTN>
44020 ERTN: fnerror(program$,err,line,act$,"xit")
44040   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
44060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
44080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
44100 ERTN_EXEC_ACT: execute act$ : goto ERTN
44120 ! /region
