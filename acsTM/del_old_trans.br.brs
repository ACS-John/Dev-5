00010 ! 
00020   dim ta(2),z$*5,e$(3)*30,id$*20,ar(5),tr(6)
00040   open #2: "Name=S:\Core\Data\acsllc\ARTrans.h[cno],Shr",internal,outIn,relative 
00050   open #1: "Name=S:\Core\Data\acsllc\CLmstr.h[cno],KFName=S:\Core\Data\acsllc\CLIndex.h[cno],Shr",internal,outIn,keyed 
00060 L60: read #1,using FORM_CLMSTR: z$,mat e$,bal,mat ta eof END1
00070 FORM_CLMSTR: form pos 1,c 5,pos 6,3*c 30,pos 283,pd 5.2,pos 299,2*pd 3
00080   ta1=ta(1)
00090 L90: if ta1=0 then goto END2
00100   read #2,using L110,rec=ta1: p$,iv$,mat tr,id$,nta
00110 L110: form pos 1,c 5,c 12,n 6,2*pd 5.2,pd 2,2*n 1,c 20,pd 3
00120   tr1$=cnvrt$("n 6",tr(1))
00130   mo1=val(tr1$(1:2))
00140   da1=val(tr1$(3:4))
00150   yr1=val(tr1$(5:6))
00160   if yr1><11 then goto L190
00170   if mo1><4 then goto L190
00180 L180: ta1=nta : goto L90
00190 L190: if tr(5)<4 or tf(5)=5 then bal=bal-tr(3) else bal=bal+tr(3)
00200   rewrite #1,using FORM_CLMSTR,key=z$: z$,mat e$,bal,mat ta
00202   tr(3)=0
00204   rewrite #2,using L110,rec=ta1: p$,iv$,mat tr,id$,nta
00210   goto L180
00220 END2: goto L60
00230 END1: stop 
